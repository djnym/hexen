-module(hexen_mirror).

-export([mirror/1]).

mirror(RepoUrl) ->
  Registry = "registry.ets.gz",
  RegistrySigned = "registry.ets.gz.signed",
  HexCSV = "hex-1.x.csv",
  HexCSVSigned = "hex-1.x.csv.signed",

  TarballsDir = "tarballs",
  ensure_dir (TarballsDir),
  InstallsDir = "installs",
  ensure_dir (InstallsDir),

  % figure out current registry or download if one doesn't exist
  CurrentRegistry = fetch_if_new (RepoUrl, undefined, Registry),
  CurrentRegistrySigned = fetch_if_new (RepoUrl, undefined, RegistrySigned),

  % load current on disk registry
  CurrentRegistryTab = registry2ets (CurrentRegistry),
  CurrentTarballList = tarball_list_from_registry (CurrentRegistryTab),
  {CTarballs, CInstalls} =
    lists:partition (fun({_,{"tarballs",_,_}}) -> true;
                        ({_,{"installs",_,_}}) -> false
                     end,
                     CurrentTarballList),
  MissingTarballs = lists:filter (fun({_,{_,_,false}}) -> true;
                                     (_) -> false
                                  end, CTarballs),
  MissingInstalls = lists:filter (fun({_,{_,_,false}}) -> true;
                                     (_) -> false
                                  end, CInstalls),
  TarballsFetched = fetch_files (MissingTarballs, RepoUrl),
  InstallsFetched = fetch_files (MissingInstalls, RepoUrl),
  io:format("MIRROR : There were ~b missing packages downloaded~n",
            [TarballsFetched + InstallsFetched ]),

  switch_if_changed (Registry, CurrentRegistry, undefined),
  switch_if_changed (RegistrySigned, CurrentRegistrySigned, undefined),

  % TODO: don't do this hackiness with directories
  CurrentHexCSVWithDir = fetch_if_new (RepoUrl, "installs", HexCSV),
  CurrentHexCSVSignedWithDir = fetch_if_new (RepoUrl, "installs", HexCSVSigned),
  CurrentHexCSV = strip_installs(CurrentHexCSVWithDir),
  CurrentHexCSVSigned = strip_installs(CurrentHexCSVSignedWithDir),
  switch_if_changed (HexCSV, CurrentHexCSV, "installs"),
  switch_if_changed (HexCSVSigned, CurrentHexCSVSigned, "installs"),
  ok.

registry2ets (File) ->
  EtsFile =
    case lists:reverse (File) of
      [ $z, $g, $. | Rest ] -> lists:reverse (Rest);
      _ -> io:format (standard_error, "ERROR : malformed registry ~p~n",[File]),
           throw({error, malformed_registry})
    end,
  case file:read_file (File) of
    {error, enoent} -> undefined;
    {ok, Binary} ->
      Unzipped = zlib:gunzip (Binary),
      ok = file:write_file (EtsFile, Unzipped),
      case ets:file2tab (EtsFile) of
        {ok, Table} ->
          file:delete (EtsFile),
          Table;
        O ->
          io:format (standard_error, "ERROR : unable to read ets from ~p : ~p~n",
                     [EtsFile, O]),
          file:delete (EtsFile),
          throw({error, bad_ets_table_in_file})
      end;
    RError ->
      io:format (standard_error, "ERROR : unable to read ets from ~p : ~p~n",
                 [File, RError]),
      throw({error, bad_ets_table_file})
  end.

to_file (RootUrl, FilePath) ->
  ok = filelib:ensure_dir (FilePath),
  Url = lists:flatten([RootUrl,FilePath]),
  case request (Url) of
    {ok, _, Data} ->
      case file:write_file (FilePath, Data) of
        ok ->
          io:format ("MIRROR : ~s -> ~s~n",[Url, FilePath]),
          ok;
        IE ->
          io:format (standard_error, "ERROR : Failed to save file ~p for ~p : ~p~n", [Url, FilePath, IE]),
          throw({error, failed_to_save})
      end;
    Error ->
      io:format (standard_error, "ERROR : Failed to make request for ~p : ~p~n", [Url, Error]),
      throw({error, request_failed})
  end.

strip_installs ("installs/" ++ R) -> R.

switch_if_changed (File, NewFile, Dir) ->
  Cwd =
    case Dir =:= undefined of
      true -> undefined;
      false ->
        {ok, C} = file:get_cwd(),
        ok = file:set_cwd(Dir),
        C
    end,
  % determine the current linked file
  CurrentLinkedFile = current (File),
  % if it's the same we're done
  case CurrentLinkedFile =:= NewFile of
    true -> ok;
    false ->
      % otherwise we delete the linked file
      case file:delete (File) of
        ok -> ok;
        {error, enoent} -> ok;
        NE ->
          io:format (standard_error, "ERROR : Failed to delete file ~p : ~p~n", [File, NE]),
          throw({error, delete_failed})
      end,

      % and make a new link
      case file:make_symlink (NewFile, File) of
        ok -> ok;
        MSE ->
          io:format (standard_error, "ERROR : Failed to make symlink ~p -> ~p : ~p~n", [File, NewFile, MSE]),
          throw({error, symlink_failed})
      end,

      case file:delete (CurrentLinkedFile) of
        ok -> ok;
        {error, enoent} -> ok;
        E ->
          io:format (standard_error, "WARNING: Failed to delete old file ~p : ~p~n", [CurrentLinkedFile, E])
      end
  end,
  case Cwd =:= undefined of
    true -> ok;
    false -> ok = file:set_cwd (Cwd)
  end.

to_etag_file (BaseUrl, Path, FileName) ->
  case filelib:ensure_dir (Path) of
    ok -> ok;
    E -> io:format (standard_error, "ERROR : Failed to ensure dir for ~p : ~p~n", [Path, E]),
      throw({error, ensure_dir_failed})
  end,
  Url =
    case Path of
      undefined -> lists:flatten([BaseUrl, FileName]);
      _  -> lists:flatten([BaseUrl, Path, "/", FileName])
    end,
  case request (Url) of
    {ok, Etag, Data} ->
      NewFile =
        case Path of
          undefined -> lists:flatten ([Etag,".",FileName]);
          _ -> filename:join ([Path,lists:flatten ([Etag,".",FileName])])
        end,
      ok = file:write_file (NewFile, Data),
      NewFile;
    Error ->
      io:format (standard_error, "ERROR : Failed to save request for ~p to <ETAG>.~p : ~p~n", [Url, FileName, Error]),
      throw({error, save_failed})
  end.

etag (BaseUrl, Path, FileName) ->
  Url =
    case Path of
      undefined -> lists:flatten([BaseUrl, FileName]);
      _  -> lists:flatten([BaseUrl, Path, "/", FileName])
    end,
  case httpc:request(head, {Url, []},
                     [{relaxed, true}],
                     [{body_format, binary}],
                     hex_mirror) of
    {ok, {{_Version, 200, _Reason}, Headers, _Body}} ->
      parse_etag (proplists:get_value ("etag",Headers));
    Error ->
      io:format (standard_error, "ERROR : Failed head request for ~p : ~p~n",[Url, Error]),
      throw({error, head_request_failed})
  end.

request (Url) ->
  case httpc:request(get, {Url, []},
                     [{relaxed, true}],
                     [{body_format, binary}],
                     hex_mirror) of
    {ok, {{_Version, 200, _Reason}, Headers, Body}} ->
      {ok, parse_etag(proplists:get_value ("etag", Headers)), Body};
    Error ->
      io:format (standard_error, "ERROR : Failed get request for ~p : ~p~n",[Url, Error]),
      throw({error, get_request_failed})
  end.

parse_etag (undefined) ->
  io:format (standard_error,"ERROR : Etag header doesn't exist~n",[]),
  throw({error, missing_etag});
parse_etag ([$" | EtagPrefixRemoved]) ->
  case lists:reverse (EtagPrefixRemoved) of
    [$" | EtagReversed ] ->
      lists:reverse (EtagReversed);
    _ ->
      io:format (standard_error, "ERROR : Malformed Etag suffix ~p~n", [EtagPrefixRemoved]),
      throw({error, malformed_etag})
  end;
parse_etag (MalformedEtag) ->
  io:format (standard_error, "ERROR : Malformed Etag prefix ~w~n", [MalformedEtag]),
  throw({error, malformed_etag_prefix}).

fetch_if_new (Url, Dir, File) ->
  case current (File) of
    undefined -> to_etag_file (Url,Dir,File);
    C ->
      case current_etag (C) =:= etag (Url, Dir, File) of
        true ->
          C;
        false ->
          T = to_etag_file (Url,Dir,File),
          T
      end
  end.

ensure_dir (Dir) ->
  case file:make_dir(Dir) of
    ok -> ok;
    {error, eexist} -> ok;
    {error, MkdirErr} ->
      io:format (standard_error, "ERROR : Unable to create directory ~p : ~p~n", [Dir, MkdirErr]),
      throw({error, cant_create_dir})
  end.

current_etag (undefined) ->
  undefined;
current_etag (File) ->
  case string:tokens (File,".") of
    [Etag|_] -> Etag;
    _ ->
      io:format (standard_error, "ERROR : problem parsing etag from ~p~n",[File]),
      throw({error, etag_parsing_failure})
  end.

current (Filename) ->
  case file:read_link (Filename) of
    {ok, F} -> F;
    {error, enoent} -> undefined;
    E ->
      io:format (standard_error, "ERROR : Problem reading current link ~p~n",[E]),
      throw({error, symlink_reading})
  end.

file_exists (FilePath) ->
  case file:read_file_info (FilePath) of
    {error, _} -> false;
    {ok, _} -> true
  end.

tarball_list_from_registry (undefined) ->
  [];
tarball_list_from_registry (RegistryTab) ->
  ets:foldl(fun ({'$$version$$',_RegistryVersion}, Accum) ->
                  Accum;
                ({_Package, [_Versions]}, Accum) ->
                  Accum;
                ({{Package, Version},[_Deps, CheckSum, _BuildTools]}, Accum) ->
                  TarFile =
                    binary_to_list(
                      <<"tarballs/",Package/binary, "-", Version/binary, ".tar">>
                    ),
                  [{TarFile, {"tarballs",CheckSum,file_exists(TarFile)}} | Accum];
                % installs2 is a mapping of hex versions to elixir versions
                % we need to copy all of them as well
                ({'$$installs2$$', ListOfVersions}, Accum) ->
                  % some key we are supposed to ignore
                  Files = lists:flatten ([
                    [ begin
                       HexFile =
                         binary_to_list(
                          <<"installs/", ElixirVersion/binary, "/hex-", HexVersion/binary,".ez">>
                        ),
                       {HexFile, {"installs",<<>>,file_exists(HexFile)} }
                      end
                      || ElixirVersion
                      <- ElixirVersions
                    ]
                    || {HexVersion, ElixirVersions}
                    <- ListOfVersions
                  ]),
                  Files ++ Accum;
                ({Unrecognized, _}, Accum) ->
                  io:format (standard_error, "WARNING: Registry key ~p is unrecognized~n", [Unrecognized]),
                  Accum
             end,
             [],
             RegistryTab).

fetch_files (FileList, RootUrl) ->
  lists:foldl (fun
                ({HexFile, {"installs",_,_}}, I) ->
                  to_file (RootUrl, HexFile),
                  I + 1;
                ({TarFile, {"tarballs",CheckSum,_}},I) ->
                 FileChecksum = hexen_pkg:checksum (TarFile),
                 case FileChecksum =:= CheckSum of
                   true ->
                     ok;
                   false ->
                     case FileChecksum =/= undefined of
                       true ->
                         io:format (standard_error, "WARNING : Checksum Error~n", []),
                         io:format (standard_error, "WARNING : FileName           : ~s~n",[TarFile]),
                         io:format (standard_error, "WARNING : RegistryChecksum   : ~s~n",[CheckSum]),
                         io:format (standard_error, "WARNING : FileChecksum       : ~s~n",[FileChecksum]),
                         io:format (standard_error, "WARNING : CalculatedChecksum : ~s~n",[hexen_pkg:calc_checksum (TarFile)]);
                       false ->
                         ok
                     end,
                     to_file (RootUrl, TarFile)
                 end,
                 I + 1
               end,
               0,
               FileList).
