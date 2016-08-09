-module(hexen_pkg).

-export([ checksum/1, % File or #pkg
          from_tar/1,
          calc_checksum/1,
          validate/1]).

-record (pkg, { version,
                meta,
                contents,
                checksum }).

from_tar (TarFile) ->
  % it appears that at some point in the past there was a package published
  % with it's metadata in elixir format, so I also check for "metadata.exs"
  % below, and use that if I don't find "metadata.config"
  case erl_tar:extract (
         TarFile,
         [ {files,["VERSION",
                   "CHECKSUM",
                   "metadata.config",
                   "metadata.exs",
                   "contents.tar.gz"]},
           memory
         ]
      ) of
    {ok, FileList} ->
      #pkg { version = proplists:get_value ("VERSION",FileList),
             meta = case proplists:get_value ("metadata.config",FileList) of
                      undefined ->
                        proplists:get_value ("metadata.exs",FileList);
                      M -> M
                    end,
             contents = proplists:get_value ("contents.tar.gz",FileList),
             checksum = proplists:get_value ("CHECKSUM", FileList)
           };
    Error ->
      Error
  end.

validate (TarFile) ->
  Pkg = from_tar (TarFile),
  checksum(Pkg) =:= calc_checksum(Pkg).

checksum (#pkg { checksum = Checksum }) ->
  Checksum;
checksum (TarFile) ->
  case erl_tar:extract ( TarFile, [ {files,["CHECKSUM"]}, memory ]) of
    {ok, FileList} ->
      proplists:get_value ("CHECKSUM", FileList);
    {error, {_,enoent}} ->
      undefined;
    Error ->
      Error
  end.

calc_checksum (#pkg { version = VBytes,
                      meta = MetaBytes,
                      contents = ContentsBytes }) ->
  calc_checksum (VBytes, MetaBytes, ContentsBytes);
calc_checksum (TarFile) ->
  calc_checksum (from_tar (TarFile)).

calc_checksum (VBytes, MetaBytes, ContentsBytes) ->
  % snagged this bit from rebar3_hex here
  % https://github.com/hexpm/rebar3_hex/blob/e9ce30f9f8fac946ada233fbc2ee0941dbabd16e/src/rebar3_hex_tar.erl
  Blob = <<VBytes/binary, MetaBytes/binary, ContentsBytes/binary>>,
  <<X:256/big-unsigned-integer>> = crypto:hash(sha256,Blob),
  list_to_binary(
    string:to_upper(
      lists:flatten(
        io_lib:format("~64.16.0b", [X])
      )
    )
  ).


