%%
%% I wanted a quick way to serve a hex mirror, so thought hey I'll try
%% out the built in web server.  That was a mistake, but I got through
%% it.  I grabbed most of the contents of mod_get.erl and mod_head.erl
%% which have this license

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2012. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(hexen_mod).

-export([do/1]).

-define(FILE_CHUNK_SIZE,64*1024).
-include_lib("inets/include/httpd.hrl").

log(#mod { request_line  = RequestLine,
           init_data     = #init_data{peername = {_, RemoteHost}},
           data          = Data,
           parsed_header = Headers
         },
    StatusCode,
    Size) ->
  AuthUser  = auth_user(Data),
  Date      = httpd_util:custom_date(),
  Referer   = proplists:get_value("referer", Headers, "-"),
  UserAgent = proplists:get_value("user-agent", Headers, "-"),
  io:format ("REQUEST : ~s ~s ~s [~s] \"~s\" ~w ~w \"~s\" \"~s\"~n",
             [RemoteHost, "-", AuthUser, Date, RequestLine,
              StatusCode, Size, Referer, UserAgent]).

auth_user(Data) ->
  case proplists:get_value(remote_user, Data) of
    undefined ->
      "-";
    RemoteUser ->
      RemoteUser
  end.

%% do
do(Info = #mod { method = Method }) ->
  case Method of
    "GET" -> check_status_and_run (Info, fun do_get/1);
    "HEAD" -> check_status_and_run (Info, fun do_head/1);
    "POST" ->
      Body = list_to_binary (Info#mod.entity_body),
      io:format("got ~p~n",[Info#mod.request_uri]),
      io:format("got ~p~n",[Info#mod.parsed_header]),
%      Term = binary_to_term (Body),
%      io:format("got term ~p~n",[Term]),
      file:write_file("tmp.tar",Body),
      {break,[{response, {201,""}}]};
    %% Not a GET method!
    _ ->
      {proceed,Info#mod.data}
  end.

check_status_and_run (Info, Fun) ->
  % this chunk seems to get used in a lot of the built in mod's, so putting
  % in a separate function as I'll reuse it
  case proplists:get_value(status, Info#mod.data) of
    % something else handled this request, so just proceed
    {_StatusCode, _PhraseArgs, _Reason} -> {proceed, Info#mod.data};
    % nothing has handled this yet, so attempt to
    undefined ->
      case proplists:get_value(response, Info#mod.data) of
        % nothing has handled this request yet, so handle it
        undefined -> Fun(Info);
        % something already generated some sort of response so just return it
        _Response -> {proceed, Info#mod.data}
      end
  end.

do_get(Info) ->
  Path =
    mod_alias:path(Info#mod.data, Info#mod.config_db, Info#mod.request_uri),
  send_response(Info#mod.socket,Info#mod.socket_type, Path, Info).

do_head(Info) ->
  Path =
    mod_alias:path(Info#mod.data, Info#mod.config_db, Info#mod.request_uri),
  Suffix = httpd_util:suffix(Path),
  %% Does the file exists?
  case file:read_file_info(Path) of
    {ok, FileInfo} ->
      MimeType =
        httpd_util:lookup_mime_default(Info#mod.config_db,Suffix,"text/plain"),
      Length = io_lib:write(FileInfo#file_info.size),
      Head =
        [ {content_type, MimeType},
          {etag, etag(Path)},
          {content_length, Length},
          {code, 200}
        ],
      log (Info, 200, FileInfo#file_info.size),
      {proceed,[{response, {response, Head,  nobody}} | Info#mod.data]};
    {error, Reason} ->
      Status = httpd_file:handle_error(Reason, "access", Info, Path),
      log (Info, Status, 0),
      {proceed,[{status, Status} | Info#mod.data]}
  end.

% snagged from rebar3
% https://github.com/erlang/rebar3/blob/feed75ca91423be8eaf49e1db57a5ef605238aed/src/rebar_pkg_resource.erl#L129
etag(Path) ->
  case file:read_file(Path) of
    {ok, Binary} ->
      <<X:128/big-unsigned-integer>> = crypto:hash(md5, Binary),
      string:to_lower(lists:flatten(io_lib:format("\"~32.16.0b\"", [X])));
    {error, _} ->
      false
  end.

%% The common case when no range is specified
send_response(_Socket, _SocketType, Path, Info)->
  %% Send the file!
  %% Find the modification date of the file
  case file:open(Path,[raw,binary]) of
    {ok, FileDescriptor} ->
      {FileInfo, LastModified} = get_modification_date(Path),
      Suffix = httpd_util:suffix(Path),
      MimeType = httpd_util:lookup_mime_default(Info#mod.config_db,
                                                Suffix,"text/plain"),
      %% FileInfo = file:read_file_info(Path),
      Size = integer_to_list(FileInfo#file_info.size),
      Headers = case Info#mod.http_version of
        "HTTP/1.1" ->
          [{content_type, MimeType},
           {etag, etag(Path)},
           {content_length, Size}|LastModified];
        %% OTP-4935
        _ ->
          %% i.e http/1.0 and http/0.9
          [{content_type, MimeType},
           {content_length, Size}|LastModified]
      end,
      log (Info, 200, FileInfo#file_info.size),
      send(Info, 200, Headers, FileDescriptor),
      file:close(FileDescriptor),
      {proceed,[{response,{already_sent,200,
                           FileInfo#file_info.size}},
                {mime_type,MimeType} | Info#mod.data]};
    {error, Reason} ->
      Status = httpd_file:handle_error(Reason, "open", Info, Path),
      log (Info, Status, 0),
      {proceed, [{status, Status} | Info#mod.data]}
  end.

%% send
send(#mod{socket = Socket, socket_type = SocketType} = Info,
     StatusCode, Headers, FileDescriptor) ->
  httpd_response:send_header(Info, StatusCode, Headers),
  send_body(SocketType,Socket,FileDescriptor).

send_body(SocketType,Socket,FileDescriptor) ->
  case file:read(FileDescriptor,?FILE_CHUNK_SIZE) of
    {ok,Binary} ->
      case httpd_socket:deliver(SocketType,Socket,Binary) of
        socket_closed ->
          socket_close;
        _ ->
          send_body(SocketType,Socket,FileDescriptor)
      end;
    eof ->
      eof
  end.

get_modification_date(Path)->
  {ok, FileInfo0} = file:read_file_info(Path), 
  LastModified =
    case catch httpd_util:rfc1123_date(FileInfo0#file_info.mtime) of
      Date when is_list(Date) -> [{last_modified, Date}];
      _ -> []
    end,
  {FileInfo0, LastModified}.
