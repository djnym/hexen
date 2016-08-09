-module(hexen_app).

-behaviour(application).

%% API
-export([start/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
%%
start() ->
  [ ensure_started (App)
    || App
    <- [ crypto, asn1, public_key, ssl, hexen ]
  ].

start(_StartType, _StartArgs) ->
  hexen_sup:start_link().

stop(_State) ->
  ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
ensure_started(App) ->
  case application:start(App) of
    ok ->
      App;
    {error, {already_started, App}} ->
      App
  end.
