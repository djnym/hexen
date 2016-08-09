-module(hexen_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  inets:start(),
%  inets:start(httpc, [{profile, hex_mirror}]),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, Cwd} = file:get_cwd(),
  {ok, Url} = application:get_env (hexen, mirror_url),
  {ok, {
         {one_for_one, 5, 10},
         [
           { httpc,
             {inets, start, [httpc, [{profile, hex_mirror}]]},
             permanent,
             5000,
             worker,
             []
           },
           { hexen_sync,
             { hexen_sync, start_link, [Url] },
             permanent,
             5000,
             worker,
             []
           },
           { hexen_httpd,
             { inets, start, [httpd, [{port,31337},
                                      {server_name, "pm"},
                                      {server_root,Cwd},
                                      {document_root,Cwd},
                                      {bind_address, "localhost"},
                                      {modules, [mod_alias,
                                                 mod_auth,
                                                 mod_actions,
                                                 mod_dir,
                                                 hexen_mod,
                                                 mod_log,
                                                 mod_disk_log]}
                                     ]
                             ] },
             permanent,
             5000,
             worker,
             []
           }
         ]
       }
  }.

