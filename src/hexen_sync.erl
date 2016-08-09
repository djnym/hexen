-module(hexen_sync).

-behaviour (gen_server).

%% API
-export ([ start_link/1 ]).

%% gen_server callbacks
-export ([init/1,
          handle_call/3,
          handle_cast/2,
          handle_info/2,
          terminate/2,
          code_change/3]).

-record (state, { mirror_url,
                  delay
                }).

start_link (Url) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Url], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Url]) ->
  % make sure terminate is called
  process_flag (trap_exit, true),
  Delay = 60000, % wakeup once a minute

  { ok,
    #state { mirror_url = Url,
             delay = Delay
           },
    0
  }.

handle_call (_Request, _From, State = #state { delay = Delay }) ->
  {reply, ok, State, Delay}.

handle_cast (_Request, State = #state { delay = Delay }) ->
  {noreply, State, Delay}.

handle_info (timeout, State = #state { mirror_url = Url, delay = Delay }) ->
  % just catch all exceptions and ignore for now
  try hexen_mirror:mirror (Url) of
    ok -> ok
  catch
    _:_ -> ok
  end,
  {noreply, State, Delay};
handle_info (_Info, State = #state { delay = Delay }) ->
  {noreply, State, Delay}.

terminate (_Reason, _State) ->
    ok.

code_change (_OldVsn, State, _Extra) ->
    {ok, State}.
