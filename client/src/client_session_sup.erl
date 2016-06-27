-module(client_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([start_session/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_session(Args) ->
  {ok, Worker} = supervisor:start_child(?SERVER, [Args]),
  Worker.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  {ok, {{simple_one_for_one, 10, 10},
        [{undefined, {client_session, start_link, []},
          temporary, 5000, worker, [client_session]}]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
