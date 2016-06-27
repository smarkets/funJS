%% -*- erlang-indent-level: 2 -*-
-module(client_sup).

-behaviour(supervisor).

-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

init(_Args) ->
  ClientSup =
    {client_session_sup,
     {client_session_sup, start_link, []},
     permanent,
     infinity,
     supervisor,
     [client_session_sup]},
  Engine =
    {client_session_engine,
     {client_session_engine, start_link, []},
     permanent,
     infinity,
     worker,
     [client_session_engine]},
  Listener =
    {client_price_listener,
     {client_price_listener, start_link, []},
     permanent,
     infinity,
     worker,
     [client_price_listener]},
  Children = [ClientSup, Engine, Listener],
  {ok, {{one_for_one, 20, 1}, Children}}.
