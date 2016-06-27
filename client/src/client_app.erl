-module(client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
  {ok, _} = client_sup:start_link([]).

stop(_) ->
  ok.


