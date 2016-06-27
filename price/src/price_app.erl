-module(price_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
  price_sup:start_link([]).

stop(_) ->
  ok.
