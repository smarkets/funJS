%% -*- erlang-indent-level: 2 -*-
-module(price_stream_sup).

-behaviour(supervisor).

-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

init(_Args) ->
  StreamPriceSpawner =
    {price_stream_spawner,
     {price_stream_spawner, start_link, [price_stream_worker]},
     permanent,
     infinity,
     supervisor,
     [price_stream_spawner]},
  Stream =
    {price_stream,
     {price_stream, start_link, []},
     permanent,
     infinity,
     worker,
     [price_stream]},
  Children = [StreamPriceSpawner, Stream],
  {ok, {{one_for_one, 20, 1}, Children}}.
