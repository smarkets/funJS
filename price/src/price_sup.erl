%% -*- erlang-indent-level: 2 -*-
-module(price_sup).

-behaviour(supervisor).

-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).


start_link(Args) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Args]).

init(_Args) ->
  Generator =
    {price_generator,
     {price_generator, start_link, []},
     permanent,
     infinity,
     worker,
     [price_generator]},
  StreamSupervisor =
    {price_stream_sup,
     {price_stream_sup, start_link, [[]]},
     permanent,
     infinity,
     supervisor,
     [price_stream_sup]},

  Children = [Generator, StreamSupervisor],
  {ok, {{one_for_one, 20, 1}, Children}}.
