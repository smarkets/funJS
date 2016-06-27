%% -*- erlang-indent-level: 2 -*-
-module(price).

-compile([{parse_transform, lager_transform}]).

-export([start/0, start_shell/0]).

%% - Application start ---------------------------------------------------------

start() ->
  ok = application:start(price).

start_shell() ->
  ok = application:start(compiler),
  ok = application:start(syntax_tools),
  ok = application:start(goldrush),
  ok = application:start(lager),
  ok = application:start(price).
