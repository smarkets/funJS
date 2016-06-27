-module(client).

-export([start_shell/0, start_session/2]).

start_shell() ->
  ok = application:start(compiler),
  ok = application:start(syntax_tools),
  ok = application:start(goldrush),
  ok = application:start(lager),
  ok = application:start(client).

start_session(Email, Events) ->
  client_session_sup:start_session([{email, Email}, {events, Events}]).
