%% Module must match the file name
-module(example).

%% Exported functions, several lists allowed.
-export([something/1, something/2, something2/2]).

%% Constant definition
-define(SECOND, 1000).

%% Record/Structs definition
-record(struct_example,
        {arg = undefined, list = [a, b, c]}).

%% Function declaration
something(Arg) ->
  Arg.

%% A function is a tuple name/#args so something/1 and
%% something/2 are 2 different functions.
something(Arg, Bool) ->
  case Bool of
    true ->
      Arg;
    _ ->
      nope
  end.

something2(Arg, true) ->
  Arg;
something2(_Arg, _) ->
  nope.
