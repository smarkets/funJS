%% -*- erlang-indent-level: 2 -*-
-module(price_stream_spawner).
-compile([{parse_transform, lager_transform}]).

-behaviour(supervisor).

%% supervisor callbacks
-export([init/1, start_link/1]).

%% - Init ----------------------------------------------------------------------

init(Worker) ->
  ConfigKey = Worker,
  Args = application:get_env(price, ConfigKey, []),
  Port = proplists:get_value(port, Args),
  Options = proplists:get_value(options, Args),
  MaxChilds = proplists:get_value(max_workers, Args),
  {ok, ListenSocket} = gen_tcp:listen(Port, Options),
  Children = [
    {base64:encode(crypto:strong_rand_bytes(8)),
     {Worker, start_link, [ListenSocket]},
     permanent,
     infinity,
     worker,
     [Worker]
    } || _X <- lists:seq(1, MaxChilds)
  ],
  {ok, {{one_for_one, 20, 1}, Children}}.

start_link(Args) ->
  %% Because it can be usefull to have this process registered, this "hack"
  %% is present. If you know a better way let me know (learning is always fun)
  RegisteredName = list_to_atom(atom_to_list(?MODULE) ++
                                "_" ++ atom_to_list(Args)),
  supervisor:start_link({local, RegisteredName}, ?MODULE, Args).

