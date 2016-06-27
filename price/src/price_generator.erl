%% -*- erlang-indent-level: 2 -*-
-module(price_generator).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_link/0]).

-export([info/0]).

-record(state, {events = 0,
                rate = 0
               }).

%% - Init ----------------------------------------------------------------------

init(_InitArgs) ->
  timer:send_interval(1000, tick),
  {ok, Args} = application:get_env(price, generator),
  {ok, #state{events = proplists:get_value(events, Args),
              rate = proplists:get_value(rate, Args)}}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% - API -----------------------------------------------------------------------

info() ->
  gen_server:call(?MODULE, info).

%% - Handle cast ---------------------------------------------------------------

handle_cast(_Message, State) ->
  {noreply, State}.

%% - Handle call ---------------------------------------------------------------

handle_call(info, _From, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  {reply, badarg, State}.

% - Handle info ---------------------------------------------------------------

handle_info(tick, #state{events = Events, rate = Rate} = State) ->
  send_some_prices(Events, Rate),
  {noreply, State};
handle_info(stop, State) ->
  {stop, normal, State};
handle_info(_Msg, State) ->
  {noreply, State}.

% - Terminate -----------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

% - Code change ---------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% - Utils ---------------------------------------------------------------------

send_some_prices(_, 0) ->
  ok;
send_some_prices(Events, Rate) ->
  Event = pick_event(Events),
  price_stream:send_prices(Event, "New price"),
  send_some_prices(Events, Rate - 1).

pick_event(NumEvents) ->
  random:uniform(NumEvents).
