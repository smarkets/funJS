%% -*- erlang-indent-level: 2 -*-
-module(price_stream).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_link/0]).

-export([info/0]).

-export([num_workers/1, register/1, unregister/1]).

-export([send_prices/2]).

-record(state, {workers}).

%% - Init ----------------------------------------------------------------------

init(_Args) ->
  {ok, #state{workers=orddict:new()}}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% - API -----------------------------------------------------------------------

info() ->
  gen_server:call(?MODULE, info).

send_prices(EventId, Quotes) ->
  gen_server:cast(?MODULE, {send_prices, EventId, Quotes}).

num_workers(Worker) ->
  gen_server:call(?MODULE, {num_workers, Worker}).

register(Worker) ->
  gen_server:call(?MODULE, {register, Worker}).

unregister(Worker) ->
  gen_server:call(?MODULE, {unregister, Worker}).

%% - Handle cast ---------------------------------------------------------------

handle_cast({send_prices, EventId, Quotes},
            #state{workers=Workers} = State) ->
  ok = send_event_worker(price_stream_worker, Workers, send_price,
                         [EventId, Quotes]),
  {noreply, State};
handle_cast(_Message, State) ->
  {noreply, State}.

%% - Handle call ---------------------------------------------------------------

handle_call(info, _From, State) ->
  {reply, State, State};
handle_call({num_workers, Worker},
            _From,
            #state{workers=Workers} = State) ->
  case orddict:find(Worker, Workers) of
    {ok, Fetched} -> {reply, length(Fetched), State};
    _ -> {reply, 0, State}
  end;
handle_call({register, Worker}, {Pid, _Tag}, #state{workers=Workers}) ->
  NewWorkers = orddict:append(Worker, Pid, Workers),
  {reply, ok, #state{workers=NewWorkers}};
handle_call({unregister, Worker}, {Pid, _Tag}, #state{workers=Workers}) ->
  NewWorkers = orddict:update(Worker,
                  fun(Value) ->
                      lists:delete(Pid, Value)
                  end, Workers),
  {reply, ok, #state{workers=NewWorkers}};
handle_call(_Request, _From, State) ->
  {reply, badarg, State}.

% - Handle info ---------------------------------------------------------------

handle_info(_Msg, State) ->
  {noreply, State}.

% - Utils ---------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% - Utils ---------------------------------------------------------------------

send_event_worker(WorkerType, Workers, Method, ArgumentList) ->
  case orddict:find(WorkerType, Workers) of
    {ok, PriceWorkers} ->
      lists:foreach(
        fun(Pid) ->
          apply(WorkerType, Method, [Pid | ArgumentList])
        end,
        PriceWorkers
      ),
      ok;
    _Error ->
      ok
  end.
