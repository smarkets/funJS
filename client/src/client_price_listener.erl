%% -*- erlang-indent-level: 2 -*-
-module(client_price_listener).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_link/0]).

-export([info/0]).

-record(state, {socket,
                buffer = <<>>
               }).

%% - Init ----------------------------------------------------------------------

init(_Args) ->
  {ok, Host} = application:get_env(client, stream_host),
  {ok, Port} = application:get_env(client, stream_port),
  connect(Host, Port),
  {ok, #state{}}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% - API -----------------------------------------------------------------------

connect(Host, Port) ->
  gen_server:cast(?MODULE, {connect, Host, Port}).

info() ->
  gen_server:call(?MODULE, info).

%% - Handle cast ---------------------------------------------------------------

handle_cast({connect, Host, Port}, #state{socket=undefined} = State) ->
  case gen_tcp:connect(Host, Port, [{reuseaddr, true}, {active, true}, binary]) of
    {ok, Socket} ->
      {noreply, State#state{socket=Socket}};
    _Error ->
      {stop, normal, State}
  end;
handle_cast(_Message, State) ->
  {noreply, State}.

%% - Handle call ---------------------------------------------------------------

handle_call(info, _From, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  {reply, badarg, State}.

% - Handle info ---------------------------------------------------------------

handle_info({tcp, _Socket, <<"connected">>}, State) ->
  {noreply, State};
handle_info({tcp, Socket, Msg},
            #state{socket=Socket, buffer=Buffer} = State) ->
  FullBuffer = <<Buffer/binary, Msg/binary>>,
  {Messages, RestBuffer} = decode(FullBuffer),
  lists:foreach(fun client_session_engine:handle_price_message/1, Messages),
  {noreply, State#state{buffer = RestBuffer}};
handle_info({tcp_closed, _Socket}, State) ->
  lager:info("Connection closed ~n", []),
  {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
  lager:error("Connection crashed ~p~n", [Reason]),
  {stop, normal, State};
handle_info(_Msg, State) ->
  {noreply, State}.

decode(Buffer) ->
  Delimiter = erlang:term_to_binary('#'),
  Bins = binary:split(Buffer, Delimiter, [global]),
  BinaryRemainder = lists:last(Bins),
  FullMessages = lists:droplast(Bins),
  {lists:map(fun erlang:binary_to_term/1, FullMessages), BinaryRemainder}.

% - gen_tcp -------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% - Utils ---------------------------------------------------------------------


