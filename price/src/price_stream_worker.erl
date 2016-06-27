%% -*- erlang-indent-level: 2 -*-
-module(price_stream_worker).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_link/1]).

-export([info/1, send/2, send_price/3]).

-record(state, {socket,
                listen_socket}).

-record(price_message, {event_id,
                        quotes}).

%% - Init ----------------------------------------------------------------------

init(Socket) ->
  erlang:send(self(), connect),
  {ok, #state{socket=undefined, listen_socket=Socket}}.

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

%% - API -----------------------------------------------------------------------

info(Pid) ->
  gen_server:call(Pid, info).

send(Pid, Data) ->
  gen_server:call(Pid, {send, Data}).

send_price(Pid, EventId, Quotes) ->
  gen_server:cast(Pid, {send_price, EventId, Quotes}).

%% - Handle cast ---------------------------------------------------------------

handle_cast({send_price, _, _}, #state{socket=undefined} = State) ->
  {noreply, State};
handle_cast({send_price, EventId, Quotes},
            #state{socket=Sock} = State) ->
  Data = generate_price_message(EventId, Quotes),
  case gen_tcp:send(Sock, Data) of
    ok -> {noreply, State};
    {error, Reason} -> {stop, Reason, State}
  end;
handle_cast(_Message, State) ->
  {noreply, State}.

%% - Handle call ---------------------------------------------------------------

handle_call({send, _Data}, _From, #state{socket=undefined} = State) ->
  {reply, no_connection, State};
handle_call({send, Data}, _From, #state{socket=Sock} = State) ->
  ok = gen_tcp:send(Sock, Data),
  {reply, State, State};
handle_call(info, _From, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  {reply, badarg, State}.

% - Handle info ---------------------------------------------------------------

handle_info(connect, #state{listen_socket = ListenSocket} = State) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  ok = gen_tcp:send(Socket, <<"connected">>),
  ok = price_stream:register(?MODULE),
  {noreply, State#state{socket = Socket}};
handle_info({tcp_closed, Socket}, State) ->
  ok = price_stream:unregister(?MODULE),
  ok = gen_tcp:close(Socket),
  erlang:send(self(), connect),
  {noreply, State#state{socket = undefined}};
handle_info({tcp_error, _Socket, Reason}, State) ->
  {stop, Reason, State};
handle_info(_Msg, State) ->
  {noreply, State}.

% - Utils ---------------------------------------------------------------------

terminate(_Reason, #state{socket = Sock}) ->
  ok = price_stream:unregister(?MODULE),
  ok = gen_tcp:close(Sock),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% - Utils ---------------------------------------------------------------------

generate_price_message(EventId, DisplayQuotes) ->
  Price = #price_message{
    event_id = EventId,
    quotes = DisplayQuotes
  },
  BinPrice = erlang:term_to_binary(Price),
  Delimiter = erlang:term_to_binary('#'),
  <<BinPrice/binary, Delimiter/binary>>.
