%% -*- erlang-indent-level: 2 -*-
-module(client_session_engine).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_link/0]).

-export([info/0, handle_price_message/1, subscribe/1]).

-record(state, {clients = []}).

-record(price_message, {event_id,
                        quotes}).

%% - Init ----------------------------------------------------------------------

init(_Args) ->
  {ok, #state{}}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% - API -----------------------------------------------------------------------

handle_price_message(Message) ->
  gen_server:cast(?MODULE, Message).

subscribe(Events) ->
  gen_server:call(?MODULE, {subscribe, Events}).

info() ->
  gen_server:call(?MODULE, info).

%% - Handle cast ---------------------------------------------------------------

handle_cast(#price_message{event_id = EventId, quotes = Quotes},
            #state{clients = Clients} = State) ->
  % Note: this is pretty inefficient.
  lists:foreach(fun(Client) -> maybe_send(Client, EventId, Quotes) end, Clients),
  {noreply, State};
handle_cast(_Message, State) ->
  {noreply, State}.

maybe_send({Pid, Events}, EventId, Quotes) ->
  case lists:member(EventId, Events) of
    true ->
      client_session:notify(Pid, {price, EventId, Quotes});
    false ->
      ok
  end.

%% - Handle call ---------------------------------------------------------------

handle_call({subscribe, Events},
            {Pid, _},
            #state{clients = Clients} = State) ->
  NewClients =
    case lists:keymember(Pid, 1, Clients) of
      true ->
        lists:keyupdate(Pid, 1, Clients, {Pid, Events});
      false ->
        erlang:monitor(process, Pid),
        [{Pid, Events} | Clients]
    end,
  {reply, ok, State#state{clients = NewClients}};
handle_call(_Request, _From, State) ->
  {reply, badarg, State}.

% - Handle info ---------------------------------------------------------------

handle_info({'DOWN', _, process, Pid, _Reason},
            #state{clients = Clients} = State) ->
  NewClients = lists:keydelete(Pid, 1, Clients),
  {noreply, State#state{clients = NewClients}};
handle_info(_Msg, State) ->
  {noreply, State}.

% - gen_tcp -------------------------------------------------------------------

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% - Utils ---------------------------------------------------------------------


