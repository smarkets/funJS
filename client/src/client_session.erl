%% -*- erlang-indent-level: 2 -*-
-module(client_session).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, start_link/1]).

-export([notify/2, stop/1]).

-behaviour(gen_server).

-record(state, {email, events}).

%% - API -----------------------------------------------------------------------

init([Args]) ->
  Email = proplists:get_value(email, Args),
  Events = proplists:get_value(events, Args),
  client_session_engine:subscribe(Events),
  {ok, #state{email = Email, events = Events}}.

notify(Pid, Msg) ->
  gen_server:cast(Pid, Msg).

stop(Pid) ->
  gen_server:cast(Pid, stop).

start_link(Args) ->
  gen_server:start_link(?MODULE, [Args], []).

%% - Handle cast ---------------------------------------------------------------

handle_cast({price, EventId, Quote}, #state{email = Email} = State) ->
  lager:info("User with email ~p received ~p", [Email, {EventId, Quote}]),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Message, State) ->
  {noreply, State}.

%% - Handle call ---------------------------------------------------------------

handle_call(info, _From, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  {reply, badarg, State}.

%% - Handle info ---------------------------------------------------------------

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
