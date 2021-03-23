-module(nxo_db_cache).
-behaviour(gen_server).

%% API
-export([
          start_link/0
        , lookup/1
        , lookup/2
        , set/2
        , dump/0
        , flush/0
        , stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
lookup(Key) ->
  gen_server:call(?SERVER, {lookup, Key, undefined}).

lookup(Key, Default) ->
  gen_server:call(?SERVER, {lookup, Key, Default}).

set(Key, Value) ->
  gen_server:call(?SERVER, {set, Key, Value}).

dump() ->
  gen_server:call(?SERVER, dump).

flush() ->
  gen_server:cast(?SERVER, flush).

stop() ->
  gen_server:call(?SERVER, stop).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
  process_flag(trap_exit, true),
  {ok, #{}}.



handle_call({lookup, Key, Default}, _From, State) ->
  {reply, maps:get(Key, State, Default), State};

handle_call({set, Key, Value}, _From, State) ->
  {reply, Value, maps:put(Key, Value, State)};

handle_call(dump, _From, State) ->
  {reply, State, State};

handle_call(stop, _From, State) ->
  {stop, normal, shutdown_ok, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.



handle_cast(flush, _State) ->
  {noreply, #{}};

handle_cast(_Request, State) ->
  {noreply, State}.



handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, Status) ->
  Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
