%%%-------------------------------------------------------------------
%% @doc nxo_db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nxo_db_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_one,
               intensity => 0,
               period => 1},

  Cache = #{ id => nxo_db_cache,
             start => {nxo_db_cache, start_link, []} },

  Eqlite = #{ id => nxo_db_eqlite,
              start => {nxo_db_eqlite, start_link, []} },

  ChildSpecs = [Cache, Eqlite],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
