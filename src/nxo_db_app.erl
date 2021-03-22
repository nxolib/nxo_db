%%%-------------------------------------------------------------------
%% @doc nxo_db public API
%% @end
%%%-------------------------------------------------------------------

-module(nxo_db_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    nxo_db_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
