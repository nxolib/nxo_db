-module(nxo_db).

-export([
          start/0
        , sql_sources/1
        , apply_full_ddl/0
        ]).

-export([
          default_pool/0
        , pool/1
        , pool/0
        , retries/0
        , retries/1
        , retry_sleep/0
        , retry_sleep/1
        ]).

-export([
          query_list/0
        , query_info/1
        , query_sql/1
        , query_refresh/0
        ]).

%%%%%%%%%%%%%%%%%%
%% HOUSEKEEPING %%
%%%%%%%%%%%%%%%%%%
start() ->
  nxo_db_pool:config(),
  ok = pgpool:start(),
  ok = application:ensure_started(nxo_db),
  nxo_db_cache:set(default_pool, nxo_db_pool:default()),
  nxo_db_cache:set(retries, nxo_db_pool:retries()),
  nxo_db_cache:set(retry_sleep, nxo_db_pool:retry_sleep()),
  nxo_db_eqlite:refresh(sql_sources(sql)).

sql_sources(sql) ->
  sql_sources(sql_source, "{sql,eqlite}");
sql_sources(ddl) ->
  sql_sources(ddl_source, "sql").

%%%%%%%%%%%%%
%% QUERIES %%
%%%%%%%%%%%%%

% Types could be raw, scalar, list, map, ... ?

q(Type, Query) ->
  q(Type, Query, []).

q(Type, Query, Params) ->
  not_implemented.


%%%%%%%%%%%%%%%%%%%%
%% DDL MANAGEMENT %%
%%%%%%%%%%%%%%%%%%%%
apply_full_ddl() ->
  lists:foreach(fun nxo_db_util:evaluate_file/1, sql_sources(ddl)).

%%%%%%%%%%%%%%%%%%%%
%% SQL MANAGEMENT %%
%%%%%%%%%%%%%%%%%%%%
query_list() ->
  nxo_db_eqlite:list_queries().

query_info(Q) ->
  nxo_db_eqlite:get_info(Q).

query_sql(Q) ->
  nxo_db_eqlite:get_query(Q).

query_refresh() ->
  nxo_db_eqlite:refresh(sql_sources(sql)).


%%%%%%%%%%%%%%%%%%%%%
%% POOL MANAGEMENT %%
%%%%%%%%%%%%%%%%%%%%%

%% Retrieve the default (i.e., first listed) pool.
default_pool() ->
  nxo_db_cache:lookup(default_pool).

%% Set the current DB pool.
pool(default) ->
  nxo_db_cache:set(pool, default_pool());
pool(Pool) ->
  nxo_db_cache:set(pool, Pool).

%% Retrieve the current DB pool.
pool() ->
  nxo_db_cache:lookup(pool, default_pool()).


%% How many equery attempts to make.
retries() ->
  nxo_db_cache:lookup(retries).

retries(default) ->
  nxo_db_cache:set(retries, nxo_db_pool:retries());
retries(Count) ->
  nxo_db_cache:set(retries, Count).

%% How long to stall between retries.
retry_sleep() ->
  nxo_db_cache:lookup(retry_sleep).

retry_sleep(default) ->
  nxo_db_cache:set(retry_sleep, nxo_db_pool:retry_sleep());
retry_sleep(MS) ->
  nxo_db_cache:set(retry_sleep, MS).



%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

sql_sources(ConfigKey, Extensions) ->
  case application:get_env(nxo_db, ConfigKey, undefined) of
    SourceList when is_list(SourceList) ->
      parse_sourcelist(SourceList, Extensions, []);
    _ -> []
  end.

parse_sourcelist([], _Ext, Acc) ->
  Acc;
parse_sourcelist([{priv_dir, App, SubDir}|T], Ext, Acc) ->
  Path = filename:join(code:priv_dir(App), SubDir),
  Files = filelib:wildcard(Path ++ "/**/[a-zA-Z0-9]*" ++ Ext),
  parse_sourcelist(T, Ext, Files ++ Acc);
parse_sourcelist([{path, Path}|T], Ext, Acc) ->
  Files = filelib:wildcard(Path ++ "/**/[a-zA-Z0-9]*" ++ Ext),
  parse_sourcelist(T, Ext, Files ++ Acc).
