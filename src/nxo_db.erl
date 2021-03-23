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
        ]).


start() ->
  nxo_db_pool:config(),
  ok = application:ensure_started(eqlite),
  ok = pgpool:start(),
  ok = application:ensure_started(nxo_db),
  nxo_db_cache:set(default_pool, nxo_db_pool:default()).

sql_sources(sql) ->
  sql_sources(sql_source, "{sql,eqlite}");
sql_sources(ddl) ->
  sql_sources(ddl_source, "sql").

%%%%%%%%%%%%%%%%%%%%
%% DDL MANAGEMENT %%
%%%%%%%%%%%%%%%%%%%%
apply_full_ddl() ->
  lists:foreach(fun nxo_db_util:evaluate_file/1, sql_sources(ddl)).


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
  Files = filelib:wildcard(Path ++ "/**/*" ++ Ext),
  parse_sourcelist(T, Ext, Files ++ Acc);
parse_sourcelist([{path, Path}|T], Ext, Acc) ->
  Files = filelib:wildcard(Path ++ "/**/*" ++ Ext),
  parse_sourcelist(T, Ext, Files ++ Acc).
