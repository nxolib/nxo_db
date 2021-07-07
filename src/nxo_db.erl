-module(nxo_db).

-export([
          start/0
        , sql_sources/1
        , apply_full_ddl/0
        , q/1
        , q/2
        , q/3
        , q/4
        , batch/1
        , batch/2
        ]).

-export([ %% provided for backwards compatibility
          scalar_query/2
        , list_query/2
        , map_query/2
        , query/2
        , check_dup/3
        , check_dup/5
        , cascading_update/1
        ]).

-export([
          default_pool/0
        , pool/1
        , pool/0
        , retries/0
        , retries/1
        , retry_sleep/0
        , retry_sleep/1
        , return_type/0
        , return_type/1
        ]).

-export([
          query_list/0
        , query_info/1
        , query_sql/1
        , query_refresh/0
        ]).

%% Some thoughts:
%%
%% -- perhaps the nxo_db ddl_source and sql_source should also be
%% cached and perhaps there should be a mechanism for augmenting the
%% list.  this would allow included applications to tack their DB
%% requirements onto what's configured in the base app.

-define(PRINT(Var),
        error_logger:info_msg("DEBUG: ~p~n~p:~p~n~p~n  ~p~n",
                              [self(), ?MODULE, ?LINE, ??Var, Var])).

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
return_type() ->
  nxo_db_cache:lookup(default_return, nxo_db_util:default_query_return()).

return_type(default) ->
  nxo_db_cache:set(default_return, nxo_db_util:default_query_return());
return_type(Return) ->
  nxo_db_cache:set(default_return, Return).

q(Query) ->
  q(Query, [], return_type(), #{}).

q(Query, Params) ->
  q(Query, Params, return_type(), #{}).

q(Query, Params, Type) ->
  q(Query, Params, Type, #{}).

q(Query, Params, Type, Options) ->
  nxo_db_util:q(Query, Params, Type, Options).


%% ListOfParams is {QueryText, [Params]}

%% RetryFlag (a bool) when true indicates that a failing batch should
%% be re-processed statement by statement and errors reported.

batch(ListOfParams) ->
  batch(ListOfParams, true).

batch(ListOfParams, RetryFlag) ->
  InSize = length(ListOfParams),
  Res = pgpool:batch(nxo_db:pool(), ListOfParams),
  try InSize = length(Res) of
    _ -> ok
  catch
    _:_ -> case RetryFlag of
             true -> iterate_batch(ListOfParams);
             false -> error_logger:error_msg("Batch Failes")
           end
  end.

  %% case InSize /= length(Res) of
  %%   true ->  %% batch had a failing statement
  %%     case RetryFlag of
  %%       true  -> iterate_batch(ListOfParams);
  %%       false -> error_logger:error_msg("Batch Failed")
  %%     end;
  %%   false ->
  %%     ok  %% batch succeeded
  %% end.

iterate_batch(ListOfParams) ->
  lists:filter(fun([ok, _]) -> false;
                  ([error, _]) -> true
               end,
               [ q(SQL, Params, parsed) || {SQL, Params} <- ListOfParams ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BACKWARDS COMPATIBILITY %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scalar_query(Query, Params) ->
  q(Query, Params, scalar).

list_query(Query, Params) ->
  q(Query, Params, list).

map_query(Query, Params) ->
  q(Query, Params, map).

query(Query, Params) ->
  q(Query, Params).


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



%% @doc Given a table, column, and value return true if exists.
-spec check_dup(string(), string(), string()) -> true | false.
check_dup(Table, Column, Value) ->
  case scalar_query(check_dup, [Table, Column, Value]) of
    0 -> false;
    _ -> true
  end.

-spec check_dup(string(), string(), string(), string(), string())
               -> true | false.
check_dup(Table, Column, Value, IDColumn, IDValue) ->
  case scalar_query(check_dup_with_id,
                    [Table, Column, Value, IDColumn, IDValue]) of
    0 -> false;
    _ -> true
  end.


%% @doc Execute a series of SQL insert/updates using returned values.
%%
%% This is a series of queries where the returned values (if any) are
%% pre-pended to the subsequent parameter list.  For instance, if a
%% row returns an ID, prepend that ID to the next query.  Each query
%% can return something different; that new value will be used in the
%% next query.
-spec cascading_update([{string(), list()}]) -> any().
cascading_update(Plan) ->
  cascading_update(Plan, []).

cascading_update([], Return) ->
  Return;
cascading_update([{Template, Params}|T], Return) ->
  %% returned values are binaries!
  AllParams = case Return of
                Lst when is_list(Lst) -> lists:append(Lst, Params);
                Bin when is_binary(Bin) -> lists:append([Bin], Params);
                _ -> Params
              end,
  Results = q(Template, AllParams, list),
  cascading_update(T, Results).

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
