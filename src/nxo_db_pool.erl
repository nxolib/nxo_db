-module(nxo_db_pool).
-export([
          config/0
        , default/0
        , retries/0
        , retry_sleep/0
        ]).

config() ->
  case application:get_env(pgpool, databases) of
    undefined ->
      Pools = application:get_env(nxo_db, pools, []),
      Databases = lists:map(fun configure_pool/1, Pools),
      application:set_env(pgpool, databases, Databases);
    {ok, _} ->
      ok
  end.

default() ->
  case application:get_env(pgpool, databases) of
    {ok, [{Default, _} | _]} -> Default;
    _ -> undefined
  end.

retries() ->
  application:get_env(nxo_db, retries, 1).

retry_sleep() ->
  application:get_env(nxo_db, retry_sleep, 500).

configure_pool(P) ->
  Name = maps:get(name, P, db),
  Size = maps:get(size, P, 10),
  Max = maps:get(max_overflow, P, 20),
  Strategy = maps:get(strategy, P, lifo),
  Host = maps:get(host, P, Name),
  User = maps:get(user, P, []),
  SSL = maps:get(ssl, P, false),
  Database = maps:get(database, P, []),
  Pass = get_pool_password(Name, maps:get(pass, P, undefined)),
  {Name, [{pool, [{size, Size}, {max_overflow, Max}, {strategy, Strategy}]},
          {connection, [{host, Host}, {user, User}, {pass, Pass},
                        {options, [{ssl, SSL}, {database, Database}]}]}]}.

get_pool_password(_Name, undefined) ->
  [];
get_pool_password(Name, Fn) when is_function(Fn)->
  Fn(Name);
get_pool_password(_Name, Value) ->
  Value.
