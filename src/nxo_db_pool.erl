-module(nxo_db_pool).
-export([
          config/0
        , connection/1
        , default/0
        , retries/0
        , retry_sleep/0
        ]).

-define(PRINT(Var),
        error_logger:info_msg("DEBUG: ~p~n~p:~p~n~p~n  ~p~n",
                              [self(), ?MODULE, ?LINE, ??Var, Var])).

config() ->
  case application:get_env(pgpool, databases) of
    undefined ->
      Pools = application:get_env(nxo_db, pools, []),
      Databases = lists:map(fun configure_pool/1, Pools),
      application:set_env(pgpool, databases, Databases);
    {ok, _} ->
      ok
  end.

connection(Name) ->
  Pools = lists:filter(fun(#{name := DB}) when DB == Name -> true;
                          (_)                             -> false
                      end, application:get_env(nxo_db, pools, [])),
  case Pools of
    [P] ->
      Password = get_pool_password(Name, maps:get(pass, P, undefined)),
      epgsql:connect(#{host     => maps:get(host, P, Name),
                       username => maps:get(user, P, []),
                       password => Password,
                       ssl      => maps:get(ssl, P, false),
                       database => maps:get(database, P, [])});

    _ ->
      undefined
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
