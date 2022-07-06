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

%% @doc Reads nxo_db/pools environment configuration.
%%
%% If unset, pgpool/databases configuration is set based on the data;
%% if pgpool/databases is not undefined, this function no-opts.
-spec config() -> ok.
config() ->
  case application:get_env(pgpool, databases) of
    undefined ->
      Pools = application:get_env(nxo_db, pools, []),
      Databases = lists:map(fun configure_pool/1, Pools),
      application:set_env(pgpool, databases, Databases);
    {ok, _} ->
      ok
  end.

%% @doc Establish an epgsql connection (or error)for the named DB..If
%% the DB speciied isn't found in the nxo_db/pools configuration,
%% returns undefined.
-spec connection(atom()) -> {ok, epgsql:connection()}
          | {error, epgsql:connection_error()}
          | undefined.
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

%% @doc Returns the name of the default DB pool.  The default pool is
%% the first DB connection defined.  If no pools are defined, returns
%% undefined.
-spec default() -> atom() | undefined.
default() ->
  case application:get_env(pgpool, databases) of
    {ok, [{Default, _} | _]} -> Default;
    _ -> undefined
  end.

%% @doc The configured number of query retries (default: 1).
-spec retries() -> integer().
retries() ->
  application:get_env(nxo_db, retries, 1).

%% @doc The time to sleep (ms) between retries (default: 500).
-spec retry_sleep() -> integer().
retry_sleep() ->
  application:get_env(nxo_db, retry_sleep, 500).

%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

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
get_pool_password(_Name, {file, File}) ->
  {ok, PW} = file:read_file(File),
  binary_to_list(PW);
get_pool_password(Name, Fn) when is_function(Fn)->
  Fn(Name);
get_pool_password(_Name, Value) ->
  Value.
