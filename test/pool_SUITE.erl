-module(pool_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
          suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2
        , groups/0
        , all/0
        ]).

-export([
          config/1
        , connection/1
        , default/1
        , retries/1
        , retry_sleep/1
        ]).

-export([ password/1 ]).

suite() ->
  [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

init_per_group(_GroupName, Config) ->
  Config.

end_per_group(connection_tests, Config) ->
  ok = nxo_db:config(),
  Config;
end_per_group(_, Config) ->
  Config.

init_per_testcase(_TestCase, Config) ->
  Config.

end_per_testcase(_TestCase, _Config) ->
  application:unset_env(pgpool, databases).

groups() ->
  [
   {connection_tests, [connection, retries, retry_sleep]}
  ].

all() ->
  [config, default, {group, connection_tests}].


%%%%%%%%%%%
%% TESTS %%
%%%%%%%%%%%

config(_Config) ->
  undefined = application:get_env(pgpool, databases),
  ok = nxo_db:config(),
  ok = nxo_db:config(),
  {ok, Databases} = application:get_env(pgpool, databases),
  [DBOne, DBTwo] = Databases,
  {db_one, [_, {connection, [{host, "db_one"},
                             {user, "db_one_user"},
                             {pass, "db_one_pass"},
                             {options, _}]}]} = DBOne,
  {db_two, [_, {connection, [{host, "db_two"},
                             {user, "db_two_user"},
                             {pass, "db_two_pass"},
                             {options, _}]}]} = DBTwo.


connection(_Config) ->
  undefined = nxo_db:connection(not_a_name),
  {ok, Conn1} = nxo_db:connection(db_one),
  {ok, Conn2} = nxo_db:connection(db_two),
  true = is_pid(Conn1),
  true = is_pid(Conn2),
  ok = epgsql:close(Conn1),
  ok = epgsql:close(Conn2).


default(_Config) ->
  undefined = nxo_db_pool:default(),
  nxo_db:config(),
  db_one = nxo_db_pool:default().

retries(_Config) ->
  3 = nxo_db_pool:retries().

retry_sleep(_Config) ->
  400 = nxo_db_pool:retry_sleep().


%% used in the pool config.
password(Name) ->
  atom_to_list(Name) ++ "_pass".
