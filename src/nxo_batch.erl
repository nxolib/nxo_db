-module(nxo_batch).
-export([ batch/4
        , execute_batch/4
        , run_query/5
        ]).

-define(PRINT(Var),
        error_logger:info_msg("DEBUG: ~p~n~p:~p~n~p~n  ~p~n",
                              [self(), ?MODULE, ?LINE, ??Var, Var])).

batch(SQL, ListOfParams, RetryFlag, Pool) ->
  process_flag(trap_exit, true),
  spawn_monitor(?MODULE, execute_batch, [SQL, ListOfParams, RetryFlag, Pool]),
  receive
    {'DOWN', _, _, _, normal} -> ok;
    _ -> iterate_batch(SQL, ListOfParams)
  after
    10000 -> error(batch_timeout)
  end.

execute_batch(SQL, ListOfParams, RetryFlag, Pool) ->
  InSize = length(ListOfParams),
  {ok, Connection} = nxo_db_pool:connection(Pool),
  {_, Res} = epgsql:execute_batch(Connection, SQL, ListOfParams),
  try InSize = length(Res) of
    _ -> ok
  catch
    _:_ ->
      case RetryFlag of
        true -> iterate_batch(SQL, ListOfParams);
        false -> error_logger:error_msg("Batch Failed")
      end
  after
    epgsql:close(Connection)
  end,
  Res.


%% NB: safe_q is executed one at a time to prevent resource depletion.
iterate_batch(SQL, ListOfParams) ->
  lists:filter(fun([ok, _]) -> false;
                  ([error, _]) -> true
               end,
               [ safe_q(SQL, Params, parsed) || Params <- ListOfParams ]).

safe_q(SQL, Params, Type) ->
  process_flag(trap_exit, true),
  spawn_monitor(?MODULE, run_query, [SQL, Params, Type, #{}, self()]),
  receive_query_results([], Params).

receive_query_results(Res, Params) ->
  receive
    [ok, Result] ->
      receive_query_results([ok, Result], Params);
    {'DOWN', _, _, _, normal} ->
      Res;
    {'DOWN', _, _, _, _} ->
      [error, {params, Params}]
  after
    10000 ->
      error(iterate_timeout)
  end.

run_query(SQL, Params, Type, Options, Caller) ->
  Caller ! nxo_db_util:q(SQL, Params, Type, Options).
