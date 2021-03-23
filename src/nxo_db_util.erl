-module(nxo_db_util).
-include_lib("epgsql/include/epgsql.hrl").
-export([
          evaluate_file/1
        ]).

-export([
          equery/1
        , equery/2
        , squery/1
        ]).

evaluate_file(Filepath) ->
  Filename = filename:basename(Filepath),
  {ok, SQL} = file:read_file(Filepath),
  case parse_results(squery(SQL)) of
    [Res, _] when Res == ok; Res == multi ->
      io:format("OK: ~s~n", [Filename]);
    [error, E] ->
      io:format("ERROR: ~s (~s)~n", [Filename, maps:get(message, E)])
  end.


squery(SQL) ->
  query(squery, SQL, unused, nxo_db:retries()).

equery(SQL) ->
  query(equery, SQL, [], nxo_db:retries()).

equery(SQL, Params) ->
  query(equery, SQL, Params, nxo_db:retries()).

query(Type, SQL, Params, Retries) ->
  {M, F, A} = case Type == equery of
                true -> {pgpool, equery, [nxo_db:pool(), SQL, Params]};
                false -> {pgpool, squery, [nxo_db:pool(), SQL]}
              end,
  try
    apply(M, F, A)
  catch
    exit:{{noproc, _}, _}=Error ->
      query_helper(Type, SQL, Params, Retries, Error);
    exit:{{sock_closed, _}, _}=Error ->
      query_helper(Type, SQL, Params, Retries, Error)
  end.

query_helper(Type, SQL, Params, Retries, Error) ->
  case Retries > 0 of
    true ->
      timer:sleep(nxo_db:retry_sleep()),
      query(Type, SQL, Params, Retries - 1);
    false ->
      exit(Error)
  end.

parse_results(MultiResults) when is_list(MultiResults)->
  [multi, [ parse_results(R) || R <- MultiResults ]];
parse_results({error, Error}) ->
  X = #{ severity => Error#error.severity,
         code => Error#error.code,
         codename => Error#error.codename,
         message => Error#error.message },
  Y = try maps:from_list(Error#error.extra) of
        Extra -> Extra
      catch
        _:_ -> #{}
      end,
  [error, maps:merge(Y, X)];
parse_results({ok, Count}) ->
  [ok, #{ count => Count }];
parse_results({ok, Count, Rows}) ->
  [ok, #{ count => Count, rows => Rows }];
parse_results({ok, Count, Columns, Rows}) ->
  [ok, #{ count => Count, columns => Columns, rows => Rows }];
parse_results(Unaccounted) ->
  [error, Unaccounted].
