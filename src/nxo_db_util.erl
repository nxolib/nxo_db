-module(nxo_db_util).
-include_lib("epgsql/include/epgsql.hrl").
-export([
          evaluate_file/1
        , q/4
        , squery/1
        , default_query_return/0

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

default_query_return() ->
  application:get_env(nxo_db, query_return, auto).

squery(SQL) ->
  query(squery, SQL, unused, nxo_db:retries(), nxo_db:pool()).

q(Query, Params, ReturnType, Options) when is_list(Options) ->
  q(Query, Params, ReturnType, maps:from_list(Options));

q(Query, Params, ReturnType, Options) when is_map(Options) ->
  SQL = case nxo_db_eqlite:get_query(Query) of
          undefined -> error(query_not_found);
          Code -> Code
        end,
  Retries = maps:get(retries, Options, nxo_db:retries()),
  Pool = maps:get(pool, Options, nxo_db:pool()),
  Res = query(equery, SQL, Params, Retries, Pool),
  format_return(ReturnType, Res).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_return(auto, Res) ->
  format_return(auto_return_type(Res), Res);

format_return(map, {ok, Columns, Vals}) ->
  ColumnNames = [  C#column.name || C <- Columns ],
  [ maps:from_list(lists:zip(ColumnNames, tuple_to_list(R))) || R <- Vals ];

format_return(map, {ok, _Count, Columns, Vals}) ->
  format_return(map, {ok, Columns, Vals});

format_return(list, {ok, 1, _Columns, [Vals]}) ->
  tuple_to_list(Vals);

format_return(list, {ok, _Columns, Vals}) ->
  [ V || {V} <- Vals ];

format_return(list, {ok, _Count, _Columns, Vals}) ->
  Vals;

format_return(scalar, {ok, _Columns, Val}) ->
  case Val of
    [{V}] -> V;
    [] -> []
  end;

format_return(scalar, {ok, _Count, _Columns, [{Val}]}) ->
  Val;

format_return(parsed, Res) ->
  parse_results(Res);

format_return(raw, Res) ->
  Res.




auto_return_type({error, _}) ->
  parsed;
auto_return_type({ok, _}) ->
  parsed;
auto_return_type({ok, Columns, Rows}) ->
  if
    length(Columns) == 1, length(Rows) == 1 -> scalar;
    length(Columns) == 1                    -> list;
    true                                    -> map
  end;
auto_return_type({ok, _Count, Columns, Rows}) ->
  auto_return_type({ok, Columns, Rows}).


query(Type, SQL, Params, Retries, Pool) ->
  {M, F, A} = case Type == equery of
                true -> {pgpool, equery, [Pool, SQL, Params]};
                false -> {pgpool, squery, [Pool, SQL]}
              end,
  try
    apply(M, F, A)
  catch
    exit:{{noproc, _}, _}=Error ->
      query_helper(Type, SQL, Params, Retries, Pool, Error);
    exit:{{sock_closed, _}, _}=Error ->
      query_helper(Type, SQL, Params, Retries, Pool, Error)
  end.

query_helper(Type, SQL, Params, Retries, Pool, Error) ->
  case Retries > 0 of
    true ->
      timer:sleep(nxo_db:retry_sleep()),
      query(Type, SQL, Params, Retries - 1, Pool);
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
parse_results({ok, Columns, Rows}) ->
  [ok, #{ columns => columns_to_map(Columns), rows => Rows }];
parse_results({ok, Count, Columns, Rows}) ->
  [ok, #{ count => Count, columns => columns_to_map(Columns), rows => Rows }];
parse_results(Unaccounted) ->
  [error, Unaccounted].


%% Take the list of column records [{column, ...}, {column, ...}] and
%% replace it with a list of maps keyed on the column fields.
columns_to_map(Columns) ->
  Fields = record_info(fields, column),
  [ maps:from_list(lists:zip(Fields, tl(tuple_to_list(C)))) || C <- Columns ].
