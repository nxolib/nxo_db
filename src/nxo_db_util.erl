-module(nxo_db_util).
-include_lib("epgsql/include/epgsql.hrl").
-export([
          evaluate_file/1
        ]).

evaluate_file(Filepath) ->
  Filename = filename:basename(Filepath),
  {ok, SQL} = file:read_file(Filepath),
  case parse_results(equery(SQL)) of
    [ok, _] ->
      io:format("OK: ~s~n", [Filename]);
    [error, E] ->
      io:format("ERROR: ~s (~s)~n", [Filename, maps:get(message, E)])
  end.



equery(SQL) ->
  equery(SQL, []).

equery(SQL, Params) ->
  pgpool:equery(nxo_db:pool(), SQL, Params).


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
  [ok, #{ count => Count, columns => Columns, rows => Rows }].
