-module(nxo_db_eqlite).
-behaviour(gen_server).

-export([
          init/1
        , stop/0
        , get_query/1
        , get_info/1
        , get_placeholders/1
        , wash_placeholders/2
        , list_queries/0
        , refresh/1
        ]).

-export([start_link/0]).

-export([ handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , format_status/2
        ]).

-define(EQLITE_TAB, eqlite_table).
-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?SERVER, stop).

-spec init(string() | [string()]) -> ok.
init([]) ->
  new_table(?EQLITE_TAB),
  process_flag(trap_exit, true),
  {ok, #{}}.

%% @doc Retrive the specified query.
-spec get_query(atom()|string()) -> string().
get_query(Query) when is_atom(Query)->
  gen_server:call(?SERVER, {get_query, Query});
get_query(Query) when is_list(Query); is_binary(Query) ->
  Query.


%% @doc Retrieve the info from the specified query.
-spec get_info(atom()) -> string().
get_info(Query) ->
  gen_server:call(?SERVER, {get_info, Query}).

%% @doc Retrieve the parameter list from the specified query
-spec get_placeholders(atom()) -> [atom()] | no_placeholders.
get_placeholders(Query) ->
  gen_server:call(?SERVER, {get_placeholders, Query}).

%% @doc Convert a param map into a parameter list for the query
-spec wash_placeholders(atom(), map()) -> list().
wash_placeholders(_Query, Params) when is_list(Params) -> Params;
wash_placeholders(Query, Params) ->
  case get_placeholders(Query) of
    PList when is_list(PList) -> convert_param_map_to_list(PList, Params);
    _ -> undefined
  end.

convert_param_map_to_list(PList, PMap) ->
  [ maps:get(K, PMap, null) || K <- PList ].

%% @doc List the avaialble queries.
-spec list_queries() -> [atom()].
list_queries() ->
  gen_server:call(?SERVER, list_queries).

%% @doc Refresh the cache.
-spec refresh(list()) -> ok.
refresh(Files) ->
  gen_server:call(?SERVER, {refresh, Files}).

internal_get_query(Query) ->
  case ets:lookup(?EQLITE_TAB, Query) of
    [{Query, _Info, Statement, _Placeholders}] -> Statement;
    _ -> undefined
  end.

internal_get_info(Query) ->
  case ets:lookup(?EQLITE_TAB, Query) of
    [{Query, Info, _Statement, _Placeholders}] -> Info;
    _ -> undefined
  end.

internal_get_placeholders(Query) ->
  case ets:lookup(?EQLITE_TAB, Query) of
    [{Query, _Info, _Statement, Placeholders}] -> Placeholders;
    _ -> undefined
  end.

internal_list_queries() ->
  Qs = ets:foldl(fun({Query, _, _, _}, Acc) -> [Query | Acc] end,
                 [], ?EQLITE_TAB),
  lists:sort(Qs).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({get_info, Query}, _From, State) ->
  {reply, internal_get_info(Query), State};

handle_call({get_query, Query}, _From, State) ->
  {reply, internal_get_query(Query), State};

handle_call({get_placeholders, Query}, _From, State) ->
  {reply, internal_get_placeholders(Query), State};

handle_call(list_queries, _From, State) ->
  {reply, internal_list_queries(), State};

handle_call(stop, _From, State) ->
  {stop, normal, shutdown_ok, State};

handle_call({refresh, Files}, _From, State) ->
  load_queries(Files),
  {reply, internal_list_queries(), State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
format_status(_Opt, Status) ->  Status.


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%
new_table(Name) ->
  Table = case ets:whereis(Name) of
            undefined -> ets:new(Name, [named_table, set, public,
                                        {read_concurrency, true}]);
            _ -> Name
  end,
  ets:delete_all_objects(Table),
  Table.


load_queries(Files) ->
  new_table(?EQLITE_TAB),
  Queries = parse_files(Files),
  file_eqlite_queries(Queries, ?EQLITE_TAB).


parse_files(Files) ->
  lists:foldl(fun(Filepath, Acc) ->
                  read_file(filename:extension(Filepath), Filepath, Acc)
              end,
              #{}, Files).

read_file(".eqlite", Filepath, Acc) ->
  logger:notice("Parsing eqlite file ~s~n", [Filepath]),
  {ok, FileIO} = file:open(Filepath, [read]),
  maps:merge(parse_file(FileIO), Acc);
read_file(".sql", Filepath, Acc) ->
  logger:notice("Parsing SQL file ~s~n", [Filepath]),
  Query = list_to_atom(filename:basename(Filepath, ".sql")),
  {ok, Data} = file:read_file(Filepath),
  maps:merge(#{ Query => #{info => "SQL File", data => Data }}, Acc).

parse_file(FileIO) ->
  parse_line(FileIO, file:read_line(FileIO), #{}, []).

parse_line(FileIO, eof, Acc, _CurrentQuery) ->
  file:close(FileIO),
  NewAcc = maps:map(fun update_placeholder_info/2, Acc),
  NewAcc;
parse_line(FileIO, {ok, Line}, Acc, CurrentQuery) ->
  {NewCurrentQuery, NewAcc} =
    case string:trim(Line, leading) of
      "-- :" ++ Rest ->                         % query name
        [RawQueryName | Info] = string:split(Rest, " "),
        QueryName = list_to_atom(string:trim(RawQueryName)),
        NewMap = maps:put(QueryName, #{ data => [],
                                        placeholders => {0, #{}},
                                        info => string:trim(Info) }, Acc),
        {QueryName, NewMap};
      "--" ++ _ ->                              % comment line
        {CurrentQuery, Acc};
      "" ->                                     % blank line
        {CurrentQuery, Acc};
      Code ->                                   % line of code
        CurrentQueryMap = maps:get(CurrentQuery, Acc),
        CurrentPlaceholders = maps:get(placeholders, CurrentQueryMap),
        {NewCode, NewPlaceholders} =
          find_placeholders(Code, CurrentPlaceholders),
        CurrentLines = maps:get(data, CurrentQueryMap, []),
        NewLines = CurrentLines ++ NewCode,
        NewQueryMap1 = maps:put(data, NewLines, CurrentQueryMap),
        NewQueryMap2 = maps:put(placeholders, NewPlaceholders, NewQueryMap1),
        {CurrentQuery, maps:put(CurrentQuery, NewQueryMap2, Acc)}
      end,
  parse_line(FileIO, file:read_line(FileIO), NewAcc, NewCurrentQuery).


update_placeholder_info(_Key, V) ->
  {_, PH} = maps:get(placeholders, V),
  Map = maps:fold(fun(K1, V1, Acc) ->
                      K2 = list_to_atom(string:trim(K1, leading, ":")),
                      V2 = list_to_integer(string:trim(V1, leading, "$")),
                      maps:put(V2, K2, Acc)
                  end, #{}, PH),
  ParamFun =
    fun(Params) when is_list(Params) -> Params;
       (Params) when is_map(Params) ->
        lists:map(fun(N) ->
                      ParamKey = maps:get(N, Map),
                      case maps:get(ParamKey, Params, not_found) of
                        not_found -> error(missing_query_parameter);
                        ParamValue -> ParamValue
                      end
                  end, lists:seq(1, maps:size(Map)))
    end,
  maps:put(paramter_fun, ParamFun, V).


find_placeholders(Line, Placeholders) ->
  case re:run(Line, "(?<!:)(:[a-z][a-zA-Z0-9_]*)",
              [global, {capture, all_but_first, list}]) of
    nomatch ->
      {Line, Placeholders};
    {match, Captures} ->
      {NewLine, NewPlaceholders} =
        replace_placeholders(Line, Captures, Placeholders),
      {NewLine, NewPlaceholders}
  end.

replace_placeholders(Line, [], Placeholders) ->
  {Line, Placeholders};
replace_placeholders(Line, [[PH]|T], {LastCount, Map}) ->
  case maps:get(PH, Map, unseen) of
    unseen ->
      NewCount = LastCount + 1,
      NewPlaceholder = "$" ++ integer_to_list(NewCount),
      NewMap = maps:put(PH, NewPlaceholder, Map),
      NewLine = re:replace(Line, PH, NewPlaceholder, [{return,list}]),
      replace_placeholders(NewLine, T, {NewCount, NewMap});
    Place  ->
      NewLine = re:replace(Line, PH, Place, [{return,list}]),
      replace_placeholders(NewLine, T, {LastCount, Map})
  end.

%% Given a map of queries like
%%   #{ query_name => #{info => [], data => []}
%% insert {query_name, info, data} into the ETS table.
file_eqlite_queries(Queries, Table) ->
  Iterator = maps:iterator(Queries),
  query_iterator(maps:next(Iterator), Table).

query_iterator(none, _Table) ->
  ok;
query_iterator({K, V, Iterator}, Table) ->
  Placeholders = placeholder_fixup(maps:get(placeholders, V, {0, #{}})),
  ets:insert(Table, {K,
                     maps:get(info, V, []),
                     maps:get(data, V, []),
                     Placeholders}),
  query_iterator(maps:next(Iterator), Table).


placeholder_fixup({0, _}) ->
  no_placeholders;
placeholder_fixup({Count, Map}) ->
  NewMap =
    maps:fold(fun(K, V, Acc) ->
                  WashedK = list_to_atom(string:trim(K, leading, ":")),
                  WashedV = list_to_integer(string:trim(V, leading, "$")),
                  maps:put(WashedV, WashedK, Acc )
              end, #{}, Map),
  [ maps:get(N, NewMap) || N <- lists:seq(1, Count) ].
