-module(nxo_db_eqlite).
-behaviour(gen_server).

-export([
          init/1
        , stop/0
        , get_query/1
        , get_info/1
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
-spec get_query(atom()) -> string().
get_query(Query) ->
  gen_server:call(?SERVER, {get_query, Query}).

%% @doc Retrieve the info from the specified query.
-spec get_info(atom()) -> string().
get_info(Query) ->
  gen_server:call(?SERVER, {get_info, Query}).

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
    [{Query, _Info, Statement}] -> Statement;
    _ -> undefined
  end.

internal_get_info(Query) ->
  case ets:lookup(?EQLITE_TAB, Query) of
    [{Query, Info, _Statement}] -> Info;
    _ -> undefined
  end.

internal_list_queries() ->
  Qs = ets:foldl(fun({Query, _, _}, Acc) -> [Query | Acc] end,
                 [], ?EQLITE_TAB),
  lists:sort(Qs).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN_SERVER FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({get_info, Query}, _From, State) ->
  {reply, internal_get_info(Query), State};

handle_call({get_query, Query}, _From, State) ->
  {reply, internal_get_query(Query), State};

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
  io:format("Parsing eqlite file ~s~n", [Filepath]),
  {ok, FileIO} = file:open(Filepath, [read]),
  maps:merge(parse_file(FileIO), Acc);
read_file(".sql", Filepath, Acc) ->
  io:format("Parsing SQL file ~s~n", [Filepath]),
  Query = list_to_atom(filename:basename(Filepath, ".sql")),
  {ok, Data} = file:read_file(Filepath),
  maps:merge(#{ Query => #{info => "SQL File", data => Data }}, Acc).

parse_file(FileIO) ->
  parse_line(FileIO, file:read_line(FileIO), #{}, []).

parse_line(FileIO, eof, Acc, _CurrentQuery) ->
  file:close(FileIO),
  Acc;

parse_line(FileIO, {ok, Line}, Acc, CurrentQuery) ->
  {NewCurrentQuery, NewAcc} =
    case string:trim(Line, leading) of
      "-- :" ++ Rest ->                         % query name
        [RawQueryName | Info] = string:split(Rest, " "),
        QueryName = list_to_atom(string:trim(RawQueryName)),
        NewMap = maps:put(QueryName, #{ data => [],
                                        info => string:trim(Info) }, Acc),
        {QueryName, NewMap};
      "--" ++ _ ->                              % comment line
        {CurrentQuery, Acc};
      "" ->                                     % blank line
        {CurrentQuery, Acc};
      Code ->                                   % line of code
        CurrentQueryMap = maps:get(CurrentQuery, Acc),
        CurrentLines = maps:get(data, CurrentQueryMap, []),
        NewLines = CurrentLines ++ Code,
        NewQueryMap = maps:put(data, NewLines, CurrentQueryMap),
        {CurrentQuery, maps:put(CurrentQuery, NewQueryMap, Acc)}
      end,
  parse_line(FileIO, file:read_line(FileIO), NewAcc, NewCurrentQuery).


%% Given a map of queries like
%%   #{ query_name => #{info => [], data => []}
%% insert {query_name, info, data} into the ETS table.
file_eqlite_queries(Queries, Table) ->
  Iterator = maps:iterator(Queries),
  query_iterator(maps:next(Iterator), Table).

query_iterator(none, _Table) ->
  ok;
query_iterator({K, V, Iterator}, Table) ->
  ets:insert(Table, {K, maps:get(info, V, []), maps:get(data, V, [])}),
  query_iterator(maps:next(Iterator), Table).
