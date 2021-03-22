-module(nxo_db).
-export([
          start/0
        ]).

start() ->
  nxo_db_pool:config(),
  ok = application:ensure_started(eqlite),
  ok = pgpool:start(),
  ok = application:ensure_started(nxo_db).
