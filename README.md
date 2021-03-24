nxo_db
=====

This is an opinionated "last mile" library to make working with pgpool
and epgsql a ltitle easier.  There are tasks which are intrinsic to DB
applications, nxo_db attempts to provide a standard library to manage
them.  Specifically:

* **DDL Management** `nxo_db:apply_full_ddl/0` executes the SQL
  scripts in a configured directory to create a DB structre (and
  indices, stored procedures, views, and so on).

* **SQL Management** SQL code can be saved either one statement per
  file or many statement in a bespoke eqlite format.

* **Result Formatting** Query results can be re-formatted into
  scalars, lists, or maps depending on requirements.  nxo_db can
  decide what the most natural structure is for the returned data.

What this is **not** is an ORM or a database migrations manager.
Those are great tools for many applications but I don't find they
provide value -- *for me* -- over plain old DML and DDL.

## Configuration

Here's an example configuration fragment from `sys.config`:

``` erlang
{nxo_db,
   [{pools, [#{name => db,
               host => "db",
               user => "myapp_user",
               pass => fun myapp:db_pass/1,
               database => "myapp_db"}]},
    {retries, 3},
    {retry_sleep, 400},
    {default_return, auto},
    {ddl_source, [{priv_dir, myapp, "ddl"}]},
    {sql_source, [{priv_dir, myapp, "sql"}]}

   ]}
```

1. `{pools, [...]}` configures a pgpool pool with simplified syntax and
   some defaults.  `name` is the pool name, the other parameters
   should be apparent.  in addition, `size`, `max_overflow`,
   `stragegy`, and `ssl` may be specified.

   The `pass` parameter may be a list, binary, or function/1 (as
   here).  The parameter is the pool name.  (This is different than
   epgsql which expects function/0; I find this useful for pulling
   passwords out of Docker secrets files.)

2. `{retries, 3}` indicates how many attempts nxo_db will make before
   giving up.  Once in a while DB handles go stale or get dropped or
   whatever and an additional attempt is useful because pgpool will
   have reestablished the handle pretty quickly.

3. `{retry_sleep, 400}` indicates how many ms to wait between each
   retry.

4. `{default_return, auto}` specifies how data returned from a query
   should be formatted.  The options here are `raw`, `auto`, `parsed`,
   `scalar`, `list`, and `map`.

5. `{ddl_source, [...]}` indicates where the DDL files are located.
     DDL files are assumed to have the extension `.sql`.

6. `{sql_source, [...]}` indicates where the DML files are located.
   DML files are assumed to have the extension `.sql` or `.eqlite`.

### Specifying Locations

`sql_source` and `ddl_source` values are lists of priv_dir
specifications or paths.  In both cases the full hierarchy below the
specified directory is searched (so files may be in a directory
structure for organization if that's helpful.

#### priv_dir

The `ddl_source` above translates to

``` erlang
filename:join(code:priv_dir(myapp) ++ "ddl")
```

#### path

The other option is `path` (e.g., `{path, "/var/tmp/sql"}` which is
exactly what you think it is.

## DML Files

DML can be specified in `.sql` files (which, because nxo_db uses
`equery`, are limited to one statement per file) or `.eqlite`
formatted files.

### .sql

Query names are the atomic name of the file minus the `.sql`
extension.  So `insert_order.sql` provides the query name
`insert_order`.

### .eqlite

``` sql
-- :select_everything
SELECT * FROM table_one;

-- :select_value
SELECT value FROM table_one WHERE name = $1;

-- :count  This is a comment.
SELECT count(*) FROM table_one;
```

What's important here is the `-- :query_name` marker; these delimit
the queries in the eqlite file.  You'll notice in the third query
there's a comment, these can be retrieved at runtime.  The atom-like
string delimited by the colon on the left and space (or EOL) on the
right is the query name.

### Query Cache

The DML files are read/parsed when `nxo_db:query_refresh/0` is
evaluated.  The results are stored in an ETS table.  Note that the
refresh must be called manually if the code is changed!


## Querying

`q/1-4` executes a query.  The four parameters are `QueryName`,
`Parameters`, `ReturnType`, and `Options`.

Here's an example of a /4 query:

``` erlang
nxo_db:q(select_value, [<<"Bob">>], map, #{ retries => 1, pool => db }).
```

This query could also be specified as:

``` erlang
nxo_db:q(select_value, ["Bob"]).
```

### Return Types

* **raw** returns the epgsql results directly.

* **map** returns a list like

  ``` erlang
  [#{ <<"col_1">> => <<"val1_c1">>, <<"col_2">> => <<"val1_c2">> },
   #{ <<"col_1">> => <<"val2_c1">>, <<"col_2">> => <<"val2_c2">> }]
  ```

  When set to `auto` a map will be returned when there are multiple
  columns selected.

 * **list** returns a list of values. When set to `auto` a list will
   be returned when there is one column with multiple rows returned.

 * **scalar** returns (wait for it) a scalar.  When set to `auto` a
   scalar will be returned when there is one column and one row
   returned (like from `count * from table`).

 * **parsed** converts the raw data from records to maps.  This can be
   useful for column introspection, for example.


## Caveats/Todos

Really there should be function documentation.  I apologize.

This represents about 98% of what I'll do with applications.  The goal
was to encapsulate the tasks I actually need performed not a
comprehensive front end.

It's not necessarily the most optimized or speedy library.  If you
need async queries and the like, epgsql is your friend.

Obviously this is PostgreSQL specific.

DDL statement should be idempotent.  This is only a pain the first few
dozen times.

If you have additions or needs, send a pull request or submit an issue.

## Author

Bunny Lushington
