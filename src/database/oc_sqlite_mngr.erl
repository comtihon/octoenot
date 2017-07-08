%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 18:53
%%%-------------------------------------------------------------------
-module(oc_sqlite_mngr).
-author("tihon").

-include("oc_tasks.hrl").
-include("oc_database.hrl").

-define(SELECT_PACKAGES, "select ~s, ~s from ~s where ~s = '~s' and ~s = '~s'").

%% API
-export([init/0, connect/1, get_all_tasks/1]).
-export([add_task/4, del_task/2]).
-export([add_package/5, get_package/3]).

-spec init() -> ok | error.
init() ->
  ok = check_installed(),
  ok = setup_db(?TASKS_STORAGE, ?TASKS_TABLE, ?TASKS_TABLE_SCHEMA),
  ok = setup_db(?PACKAGES_STORAGE, ?PACKAGES_TABLE, ?PACKAGES_TABLE_SCHEMA).

-spec connect(atom()) -> {ok, pid()} | {error, any()}.
connect(DbName) ->
  DbFile = filename:join([oc_utils:get_priv_dir(), atom_to_list(DbName) ++ ".db"]),
  sqlite3:open(DbName, [{file, DbFile}]).

-spec add_task(pid() | atom(), binary(), binary(), binary()) -> true.
add_task(Db, Name, Url, Tag) ->
  {rowid, _} = sqlite3:write(Db, ?TASKS_TABLE, [{?NAME_FIELD, Name}, {?URL_FIELD, Url}, {?TAG_FIELD, Tag}]),
  true.

-spec add_package(pid() | atom(), binary(), binary(), binary(), binary()) -> true.
add_package(Db, Ns, Name, Vsn, Erl) ->
  {rowid, _} = sqlite3:write(Db, ?PACKAGES_TABLE,
    [{?NAMESPACE_FIELD, Ns}, {?PACKAGE_FIELD, Name}, {?VERSION_FIELD, Vsn}, {?ERLANG_FIELD, Erl}]),
  true.

-spec get_package(pid() | atom(), binary(), binary()) -> list().
get_package(Db, Ns, Name) ->
  Sql = lists:flatten(io_lib:format(?SELECT_PACKAGES,
    [?VERSION_FIELD, ?ERLANG_FIELD, ?PACKAGES_TABLE, ?NAMESPACE_FIELD, Ns, ?PACKAGE_FIELD, Name])),
  Result = sqlite3:sql_exec(Db, Sql),
  proplists:get_value(rows, Result, []).

-spec del_task(pid() | atom(), binary()) -> boolean().
del_task(Db, Name) ->  % TODO tag
  ok == sqlite3:delete(Db, ?TASKS_TABLE, {?NAME_FIELD, Name}).

-spec get_all_tasks(pid() | atom()) -> list().
get_all_tasks(Db) ->
  sqlite3:read_all(Db, ?TASKS_TABLE).


%% @private
check_installed() ->
  case oc_utils:exec("sqlite3 -version", [sync, stdout, stderr]) of
    {error, Err} ->
      Status = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stderr, Err),
      oc_logger:err("Calling sqlite3 error(~p): ~p", [Status, StdErr]),
      error;
    {ok, Reply} ->
      [Vsn] = proplists:get_value(stdout, Reply),
      [V | _] = string:tokens(binary_to_list(Vsn), " "),
      oc_logger:info("Sqlite vsn ~s", [V]),
      ok
  end.

%% @private
setup_db(Db, Table, Schema) ->
  case connect(Db) of
    {ok, Pid} ->
      ok = populate_if_needed(Pid, Table, Schema),
      sqlite3:close(Pid),
      ok;
    Err ->
      oc_logger:err("Can't open ~p with ~p", [Db, Err]),
      error
  end.

%% @private
populate_if_needed(Db, TableName, Schema) ->
  case sqlite3:table_info_timeout(Db, TableName, 10000) of
    table_does_not_exist ->
      sqlite3:create_table_timeout(Db, TableName, Schema, 10000);
    _ ->
      ok
  end.