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

%% API
-export([init/0, connect/0, add_task/4, del_task/2, get_all_tasks/1]).

-spec init() -> ok | error.
init() ->
  ok = check_installed(),
  case connect() of
    {ok, Pid} ->
      ok = populate_if_needed(Pid),
      sqlite3:close(Pid),
      ok;
    Err ->
      oc_logger:err("Can't open ~p with ~p", [?EMBEDDED_STORAGE, Err]),
      error
  end.

-spec connect() -> {ok, pid()} | {error, any()}.
connect() ->
  sqlite3:open(?EMBEDDED_STORAGE).

-spec add_task(pid() | atom(), binary(), binary(), binary()) -> boolean().
add_task(Db, Name, Url, Tag) ->
  Res = sqlite3:write(Db, ?TASKS_TABLE, [{?NAME_FIELD, Name}, {?URL_FIELD, Url}, {?TAG_FIELD, Tag}]),
  io:format("~p~n", [Res]),
  true.

-spec del_task(pid() | atom(), binary()) -> boolean().
del_task(Db, Name) ->  % TODO tag
  Res = sqlite3:delete(Db, ?TASKS_TABLE, {?NAME_FIELD, Name}),
  io:format("~p~n", [Res]),
  true.

-spec get_all_tasks(pid() | atom()) -> list().
get_all_tasks(Db) ->
  sqlite3:read_all(Db, ?TASKS_TABLE).


%% @private
check_installed() ->
  case exec:run("sqlite3 -version", [sync, stdout, stderr]) of
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
populate_if_needed(Db) ->
  case sqlite3:create_table(Db, ?TASKS_TABLE, ?TASKS_TABLE_SCHEMA) of
    ok -> ok;
    {error, _, "table tasks already exists"} -> ok;
    Other ->
      oc_logger:err("Can't populate table ~p with ~p", [?TASKS_TABLE, Other]),
      error
  end.