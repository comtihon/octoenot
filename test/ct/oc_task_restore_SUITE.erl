%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 20:59
%%%-------------------------------------------------------------------
-module(oc_task_restore_SUITE).
-author("tihon").

-compile(export_all).

-include("oc_tasks.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
  [
    test_restore_all_tasks
  ].

init_per_suite(Config) ->
  application:ensure_all_started(erlexec),
  Config.

init_per_testcase(_, Config) ->
  ok = oc_sqlite_mngr:init(),
  Config.

end_per_testcase(_, Config) ->
  ok = file:delete(atom_to_list(?EMBEDDED_STORAGE) ++ ".db"),
  meck:unload(),
  Config.

end_per_suite(Config) ->
  Config.

%% Octocoon starts and have some tasks unfinished. Should finish them all
test_restore_all_tasks(_) ->
  ct:pal("------------------~p------------------~n", [test_restore_all_tasks]),

  {ok, Db} = oc_sqlite_mngr:connect(),
  true = oc_sqlite_mngr:add_task(Db, <<"ns/proj1">>, <<"url1">>, <<"1.0.0">>),
  true = oc_sqlite_mngr:add_task(Db, <<"ns/proj2">>, <<"url2">>, <<"1.0.0">>),
  true = oc_sqlite_mngr:add_task(Db, <<"ns/proj3">>, <<"url3">>, <<"1.0.0">>),

  Self = self(),
  meck:new(oc_loader_mngr),
  meck:expect(oc_loader_mngr, add_package, fun(Name, Url, Tag) -> Self ! {add, Name, Url, Tag} end),

  {ok, Pid} = oc_namespace_limiter:start_link(),
  Pid ! check,

  true = accert_added(<<"ns/proj1">>, <<"url1">>, <<"1.0.0">>),
  true = accert_added(<<"ns/proj2">>, <<"url2">>, <<"1.0.0">>),
  true = accert_added(<<"ns/proj3">>, <<"url3">>, <<"1.0.0">>),

  ok.


%% @private
accert_added(Name, Url, Tag) ->
  receive
    {add, Name, Url, Tag} -> true
  after
    100 -> false
  end.