%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2017 2:55 PM
%%%-------------------------------------------------------------------
-module(oc_task_postphone_SUITE).
-author("tihon").

-compile(export_all).

-include("oc_database.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PACKAGE_DB, filename:join([oc_utils:get_priv_dir(), atom_to_list(?PACKAGES_STORAGE) ++ ".db"])).
-define(TASKS_DB, filename:join([oc_utils:get_priv_dir(), atom_to_list(?TASKS_STORAGE) ++ ".db"])).

all() ->
  [
    test_normal_pass,
    test_postpone,
    test_pass_after_postpone
  ].

init_per_suite(Config) ->
  application:ensure_all_started(sqlite3),
  application:set_env(octoenot, delay_between_build, 1),
  Config.

init_per_testcase(_, Config) ->
  ok = filelib:ensure_dir(oc_utils:get_priv_dir()),
  {ok, _} = oc_namespace_limiter:start_link(),
  Config.

end_per_testcase(_, Config) ->
  file:delete(?PACKAGE_DB),
  file:delete(?TASKS_DB),
  meck:unload(),
  Config.

end_per_suite(Config) ->
  Config.

%% Package added normally - should not be postponed
test_normal_pass(_) ->
  ct:pal("------------------~p------------------~n", [test_normal_pass]),
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ?assert(oc_namespace_limiter:check_package(<<"baz/bar">>)),
  ets:insert(namespace_storage, {<<"foo/bar">>, oc_utils:now_to_timestamp() - 60001}), % pretend minute passed
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ok.

%% Package added too quickly. Should be postponed
test_postpone(_) ->
  ct:pal("------------------~p------------------~n", [test_postpone]),
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ets:insert(namespace_storage, {<<"foo/bar">>, oc_utils:now_to_timestamp() - 60001}), % pretend minute passed
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ets:insert(namespace_storage, {<<"foo/bar">>, oc_utils:now_to_timestamp() - 60001}), % pretend minute passed
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ?assertNot(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ?assertNot(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ok.

test_pass_after_postpone(_) ->
  ct:pal("------------------~p------------------~n", [test_pass_after_postpone]),
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ets:insert(namespace_storage, {<<"foo/bar">>, oc_utils:now_to_timestamp() - 60001}), % pretend minute passed
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ets:insert(namespace_storage, {<<"foo/bar">>, oc_utils:now_to_timestamp() - 60001}), % pretend minute passed
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ?assertNot(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ?assertNot(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ets:insert(namespace_storage, {<<"foo/bar">>, oc_utils:now_to_timestamp() - 60001}), % pretend minute passed
  ?assert(oc_namespace_limiter:check_package(<<"foo/bar">>)),
  ok.