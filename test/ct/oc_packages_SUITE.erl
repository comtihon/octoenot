%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 17:13
%%%-------------------------------------------------------------------
-module(oc_packages_SUITE).
-author("tihon").

-compile(export_all).

-include("oc_database.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PACKAGE_DB, filename:join([oc_utils:get_priv_dir(), atom_to_list(?PACKAGES_STORAGE) ++ ".db"])).
-define(TASKS_DB, filename:join([oc_utils:get_priv_dir(), atom_to_list(?TASKS_STORAGE) ++ ".db"])).

all() ->
  [
    test_get_package,
    test_get_multiple_vsn,
    test_get_multiple_erl
  ].

init_per_suite(Config) ->
  application:ensure_all_started(erlexec),
  application:ensure_all_started(sqlite3),
  application:set_env(octocoon, build_dir, oc_utils:get_priv_dir()),
  Config.

init_per_testcase(_, Config) ->
  ok = filelib:ensure_dir(oc_utils:get_priv_dir()),
  ok = oc_sqlite_mngr:init(),
  Config.

end_per_testcase(_, Config) ->
  ok = file:delete(?PACKAGE_DB),
  ok = file:delete(?TASKS_DB),
  os:cmd("rm -rf " ++ oc_utils:get_priv_dir()),
  meck:unload(),
  Config.

end_per_suite(Config) ->
  Config.

%% Add package, get its info later
test_get_package(_) ->
  ct:pal("------------------~p------------------~n", [test_get_package]),
  {ok, _} = oc_database_holder:start_link(),
  ok = oc_database_holder:add_package(<<"comtihon/mongodb">>, <<"1.0.0">>, "19"),
  ok = oc_database_holder:add_package(<<"comtihon/bson">>, <<"1.1.0">>, "18"),
  Mongo = oc_database_holder:get_info(<<"comtihon">>, <<"mongodb">>),
  ?assertEqual([{<<"1.0.0">>, <<"19">>}], Mongo),
  Bson = oc_database_holder:get_info(<<"comtihon">>, <<"bson">>),
  ?assertEqual([{<<"1.1.0">>, <<"18">>}], Bson).

%% Add multiple package versions. Get info
test_get_multiple_vsn(_) ->
  ct:pal("------------------~p------------------~n", [test_get_multiple_vsn]),
  {ok, _} = oc_database_holder:start_link(),
  ok = oc_database_holder:add_package(<<"comtihon/mongodb">>, <<"1.0.0">>, "19"),
  ok = oc_database_holder:add_package(<<"comtihon/mongodb">>, <<"1.1.0">>, "19"),
  ok = oc_database_holder:add_package(<<"comtihon/mongodb">>, <<"2.0.0">>, "19"),
  Info = oc_database_holder:get_info(<<"comtihon">>, <<"mongodb">>),
  ?assertEqual([{<<"1.0.0">>, <<"19">>}, {<<"1.1.0">>, <<"19">>}, {<<"2.0.0">>, <<"19">>}], Info).

%% Add multiple erlang versions. Get info
test_get_multiple_erl(_) ->
  ct:pal("------------------~p------------------~n", [test_get_multiple_erl]),
  {ok, _} = oc_database_holder:start_link(),
  ok = oc_database_holder:add_package(<<"comtihon/mongodb">>, <<"1.0.0">>, "18"),
  ok = oc_database_holder:add_package(<<"comtihon/mongodb">>, <<"1.0.0">>, "19"),
  ok = oc_database_holder:add_package(<<"comtihon/mongodb">>, <<"1.0.0">>, "20"),
  Info = oc_database_holder:get_info(<<"comtihon">>, <<"mongodb">>),
  ?assertEqual([{<<"1.0.0">>, <<"18">>}, {<<"1.0.0">>, <<"19">>}, {<<"1.0.0">>, <<"20">>}], Info).

