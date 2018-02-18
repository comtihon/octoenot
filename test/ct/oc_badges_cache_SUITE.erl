%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2017 6:09 PM
%%%-------------------------------------------------------------------
-module(oc_badges_cache_SUITE).
-author("tihon").

-compile(export_all).

-include("oc_database.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PACKAGE_DB, filename:join([oc_utils:get_priv_dir(), atom_to_list(?PACKAGES_STORAGE) ++ ".db"])).
-define(TASKS_DB, filename:join([oc_utils:get_priv_dir(), atom_to_list(?TASKS_STORAGE) ++ ".db"])).

all() ->
  [
    test_lookup_and_remember
  ].

init_per_suite(Config) ->
  application:ensure_all_started(erlexec),
  Config.

init_per_testcase(_, Config) ->
  ok = filelib:ensure_dir(oc_utils:get_priv_dir()),
  ok = oc_sqlite_mngr:init(),
  {ok, _} = oc_badges_cache:start_link(),
  Config.

end_per_testcase(_, Config) ->
  file:delete(?PACKAGE_DB),
  file:delete(?TASKS_DB),
  meck:unload(),
  Config.

end_per_suite(Config) ->
  Config.

%% Build system should be remembered and cached
test_lookup_and_remember(_) ->
  ct:pal("------------------~p------------------~n", [test_lookup_and_remember]),
  meck:new(oc_github_mngr),
  meck:expect(oc_github_mngr, request_build_system, fun(_, _) -> <<"enot">> end),

  meck:new(oc_database_holder),
  meck:expect(oc_database_holder, get_info, fun(_, _) -> [] end),

  Badge = oc_badges_mngr:get_badge(<<"NS">>, <<"Package">>),
  ?assertEqual({<<"not found">>, <<"enot">>, <<"unknown">>}, Badge),
  timer:sleep(1),
  ?assertEqual([{<<"NS/Package">>, <<"enot">>}], ets:tab2list(oc_badges_cache)),

  Badge = oc_badges_mngr:get_badge(<<"NS">>, <<"Package">>),
  ?assertEqual(1, meck:num_calls(oc_github_mngr, request_build_system, '_')),

  ok.
