%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2017 17:57
%%%-------------------------------------------------------------------
-module(oc_building_SUITE).
-author("tihon").

-compile(export_all).

-include("oc_database.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PACKAGE_DB, filename:join([oc_utils:get_priv_dir(), atom_to_list(?PACKAGES_STORAGE) ++ ".db"])).
-define(TASKS_DB, filename:join([oc_utils:get_priv_dir(), atom_to_list(?TASKS_STORAGE) ++ ".db"])).

all() ->
  [
    test_build_default,
    test_build_several
  ].

init_per_suite(Config) ->
  application:ensure_all_started(erlexec),
  application:set_env(octocoon, disable_prebuild, true),
  application:set_env(octocoon, default_erlang, "18"),
  application:set_env(octocoon, loader_pool, #{max_overflow => 0, size => 1}),
  Config.

init_per_testcase(_, Config) ->
  ok = filelib:ensure_dir(oc_utils:get_priv_dir()),
  ok = oc_sqlite_mngr:init(),
  Config.

end_per_testcase(_, Config) ->
  file:delete(?PACKAGE_DB),
  file:delete(?TASKS_DB),
  meck:unload(),
  Config.

end_per_suite(Config) ->
  Config.

%% Request to build package without erl in coonfig.json - should build via default vsn
test_build_default(_) ->
  ct:pal("------------------~p------------------~n", [test_build_default]),
  {ok, _} = oc_loader_sup:start_link(),
  Self = self(),
  meck:new(oc_git_mngr),
  meck:expect(oc_git_mngr, clone_repo,
    fun(Name, Url, Tag) ->
      Self ! {clone, {Name, Url, Tag}},
      "ClonedRepoPath"
    end),
  meck:new(oc_loader_logic),
  meck:expect(oc_loader_logic, check_config, fun(_, _, _) -> ["18"] end),
  meck:expect(oc_loader_logic, build_package,
    fun(Erl, VersionedPath) ->
      Self ! {build, {Erl, VersionedPath}},
      "PackagePath"
    end),
  meck:expect(oc_artifactory_mngr, load_package,
    fun(Name, Tag, Erl, PackagePath) ->
      Self ! {load, {Name, Tag, Erl, PackagePath}},
      true
    end),
  ok = oc_loader_mngr:add_package(<<"ns/name">>, <<"some_git_url">>, <<"1.0.0">>),
  check_over_erl("18"),
  ok.

%% Request to build package with several erl versions
test_build_several(_) ->
  ct:pal("------------------~p------------------~n", [test_build_several]),
  {ok, _} = oc_loader_sup:start_link(),
  Self = self(),
  meck:new(oc_git_mngr),
  meck:expect(oc_git_mngr, clone_repo,
    fun(Name, Url, Tag) ->
      Self ! {clone, {Name, Url, Tag}},
      "ClonedRepoPath"
    end),
  meck:new(oc_loader_logic),
  meck:expect(oc_loader_logic, check_config, fun(_, _, _) -> ["18", "19", "20"] end),
  meck:expect(oc_loader_logic, build_package,
    fun(Erl, VersionedPath) ->
      Self ! {build, {Erl, VersionedPath}},
      "PackagePath"
    end),
  meck:expect(oc_artifactory_mngr, load_package,
    fun(Name, Tag, Erl, PackagePath) ->
      Self ! {load, {Name, Tag, Erl, PackagePath}},
      true
    end),
  ok = oc_loader_mngr:add_package(<<"ns/name">>, <<"some_git_url">>, <<"1.0.0">>),
  {clone, {"ns/name", <<"some_git_url">>, <<"1.0.0">>}} = get_step(clone),
  check_over_erl("18"),
  check_over_erl("19"),
  check_over_erl("20"),
  false = get_step(clone),  % only one clone should be for several builds
  ok.


%% @private
check_over_erl(Erl) ->
  ct:pal("check ~p", [Erl]),
  {build, {Erl, "ClonedRepoPath_" ++ Erl}} = get_step(build),
  {load, {<<"ns/name">>, <<"1.0.0">>, "PackagePath", Erl}} = get_step(load).

%% @private
get_step(Type) ->
  receive
    {Type, Step} -> {Type, Step}
  after
    100 -> false
  end.