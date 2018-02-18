%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 19:00
%%%-------------------------------------------------------------------
-module(oc_build_system_SUITE).
-author("tihon").

-compile(export_all).

-include("oc_database.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
  [
    test_get_build_system
  ].

init_per_suite(Config) ->
  BuildDir = oc_utils:get_priv_dir(),
  application:set_env(octoenot, build_dir, BuildDir),
  [{build_dir, BuildDir} | Config].

init_per_testcase(_, Config) ->
  Config.

end_per_testcase(_, Config) ->
  os:cmd("rm -rf " ++ proplists:get_value(build_dir, Config)),
  meck:unload(),
  Config.

end_per_suite(Config) ->
  Config.

%% Test different build system checking
test_get_build_system(_) ->
  ct:pal("------------------~p------------------~n", [test_get_build_system]),
  meck:new(oc_utils, [passthrough]),
  meck:expect(oc_utils, exec, fun(_, _) -> {ok, []} end),
  {ok, Dir} = application:get_env(octoenot, build_dir),
%%  populate enot app
  PathC = filename:join([Dir, <<"ensure/ns/test_app_enot/">>]),
  os:cmd("mkdir -p " ++ binary_to_list(PathC)),
  file:write_file(filename:join([PathC, "enotfig.json"]), <<"">>),
  file:write_file(filename:join([PathC, "somefile"]), <<"">>),
  ?assertEqual(<<"enot">>, oc_github_mngr:request_build_system(<<"ns">>, <<"test_app_enot">>)),
%%  populate rebar app
  PathR = filename:join([Dir, <<"ensure/ns/test_app_rebar">>]),
  os:cmd("mkdir -p " ++ binary_to_list(PathR)),
  file:write_file(filename:join([PathR, "rebar.config"]), <<"">>),
  file:write_file(filename:join([PathR, "somefile"]), <<"">>),
  ?assertEqual(<<"rebar">>, oc_github_mngr:request_build_system(<<"ns">>, <<"test_app_rebar">>)),
%%  populate erlang.mk
  PathE = filename:join([Dir, <<"ensure/ns/test_app_emk">>]),
  os:cmd("mkdir -p " ++ binary_to_list(PathE)),
  file:write_file(filename:join([PathE, "erlang.mk"]), <<"">>),
  file:write_file(filename:join([PathE, "somefile"]), <<"">>),
  ?assertEqual(<<"erlang.mk">>, oc_github_mngr:request_build_system(<<"ns">>, <<"test_app_emk">>)),
  ok.