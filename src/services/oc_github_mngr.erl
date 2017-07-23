%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 17:59
%%%-------------------------------------------------------------------
-module(oc_github_mngr).
-author("tihon").

-define(GITHUB_URL, "https://github.com/~s/~s.git").

%% API
-export([request_build_system/2, get_signature/1]).

-spec request_build_system(binary(), binary()) -> binary().
request_build_system(Namespace, PackageName) ->
  {ok, Dir} = application:get_env(octocoon, build_dir),
  Path = filename:join([Dir, <<"ensure">>, Namespace, PackageName]),
  Url = lists:flatten(io_lib:format(?GITHUB_URL, [Namespace, PackageName])),
  try oc_git_mngr:clone_with_depth_1(Url, Path) of
    true ->
      get_build_system(Path);
    false ->
      <<"undefined">>
  after
    os:cmd("rm -Rf " ++ binary_to_list(Path))
  end.

-spec get_signature(binary()) -> string().
get_signature(Body) ->
  {ok, #{secret := Secret}} = application:get_env(octocoon, github),
  Hmac = crypto:hmac(sha256, Secret, Body),
  oc_utils:base16(Hmac).


%% @private
get_build_system(Path) ->
  {ok, Names} = file:list_dir(Path),
  IsCoon = lists:member("coonfig.json", Names),
  IsRebar = lists:member("rebar.config", Names),
  IsErlangMk = lists:member("erlang.mk", Names),
  BSList = [{IsCoon, <<"coon">>}, {IsRebar, <<"rebar">>}, {IsErlangMk, <<"erlang.mk">>}],
  proplists:get_value(true, BSList, <<"undefined">>).