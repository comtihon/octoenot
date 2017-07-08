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

-define(CLONE_CMD, "git clone ~s ~s --depth=1").
-define(GITHUB_URL, "https://github.com/~s/~s.git").

%% API
-export([request_build_system/2]).

-spec request_build_system(binary(), binary()) -> binary().
request_build_system(Namespace, PackageName) ->
  {ok, Dir} = application:get_env(octocoon, build_dir),
  Path = filename:join([Dir, Namespace, PackageName]),
  Url = lists:flatten(io_lib:format(?GITHUB_URL, [Namespace, PackageName])),
  Cmd = lists:flatten(io_lib:format(?CLONE_CMD, [Url, Path])),
  oc_logger:debug("run ~p", [Cmd]),
  try oc_utils:exec(Cmd, [sync, {stderr, stdout}, stdout]) of
    {ok, _} -> get_build_system(Path);
    {error, Err} ->
      Code = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stdout, Err, [undefined]),
      oc_logger:warn("~p failed (~p): ~s", [Cmd, Code, StdErr]),
      oc_metrics_mngr:clone_error(),
      <<"undefined">>
  catch
    _:Err ->
      oc_logger:warn("~p failed (~p)", [Cmd, Err]),
      oc_metrics_mngr:clone_error(),
      <<"undefined">>
  after
    os:cmd("rm -Rf " ++ binary_to_list(Path))
  end.


%% @private
get_build_system(Path) ->
  {ok, Names} = file:list_dir(Path),
  IsCoon = lists:member("coonfig.json", Names),
  IsRebar = lists:member("rebar.config", Names),
  IsErlangMk = lists:member("erlang.mk", Names),
  BSList = [{IsCoon, <<"coon">>}, {IsRebar, <<"rebar">>}, {IsErlangMk, <<"erlang.mk">>}],
  proplists:get_value(true, BSList, <<"undefined">>).