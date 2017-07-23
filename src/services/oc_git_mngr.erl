%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2017 1:53 PM
%%%-------------------------------------------------------------------
-module(oc_git_mngr).
-author("tihon").

%% API
-export([get_last_commit_email/1, clone_repo/3, clone_with_depth_1/2]).

-define(CLONE_CMD, "git clone -b ~s ~s ~s").
-define(CLONE_FAST_CMD, "git clone ~s ~s --depth=1").
-define(GET_COMMIT_DATA, "git log --format='%an <%ae>' -1").
-define(GET_EMAIL_REGEXP, "^(?:[^<]*)<([^>]*)").


-spec clone_repo(string(), binary(), binary()) -> string() | undefined.
clone_repo(FullName, Url, Tag) ->
  oc_logger:info("clone repo ~p ~p ~p", [FullName, Url, Tag]),
  oc_metrics_mngr:clone_request(),
  {ok, Dir} = application:get_env(octocoon, build_dir),
  Path = filename:join([Dir, FullName]),
  Cmd = lists:flatten(io_lib:format(?CLONE_CMD, [Tag, Url, Path])),
  os:cmd("rm -Rf " ++ Path),
  oc_logger:debug("run ~p", [Cmd]),
  try oc_utils:exec(Cmd, [sync, {stderr, stdout}, stdout]) of
    {ok, _} -> Path;
    {error, Err} ->
      Code = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stdout, Err, [undefined]),
      oc_logger:warn("~p failed (~p): ~s", [Cmd, Code, StdErr]),
      oc_metrics_mngr:clone_error(),
      undefined
  catch
    _:Err ->
      oc_logger:warn("~p failed (~p)", [Cmd, Err]),
      oc_metrics_mngr:clone_error(),
      undefined
  end.

-spec clone_with_depth_1(string() | binary(), binary()) -> boolean().
clone_with_depth_1(Url, Path) ->
  Cmd = lists:flatten(io_lib:format(?CLONE_FAST_CMD, [Url, Path])),
  oc_logger:debug("run ~p", [Cmd]),
  try oc_utils:exec(Cmd, [sync, {stderr, stdout}, stdout]) of
    {ok, _} -> true;
    {error, Err} ->
      Code = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stdout, Err, [undefined]),
      oc_logger:warn("~p failed (~p): ~s", [Cmd, Code, StdErr]),
      oc_metrics_mngr:clone_error(),
      false
  catch
    _:Err ->
      oc_logger:warn("~p failed (~p)", [Cmd, Err]),
      oc_metrics_mngr:clone_error(),
      false
  end.

-spec get_last_commit_email(string()) -> string() | undefined.
get_last_commit_email(Path) ->
  try oc_utils:exec(?GET_COMMIT_DATA, [sync, {stderr, stdout}, stdout, {cd, Path}]) of
    {ok, Res} ->
      case proplists:get_value(stdout, Res) of
        undefined -> undefined;
        [Value | _] -> extract_email(Value)
      end;
    {error, Err} ->
      Code = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stdout, Err, [undefined]),
      oc_logger:warn("get last commit for ~p failed with (~p): ~p", [Path, Code, StdErr]),
      undefined
  catch
    _:Err ->
      oc_logger:warn("get last commit for ~p failed with ~p", [Path, Err]),
      undefined
  end.


%% @private
extract_email(Email) ->
  EmailStr = binary_to_list(Email),
  Found = re:run(EmailStr, ?GET_EMAIL_REGEXP, [anchored, {capture, all_but_first}]),
  case Found of
    {match, [{Start, End}]} ->
      lists:sublist(EmailStr, Start + 1, End);
    _ ->
      oc_logger:warn("No email in a commit info: ~s", [EmailStr]),
      undefined
  end.