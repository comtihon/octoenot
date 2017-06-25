%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2017 18:47
%%%-------------------------------------------------------------------
-module(oc_artifactory_mngr).
-author("tihon").

-include("oc_error.hrl").

%% API
-export([load_package/4, init/0]).

-define(CALL_TIMEOUT_MS, 5000).
-define(LOAD_CMD, "curl -u ~s:~s -X PUT ~s -T ~s").
-define(PING_PATH, "api/system/ping").
-define(REPO_INFO_PATH, "api/storage").

%% TODO install artifactory locally if not found?
-spec init() -> ok.
init() ->
  {ok, Conf} = application:get_env(octocoon, artifactory),
  #{repo := Repo, host := Host} = Conf,
  true = is_artifactory_available(Host),
  true = is_repo_available(Host, Repo),
  ok.

-spec load_package(binary(), binary(), string(), string()) -> true.
load_package(Name, Tag, PackagePath, Erl) ->
  {ok, Conf} = application:get_env(octocoon, artifactory),
  #{user := User, password := Pass, repo := Repo, host := Host} = Conf,
  PackageName = get_package_name(PackagePath),
  Path = get_package_url(Host, Repo, Name, Tag, Erl, PackageName),
  Cmd = lists:flatten(io_lib:format(?LOAD_CMD, [User, Pass, Path, PackagePath])),
  oc_logger:debug("run ~p", [Cmd]),  % TODO use erlang http client (or hackney lib)
  try exec:run(Cmd, [sync, {stderr, stdout}, stdout]) of
    {ok, _Res} -> true;
    {error, Err} ->
      Code = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stdout, Err, [undefined]),
      oc_logger:warn("~p failed (~p) ~p", [Cmd, Code, StdErr]),
      throw({error, ?LOAD_FAILURE})
  catch
    _:Err ->
      oc_logger:warn("~p failed (~p)", [Cmd, Err]),
      throw({error, ?LOAD_FAILURE})
  end.


%% @private
is_artifactory_available(Host) ->
  Url = string:join([Host, ?PING_PATH], "/"),
  case httpc:request(get, {Url, []}, [{timeout, ?CALL_TIMEOUT_MS}], []) of
    {ok, {{_, 200, _}, _, "OK"}} -> true;
    Err ->
      oc_logger:err("Error accessing artifactory: ~p", [Err]),
      false
  end.

%% @private
is_repo_available(Host, Repo) ->
  Url = string:join([Host, ?REPO_INFO_PATH, Repo], "/"),  % TODO use smarter join
  case httpc:request(get, {Url, []}, [{timeout, ?CALL_TIMEOUT_MS}], [{body_format, binary}]) of
    {ok, {{_, 200, _}, _, Json}} ->
      #{<<"repo">> := RepoBin} = jsone:decode(Json, [{object_format, map}]),
      RepoBin = list_to_binary(Repo),
      true;
    Err ->
      oc_logger:err("Error accessing artifactory's repo: ~p", [Err]),
      false
  end.

%% @private
get_package_url(Host, Repo, Name, Tag, Erl, Package) ->
  string:join([Host, Repo, binary_to_list(Name), binary_to_list(Tag), Erl, Package], "/").

%% @private
get_package_name(PackagePath) ->
  lists:last(string:tokens(PackagePath, "/")).