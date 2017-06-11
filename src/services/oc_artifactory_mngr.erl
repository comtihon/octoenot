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

%% API
-export([load_package/3, init/0]).

-define(CALL_TIMEOUT_MS, 5000).
-define(LOAD_CMD, "curl -u ~s:~s -X PUT ~s -T ~s").
-define(PING_PATH, "api/system/ping").
-define(REPO_INFO_PATH, "api/storage").

-spec init() -> ok.
init() ->
  {ok, Conf} = application:get_env(octocoon, artifactory),
  #{repo := Repo, host := Host} = Conf,
  true = is_artifactory_available(Host),
  true = is_repo_available(Host, Repo),
  ok.

-spec load_package(binary(), binary(), string()) -> boolean().
load_package(Name, Tag, Package) ->
  {ok, Conf} = application:get_env(octocoon, artifactory),
  #{user := User, password := Pass, repo := Repo, host := Host} = Conf,
  PackageName = get_package_name(Package),
  Path = get_package_url(Host, Repo, Name, Tag, PackageName),
  Cmd = lists:flatten(io_lib:format(?LOAD_CMD, [User, Pass, Path, Package])),
  io:format("run ~p~n", [Cmd]),
  A = os:cmd(Cmd), % TODO use erlang http client (or hackney lib)
  io:format("~p~n", [A]),
  true.


%% @private
is_artifactory_available(Host) ->
  Url = string:join([Host, ?PING_PATH], "/"),
  case httpc:request(get, {Url, []}, [{timeout, ?CALL_TIMEOUT_MS}], []) of
    {ok, {{_, 200, _}, _, "OK"}} -> true;
    Err ->
      io:format("Error accessing artifactory: ~p~n", [Err]),
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
      io:format("Error accessing artifactory's repo: ~p~n", [Err]),
      false
  end.

%% @private
get_package_url(Host, Repo, Name, Tag, Package) ->
  ErlangVsn = erlang:system_info(otp_release),
  string:join([Host, Repo, binary_to_list(Name), binary_to_list(Tag), ErlangVsn, Package], "/").

%% @private
get_package_name(PackagePath) ->
  lists:last(string:tokens(PackagePath, "/")).