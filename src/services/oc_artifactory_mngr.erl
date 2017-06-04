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
-export([load_package/3]).

-define(LOAD_CMD, "curl -u ~s:~s -X PUT ~s -T ~s").

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
get_package_url(Host, Repo, Name, Tag, Package) ->
  ErlangVsn = erlang:system_info(otp_release),
  filename:join([Host, Repo, Name, Tag, ErlangVsn, Package]).

%% @private
get_package_name(PackagePath) ->
  lists:last(string:tokens(PackagePath, "/")).