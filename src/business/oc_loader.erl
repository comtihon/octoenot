%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2017 18:48
%%%-------------------------------------------------------------------
-module(oc_loader).
-author("tihon").

-include("oc_error.hrl").
-define(REF_NAME, <<"ref">>).
-define(REF_TYPE, <<"ref_type">>).
-define(REPO_INFO, <<"repository">>).

-define(CLONE_CMD, "git clone -b ~s ~s ~s").
-define(PACKAGE_CMD, "cd ~s; coon package").

%% API
-export([add_package/1]).

-spec add_package(map()) -> true.
add_package(#{?REF_NAME := Tag, ?REF_TYPE := <<"tag">>, ?REPO_INFO := Repo}) ->
  #{<<"clone_url">> := Url, <<"full_name">> := Name} = Repo,
  case oc_namespace_limiter:check_package(Name) of
    true ->
      Path = clone_package(binary_to_list(Name), Url, Tag),
      Package = build_package(Path),
      oc_artifactory_mngr:load_package(Name, Tag, Package);
    false ->
      % TODO save to disc and proceed build later?
      % TODO log this
      throw({error, ?REACH_NS_LIMIT})
  end;
add_package(_) ->  % make only tag support configurable?
  throw({error, ?TAG_ONLY}).


%% @private
-spec clone_package(string(), binary(), binary()) -> string().
clone_package(Name, Url, Tag) ->
  {ok, Dir} = application:get_env(octocoon, build_dir),
  Path = filename:join([Dir, Name]),
  Cmd = lists:flatten(io_lib:format(?CLONE_CMD, [Tag, Url, Path])),
  os:cmd("rm -Rf " ++ Path),
  io:format("run ~p~n", [Cmd]),
  try exec:run(Cmd, [sync, stderr]) of
    {ok, _} -> Path;
    {error, Err} ->
      clone_error(Cmd, Err)
  catch
    _:Err ->
      clone_error(Cmd, Err)
  end.

%% @private
-spec build_package(string()) -> string().
build_package(Path) ->
  Cmd = lists:flatten(io_lib:format(?PACKAGE_CMD, [Path])),
  io:format("run ~s~n", [Cmd]),
  try exec:run(Cmd, [sync, {stderr, stdout}, stdout]) of
    {ok, Res} ->
      io:format("out ~p~n", [Res]),
      Stdout = proplists:get_value(stdout, Res), % TODO send to email, save attempt to db, send to http response
      get_package_if_succeed(Stdout);
    {error, Err} ->
      clone_error(Cmd, Err)
  catch
    _:Err ->
      clone_error(Cmd, Err)
  end.

%% @private
clone_error(Cmd, Err) ->
  Code = proplists:get_value(exit_status, Err),
  [StdErr] = proplists:get_value(stderr, Err, [undefined]),
  io:format("~p failed (~p) with: ~n", [Cmd, Code]),
  io:format("~s~n", [StdErr]),
  throw({error, ?CLONE_FAILURE}).

%% @private
-spec get_package_if_succeed(list(binary())) -> string().
get_package_if_succeed(undefined) ->
   throw({error, ?BUILD_FAILURE});
get_package_if_succeed(Output) ->
  Filtered = lists:dropwhile(fun(L) -> string:str(binary_to_list(L), "create package") == 0 end, Output),
  case Filtered of
    [] ->
      io:format("~s~n", [Output]),
      throw({error, ?BUILD_FAILURE});
    [First | _] -> % normally it should be one
      PackPath = lists:last(string:tokens(binary_to_list(First), " ")),
      string:strip(PackPath, right, $\n)
  end.