%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2017 18:04
%%%-------------------------------------------------------------------
-module(oc_loader_logic).
-author("tihon").

-include("oc_enotfig.hrl").
-include("oc_error.hrl").

-define(PACKAGE_CMD, "enot package").

%% API
-export([check_config/3, build_package/2]).

-spec check_config(string(), boolean(), string()) -> list(string()).
check_config(Path, DisablePrebuild, DefaultErl) ->
  case file:read_file(filename:join(Path, "enotfig.json")) of
    {ok, ConfigBin} ->
      Config = jsone:decode(ConfigBin, [{object_format, map}]),
      SecureConfig = disable_prebuild(Config, DisablePrebuild),
      get_erl(SecureConfig, DefaultErl);
    {error, enoent} -> throw({error, ?NOT_A_enot})
  end.

%% Build repo and generate package. Return path to the package
-spec build_package(string(), string()) -> string().
build_package(Erl, Path) ->
  oc_logger:info("build package ~p ~p", [Erl, Path]),
  oc_metrics_mngr:build_request(),
  SystemErl = oc_conf_holder:get_system_erl(),
  Prefix = activate_erl_cmd(Erl, SystemErl),
  oc_logger:debug("run ~s", [Prefix ++ ?PACKAGE_CMD]),
  try oc_utils:exec(?PACKAGE_CMD, [sync, {stderr, stdout}, stdout, {cd, Path}]) of
    {ok, Res} ->
      Stdout = proplists:get_value(stdout, Res),
      oc_metrics_mngr:build_success(),
      get_package_if_succeed(Stdout);
    {error, Err} ->
      Code = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stdout, Err, [undefined]),
      oc_logger:warn("~p failed (~p) ~p", [?PACKAGE_CMD, Code, StdErr]),
      oc_metrics_mngr:build_error(),
      throw({error, ?BUILD_FAILURE, ["Erlang Vsn: ", Erl, StdErr]})
  catch
    _:Err ->
      oc_logger:warn("~p failed (~p)", [?PACKAGE_CMD, Err]),
      oc_metrics_mngr:build_error(),
      throw({error, ?BUILD_FAILURE})
  end.


%% @private
disable_prebuild(Config, false) -> Config;
disable_prebuild(Config = #{?DISABLE_PREBUILD := true, ?OVERRIDE_CONF := true}, _) ->
  Config;
disable_prebuild(Config, _) ->
  Config#{?DISABLE_PREBUILD => true, ?OVERRIDE_CONF => true}.

%% @private
get_erl(#{?ERL_RELEASES := Releases}, _) ->
  lists:map(fun binary_to_list/1, Releases);
get_erl(_, Default) ->
  [Default].

%% @private
activate_erl_cmd(SystemErl, SystemErl) -> "";
activate_erl_cmd(Erl, _) ->  % TODO handle undefined erl. Should make kerl download and install erl, then rerun this task
  {ok, Path} = oc_conf_holder:get_kerl_installation(Erl),
  ". " ++ Path ++ "/activate && ".

%% @private
-spec get_package_if_succeed(list(binary())) -> string().
get_package_if_succeed(undefined) ->
  throw({error, ?BUILD_FAILURE});
get_package_if_succeed(Output) ->
  Filtered = lists:dropwhile(fun(L) -> string:str(binary_to_list(L), "create package") == 0 end, Output),
  case Filtered of
    [] ->
      oc_logger:warn("~p", [Output]),
      throw({error, ?BUILD_FAILURE, Output});
    [First | _] -> % normally it should be one
      PackPath = lists:last(string:tokens(binary_to_list(First), " ")),
      string:strip(PackPath, right, $\n)
  end.
