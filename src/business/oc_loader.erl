%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Clone git project, build it, generate a package and load to remote
%%% @end
%%% Created : 11. Jun 2017 15:43
%%%-------------------------------------------------------------------
-module(oc_loader).
-author("tihon").

-include("oc_error.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, load_package_async/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(CLONE_CMD, "git clone -b ~s ~s ~s").
-define(PACKAGE_CMD, "cd ~s; coon package").
-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec load_package_async(pid(), binary(), binary(), binary()) -> ok.
load_package_async(Worker, Name, Url, Tag) ->
  gen_server:cast(Worker, {load, Name, Url, Tag}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(list()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Options) ->
  gen_server:start_link(?MODULE, Options, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({load, Name, Url, Tag}, State) ->
  Path = clone_package(binary_to_list(Name), Url, Tag),
  Package = build_package(Path),
  oc_artifactory_mngr:load_package(Name, Tag, Package),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
-spec clone_package(string(), binary(), binary()) -> string().
clone_package(Name, Url, Tag) ->
  {ok, Dir} = application:get_env(octocoon, build_dir),
  Path = filename:join([Dir, Name]),
  Cmd = lists:flatten(io_lib:format(?CLONE_CMD, [Tag, Url, Path])),
  os:cmd("rm -Rf " ++ Path),
  oc_logger:debug("run ~p", [Cmd]),
  try exec:run(Cmd, [sync, stderr]) of
    {ok, _} -> Path;
    {error, Err} ->
      Code = proplists:get_value(exit_status, Err),
      [StdErr] = proplists:get_value(stderr, Err, [undefined]),
      oc_logger:warn("~p failed (~p): ~s", [Cmd, Code, StdErr]),
      throw({error, ?CLONE_FAILURE})
  catch
    _:Err ->
      oc_logger:warn("~p failed (~p)", [Cmd, Err]),
      throw({error, ?CLONE_FAILURE})
  end.

%% @private
-spec build_package(string()) -> string().
build_package(Path) ->
  Cmd = lists:flatten(io_lib:format(?PACKAGE_CMD, [Path])),
  oc_logger:debug("run ~s", [Cmd]),
  try exec:run(Cmd, [sync, {stderr, stdout}, stdout]) of
    {ok, Res} ->
      Stdout = proplists:get_value(stdout, Res), % TODO send to email, save attempt to db, send to http response
      get_package_if_succeed(Stdout);
    {error, Err} ->
      Code = proplists:get_value(exit_status, Err),
      [StdErr] = proplists:get_value(stderr, Err, [undefined]),
      oc_logger:warn("~p failed (~p) ~p", [Cmd, Code, StdErr]),
      throw({error, ?BUILD_FAILURE})
  catch
    _:Err ->
      oc_logger:warn("~p failed (~p)", [Cmd, Err]),
      throw({error, ?BUILD_FAILURE})
  end.

%% @private
-spec get_package_if_succeed(list(binary())) -> string().
get_package_if_succeed(undefined) ->
  throw({error, ?BUILD_FAILURE});
get_package_if_succeed(Output) ->
  Filtered = lists:dropwhile(fun(L) -> string:str(binary_to_list(L), "create package") == 0 end, Output),
  case Filtered of
    [] ->
      oc_logger:warn("~p", [Output]),
      throw({error, ?BUILD_FAILURE});
    [First | _] -> % normally it should be one
      PackPath = lists:last(string:tokens(binary_to_list(First), " ")),
      string:strip(PackPath, right, $\n)
  end.