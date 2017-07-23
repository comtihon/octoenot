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

-include("oc_coonfig.hrl").
-include("oc_error.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1, load_package/4]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {disable_prebuild :: boolean(), default_erl :: string()}).

%%%===================================================================
%%% API
%%%===================================================================
-spec load_package(pid(), binary(), binary(), binary()) -> ok.
load_package(Worker, Name, Url, Tag) ->
  oc_logger:info("load package ~p ~p ~p", [Name, Url, Tag]),
  gen_server:call(Worker, {load, Name, Url, Tag}, infinity).

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
  {ok, Disable} = application:get_env(octocoon, disable_prebuild),
  {ok, DefaultErl} = application:get_env(octocoon, default_erlang),
  {ok, #state{disable_prebuild = Disable, default_erl = DefaultErl}}.

handle_call({load, Name, Url, Tag}, _From, State = #state{disable_prebuild = Disable, default_erl = Default}) ->
  Path = oc_git_mngr:clone_repo(binary_to_list(Name), Url, Tag),
  try_build(Path, Name, Tag, Disable, Default),
  {reply, ok, State}.

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
try_build(undefined, Name, Tag, _, _) ->
  oc_logger:warn("Clone error for ~p:~p", [Name, Tag]);
try_build(Path, Name, Tag, Disable, Default) ->
  Email = oc_git_mngr:get_last_commit_email(Path),
  try
    Erls = oc_loader_logic:check_config(Path, Disable, Default),
    build_with_all_erl(Name, Tag, Path, Erls),
    notify_success(Email, Name, Tag, Erls)
  catch
    throw:{error, Error} ->
      notify_user(Email, Name, Tag, Error, undefined);
    throw:{error, Error, Msg} ->
      notify_user(Email, Name, Tag, Error, Msg)
  end.

%% @private
%% Build package with all erlang versions and load to remote repo
build_with_all_erl(Name, Tag, Path, Erls) ->
  oc_logger:info("erls ~p", [Erls]),
  lists:foreach(fun(Erl) -> build_with_erl(Erl, Path, Name, Tag) end, Erls),
  os:cmd("rm -Rf " ++ Path).  % remove project dir

%% @private
build_with_erl(Erl, Path, Name, Tag) ->
  VersionedPath = ensure_path(Path, Erl),
  try
    PackagePath = oc_loader_logic:build_package(Erl, VersionedPath),
    oc_artifactory_mngr:load_package(Name, Tag, PackagePath, Erl),
    oc_database_holder:add_package(Name, Tag, Erl)
  after
    os:cmd("rm -Rf " ++ VersionedPath)  % remove tmp vsn dir
  end.

%% @private
%% Copy cloned repo to repoErlVsn dir.
%% This should be done in order to force clean compilation for
%% multiple erlang vsns
ensure_path(Path, Erl) ->
  VersionedDir = Path ++ "_" ++ Erl,
  os:cmd("rm -Rf " ++ VersionedDir),
  ok = filelib:ensure_dir(VersionedDir),
  os:cmd("cp -r " ++ Path ++ " " ++ VersionedDir),
  VersionedDir.

%% @private
notify_user(Email, Name, Tag, Error, Msg) ->
  NameStr = binary_to_list(Name),
  case oc_resource_holder:get_email_resource(<<"build_failed">>) of
    undefined ->
      oc_logger:err("No email body for build_failed!");
    Body ->
      Filled = lists:flatten(io_lib:format(Body, [NameStr, Tag, Error, Msg])),
      oc_email_mngr:send_mail(Email, NameStr ++ " build failed", Filled)
  end.

%% @private
notify_success(Email, Name, Tag, Erlangs) ->
  NameStr = binary_to_list(Name),
  case oc_resource_holder:get_email_resource(<<"build_success">>) of
    undefined ->
      oc_logger:err("No email body for build_success!");
    Body ->
      Filled = lists:flatten(io_lib:format(Body, [NameStr, Tag, Erlangs])),
      oc_email_mngr:send_mail(Email, NameStr ++ " build succeed", Filled)
  end.