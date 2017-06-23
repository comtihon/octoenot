%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Jun 2017 20:17
%%%-------------------------------------------------------------------
-module(oc_conf_holder).
-author("tihon").

-behaviour(gen_server).

%% API
-export([start_link/0, get_kerl_installation/1, get_kerl_executable/0, get_system_erl/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(CONF_ETS, oc_conf).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_kerl_installation(string()) -> {ok, string()} | undefined .
get_kerl_installation(Erl) ->
  case ets:lookup(?CONF_ETS, Erl) of
    [] -> undefined;
    [{_, Path}] -> {ok, Path}
  end.

-spec get_kerl_executable() -> string().
get_kerl_executable() ->
  ets:lookup_element(?CONF_ETS, kerl, 2).

-spec get_system_erl() -> string().
get_system_erl() ->
  ets:lookup_element(?CONF_ETS, erl, 2).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  ets:new(?CONF_ETS, [named_table, protected, {read_concurrency, true}]),
  SystemErl = oc_erlang_mngr:erlang_version(),
  {ok, KerlPath} = oc_erlang_mngr:ensure_kerl(),
  {ok, KerlInstallations} = oc_erlang_mngr:kerl_installations(KerlPath),
  save_erl(SystemErl, KerlPath, KerlInstallations),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
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
save_erl(Default, KerlPath, Installations) ->
  ets:insert(?CONF_ETS, {erl, Default}),
  ets:insert(?CONF_ETS, {kerl, KerlPath}),
  ets:insert(?CONF_ETS, Installations).