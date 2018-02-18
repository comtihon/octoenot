%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 15:44
%%%-------------------------------------------------------------------
-module(oc_loader_sup).
-author("tihon").

-include("oc_loader.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([]) ->
  Options = form_pool_conf(),
  WorkerOpts = [],
  {ok, {{one_for_one, 1000, 3600}, [poolboy:child_spec(?BUILDER_POOL, Options, WorkerOpts)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
form_pool_conf() ->
  {ok, #{max_overflow := Overflow, size := Size}} = application:get_env(octoenot, loader_pool),
  [
    {worker_module, oc_loader},
    {name, {local, ?BUILDER_POOL}},
    {max_overflow, Overflow},
    {size, Size}
  ].
