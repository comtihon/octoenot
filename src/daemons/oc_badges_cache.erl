%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Store build system data for badges to minimize git cloning.
%%% Stores data in ets. Invalidates cache periodically.
%%% @end
%%% Created : 14. Jul 2017 3:46 PM
%%%-------------------------------------------------------------------
-module(oc_badges_cache).
-author("tihon").

-behaviour(gen_server).

%% API
-export([start_link/0, remember/3, lookup/2]).

-include("oc_database.hrl").

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(BADGES_CACHE, ?MODULE).
-define(CLEAN_INTERVAL, timer:minutes(1)).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec remember(binary(), binary(), binary()) -> ok.
remember(Ns, Name, BuildSystem) ->
  FullName = <<Ns/binary, <<"/">>/binary, Name/binary>>,
  gen_server:cast(?MODULE, {remember, FullName, BuildSystem}).

-spec lookup(binary(), binary()) -> binary() | undefined.
lookup(Ns, Name) ->
  FullName = <<Ns/binary, <<"/">>/binary, Name/binary>>,
  case ets:lookup(?BADGES_CACHE, FullName) of
    [] -> undefined;
    [{_, Bs}] -> Bs
  end.

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
  ets:new(?BADGES_CACHE, [named_table, protected, {read_concurrency, true}]),
  erlang:send_after(?CLEAN_INTERVAL, self(), clean),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({remember, FullName, BuildSystem}, State) ->
  ets:insert(?BADGES_CACHE, {FullName, BuildSystem}),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(clean, State) ->
  ets:delete_all_objects(?BADGES_CACHE),
  erlang:send_after(?CLEAN_INTERVAL, self(), clean),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
