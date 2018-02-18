%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2017 17:31
%%%-------------------------------------------------------------------
-module(oc_handler_mngr).
-author("tihon").

-define(LISTENER, oc_http_listener).
-define(STATISTICS, oc_statistics_listener).
-define(ROUTES(R), cowboy_router:compile([{'_', R}])).

%% API
-export([init/0]).

init() ->
  ok = start_callback(),
  ok = start_mertics().


%% @private
start_callback() ->
  {ok, Port} = application:get_env(octoenot, http_port),
  Dispatch = ?ROUTES(
    [
      {"/callback", oc_callback_handler, #{}},
      {"/badge/[...]", oc_badge_handler, #{}},
      {'_', oc_notfound_handler, #{}}
    ]),
  {ok, _} = cowboy:start_clear(?LISTENER, 100, [{port, Port}], #{env => #{dispatch => Dispatch}}),
  ok.

%% @private
start_mertics() ->
  {ok, #{port := Port}} = application:get_env(octoenot, mertics),
  Dispatch = ?ROUTES(
    [
      {"/statistics", oc_statistics_handler, #{}},
      {'_', oc_notfound_handler, #{}}
    ]),
  {ok, _} = cowboy:start_clear(?STATISTICS, 2, [{port, Port}], #{env => #{dispatch => Dispatch}}),
  ok.