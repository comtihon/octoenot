%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2017 16:11
%%%-------------------------------------------------------------------
-module(oc_metrics_mngr).
-author("tihon").

-include("oc_metrics.hrl").

%% API
-export([init/0]).
-export([retrieve_and_flush/0]).
-export([clone_request/0,
  clone_error/0,
  build_request/0,
  build_error/0,
  build_success/0,
  load_success/0,
  load_error/0]).

init() ->
  ok = folsom_metrics:new_counter(?CLONE),
  ok = folsom_metrics:new_counter(?CLONE_ERROR),
  ok = folsom_metrics:new_counter(?BUILD),
  ok = folsom_metrics:new_counter(?BUILD_ERROR),
  ok = folsom_metrics:new_counter(?BUILD_SUCCESS),
  ok = folsom_metrics:new_counter(?LOAD),
  ok = folsom_metrics:new_counter(?LOAD_ERROR),
  ok.

clone_request() ->
  folsom_metrics:notify({?CLONE, {inc, 1}}).

clone_error() ->
  folsom_metrics:notify({?CLONE_ERROR, {inc, 1}}).

build_request() ->
  folsom_metrics:notify({?BUILD, {inc, 1}}).

build_error() ->
  folsom_metrics:notify({?BUILD_ERROR, {inc, 1}}).

build_success() ->
  folsom_metrics:notify({?BUILD_SUCCESS, {inc, 1}}).

load_success() ->
  folsom_metrics:notify({?LOAD, {inc, 1}}).

load_error() ->
  folsom_metrics:notify({?LOAD_ERROR, {inc, 1}}).

-spec retrieve_and_flush() -> map().
retrieve_and_flush() ->
  All = folsom_metrics:get_metrics(),
  User = lists:foldl(fun flush/2, #{}, All),
  System = maps:from_list(folsom_vm_metrics:get_memory()),
  maps:merge(User, System).


%% @private
flush(Name, Acc) ->
  Value = folsom_metrics:get_metric_value(Name),
  folsom_metrics:notify({Name, {dec, Value}}),
  Acc#{Name => Value}.
