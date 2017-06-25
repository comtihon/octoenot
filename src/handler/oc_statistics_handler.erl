%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2017 17:16
%%%-------------------------------------------------------------------
-module(oc_statistics_handler).
-author("tihon").

-include("oc_error.hrl").

%% API
-export([init/2]).

init(Req0, State) ->
  Statistics = oc_metrics_mngr:retrieve_and_flush(),
  Data = jsone:encode(Statistics),
  Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Data, Req0),
  {ok, Req1, State}.