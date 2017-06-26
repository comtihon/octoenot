%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jun 2017 11:41 AM
%%%-------------------------------------------------------------------
-module(oc_badge_handler).
-author("tihon").

%% API
-export([init/2]).

init(Req0, State) ->
  #{} = cowboy_req:match_qs([app], Req0),
  Data = jsone:encode([]),
  Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Data, Req0),
  {ok, Req1, State}.