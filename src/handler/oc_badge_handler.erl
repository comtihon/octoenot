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

-define(BAD_REQUEST, <<"Wrong request format">>).
-define(ERLANG, <<"erlang">>).
-define(VERSION, <<"version">>).
-define(BUILD_SYSTEM, <<"build_system">>).

%% API
-export([init/2]).

init(Req0, State) ->
  Path = cowboy_req:path(Req0),
  oc_metrics_mngr:badge_request(),
  case binary:split(Path, <<"/">>, [global]) of
    [_, _, NS, Name] ->
      {Erlang, Bs, Vsn} = oc_badges_mngr:get_badge(NS, Name),
      Prepared = #{?ERLANG => Erlang, ?BUILD_SYSTEM => Bs, ?VERSION => Vsn},
      Data = jsone:encode(Prepared),
      Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Data, Req0),
      {ok, Req1, State};
    _ ->
      oc_logger:warn("Wrong badge url ~p", [Path]),
      Req1 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, ?BAD_REQUEST, Req0),
      {ok, Req1, State}
  end.

