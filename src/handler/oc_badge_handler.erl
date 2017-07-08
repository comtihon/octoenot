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
-define(BS_GET_TIMEOUT, 5000).

%% API
-export([init/2]).

init(Req0, State) ->
  Path = cowboy_req:path(Req0),
  oc_metrics_mngr:badge_request(),
  case binary:split(Path, <<"/">>, [global]) of
    [_, _, NS, Name] ->
      request_build_system_async(NS, Name),
      Info = oc_database_holder:get_info(NS, Name),
      Prepared = prepare_bagde(Info),
      Data = jsone:encode(Prepared),
      Req1 = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Data, Req0),
      {ok, Req1, State};
    _ ->
      oc_logger:warn("Wrong badge url ~p", [Path]),
      Req1 = cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, ?BAD_REQUEST, Req0),
      {ok, Req1, State}
  end.


%% @private
prepare_bagde([]) ->
  Bs = get_build_system(),
  #{?ERLANG => <<"not found">>, ?BUILD_SYSTEM => Bs, ?VERSION => <<"unknown">>};
prepare_bagde(Info) ->
  [{Highest, _} | _] = lists:sort(fun({KeyA, _}, {KeyB, _}) -> KeyA > KeyB end, Info),
  Erlang = get_erlang(Highest, Info),
  Bs = get_build_system(),
  #{?ERLANG => Erlang, ?BUILD_SYSTEM => Bs, ?VERSION => Highest}.

%% @private
% Get all Erlang versions by highest package version
get_erlang(Highest, Info) ->
  Filtered = lists:filter(fun({Vsn, _}) -> Vsn =:= Highest end, Info),
  lists:map(fun({_, Erl}) -> Erl end, Filtered).

%% @private
request_build_system_async(NS, Name) ->
  Self = self(),
  spawn(
    fun() ->
      Bs = oc_github_mngr:request_build_system(NS, Name),
      Self ! {build_system, Bs}
    end).

%% @private
get_build_system() ->
  receive
    {build_system, Bs} -> Bs
  after ?BS_GET_TIMEOUT -> <<"undefined">>
  end.