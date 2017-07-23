%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2017 17:53
%%%-------------------------------------------------------------------
-module(oc_callback_handler).
-author("tihon").

-include("oc_error.hrl").

%% API
-export([init/2]).

init(Req0, State) ->
  AllHeaders = cowboy_req:headers(Req0),
  {ok, Data, Req} = cowboy_req:read_body(Req0),
  Req1 = act_callback(AllHeaders, Data, Req),
  {ok, Req1, State}.


%% @private
act_callback(Headers = #{<<"x-github-event">> := <<"create">>, <<"x-hub-signature">> := Hash}, Body, Req0) ->
  oc_logger:info("Headers ~p", [Headers]),
  case check_hash(Hash, Body) of
    true -> process_create_event(Body, Req0);
    false -> cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, ?WRONG_SIGNATURE, Req0)
  end;
act_callback(#{<<"accept">> := _}, _Body, Req0) ->
  cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req0);
act_callback(Headers, _Body, Req0) ->
  oc_logger:warn("Unknown callback: ~p", [Headers]),
  cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, ?UNSUPPORTED_TYPE, Req0).

%% @private
select_package(#{<<"payload">> := Package}) -> Package;  % in webhook data is wrapped in a payload
select_package(Package) -> Package.  % in github app data is plain

%% @private
check_hash(Hash, Body) ->
  Calculated = oc_github_mngr:get_signature(Body),
  binary_to_list(Hash) =:= "sha1=" ++ Calculated.

%% @private
process_create_event(Body, Req) ->
  Decoded = jsone:decode(Body, [{object_format, map}]),
  try oc_loader_mngr:add_package(select_package(Decoded)) of
    true ->
      cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req)
  catch
    throw:{error, Desc} ->
      cowboy_req:reply(500, #{<<"content-type">> => <<"text/plain">>}, Desc, Req)
  end.