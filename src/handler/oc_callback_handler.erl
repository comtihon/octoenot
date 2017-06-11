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
  {ok, Data, Req} = cowboy_req:read_urlencoded_body(Req0),
  Req1 = act_callback(AllHeaders, Data, Req),
  {ok, Req1, State}.


%% @private
act_callback(#{<<"x-github-event">> := <<"create">>}, Body, Req0) ->
  Payload = proplists:get_value(<<"payload">>, Body),
  Decoded = jsone:decode(Payload, [{object_format, map}]),
  try oc_loader_mngr:add_package(Decoded) of
    true ->
      cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"OK">>, Req0)
  catch
    throw:{error, Desc} ->
      cowboy_req:reply(500, #{<<"content-type">> => <<"text/plain">>}, Desc, Req0)
  end;
act_callback(Headers, _Body, Req0) ->
  oc_logger:warn("Unknown callback: ~p", [Headers]),
  cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain">>}, ?UNSUPPORTED_TYPE, Req0).