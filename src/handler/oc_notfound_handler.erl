%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2017 17:52
%%%-------------------------------------------------------------------
-module(oc_notfound_handler).
-author("tihon").

%% API
-export([init/2]).

init(Req0, State) ->
  Req = cowboy_req:reply(404,
    #{<<"content-type">> => <<"text/plain">>},
    <<"Not found :(">>,
    Req0),
  {ok, Req, State}.