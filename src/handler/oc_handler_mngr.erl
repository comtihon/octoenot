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

%% API
-export([init/0]).

init() ->
  {ok, Port} = application:get_env(octocoon, http_port),
  Dispatch = cowboy_router:compile([
    {'_',
      [
        {"/callback", oc_callback_handler, #{}},  % TODO badge handler?
        {'_', oc_notfound_handler, #{}}
      ]
    }
  ]),
  {ok, _} = cowboy:start_clear(?LISTENER, 100, [{port, Port}], #{env => #{dispatch => Dispatch}}),
  ok.
