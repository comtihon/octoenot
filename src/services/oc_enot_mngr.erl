%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 13:08
%%%-------------------------------------------------------------------
-module(oc_enot_mngr).
-author("tihon").

%% API
-export([init/0]).

%% Check if enot presents.
%% TODO install enot if not.
-spec init() -> ok | error.
init() ->
  Res = oc_utils:exec("enot -v", [sync, stdout, stderr]),
  case Res of
    {error, Err} ->
      Status = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stderr, Err),
      oc_logger:err("Calling enot error(~p): ~p", [Status, StdErr]),
      error;
    {ok, Reply} ->
      [Vsn] = proplists:get_value(stdout, Reply),
      oc_logger:info("enot vsn ~s", [Vsn]),
      ok
  end.