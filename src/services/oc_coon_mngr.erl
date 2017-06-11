%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 13:08
%%%-------------------------------------------------------------------
-module(oc_coon_mngr).
-author("tihon").

%% API
-export([init/0]).

%% Check if coon presents.
%% TODO install coon if not.
init() ->
  Res = exec:run("coon -v", [sync, stdout, stderr]),
  case Res of
    {error, Err} ->
      Status = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stderr, Err),
      oc_logger:err("Calling coon error(~p): ~p~n", [Status, StdErr]),
      false;
    {ok, Reply} ->
      [Vsn] = proplists:get_value(stdout, Reply),
      oc_logger:info("Coon vsn ~s~n", [Vsn]),
      ok
  end.