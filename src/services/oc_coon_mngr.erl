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
-spec init() -> ok | error.
init() ->
  Res = oc_utils:exec("coon -v", [sync, stdout, stderr]),
  case Res of
    {error, Err} ->
      Status = proplists:get_value(exit_status, Err),
      StdErr = proplists:get_value(stderr, Err),
      oc_logger:err("Calling coon error(~p): ~p", [Status, StdErr]),
      error;
    {ok, Reply} ->
      [Vsn] = proplists:get_value(stdout, Reply),
      oc_logger:info("Coon vsn ~s", [Vsn]),
      ok
  end.