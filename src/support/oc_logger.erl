%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 12:30
%%%-------------------------------------------------------------------
-module(oc_logger).
-author("tihon").

-compile([{parse_transform, lager_transform}]).

%% API
-export([
  debug/1, debug/2,
  info/1, info/2,
  warn/1, warn/2,
  err/1, err/2]).

debug(M) -> debug(M, []).

debug(M, A) -> lager:debug(M, A).

info(M) -> info(M, []).

info(M, A) -> lager:info(M, A).

warn(M) -> warn(M, []).

warn(M, A) -> lager:warning(M, A).

err(M) -> err(M, []).

err(M, A) -> lager:error(M, A).