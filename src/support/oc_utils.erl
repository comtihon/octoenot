%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 14:30
%%%-------------------------------------------------------------------
-module(oc_utils).
-author("tihon").

%% API
-export([
  get_priv_dir/0,
  to_lower/1,
  now_to_timestamp/0,
  ts_to_timestamp/1,
  exec/2]).

-spec get_priv_dir() -> string().
get_priv_dir() ->
  Path = case application:get_key(octocoon, vsn) of
           {ok, VSNString} ->
             "./lib/octocoon-" ++ VSNString;
           undefined ->
             {ok, Dir} = file:get_cwd(),
             Dir
         end,
  Path ++ "/priv/".

-spec to_lower(binary() | string()) -> binary() | string().
to_lower(Bin) when is_binary(Bin) ->
  list_to_binary(to_lower(binary_to_list(Bin)));
to_lower(List) -> string:to_lower(List).

-spec now_to_timestamp() -> integer().
now_to_timestamp() ->
  ts_to_timestamp(os:timestamp()).

-spec ts_to_timestamp(erlang:timestamp()) -> integer().
ts_to_timestamp({MegaSecs, Secs, MicroSecs}) ->
  (MegaSecs * 1000000 + Secs) * 1000 + MicroSecs div 1000.

%% none, compile, {crash,lint_module... while mocking exec
-spec exec(string(), list()) ->
  {ok, pid(), pid()} | {ok, [{stdout | stderr, [binary()]}]} | {error, any()}.
exec(Cmd, Args) ->
  exec:run(Cmd, Args).