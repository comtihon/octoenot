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
-export([get_priv_dir/0, to_lower/1]).

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

