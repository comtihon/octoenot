%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jul 2017 3:48 PM
%%%-------------------------------------------------------------------
-module(oc_badges_mngr).
-author("tihon").

-define(BS_GET_TIMEOUT, 5000).

%% API
-export([get_badge/2]).

-spec get_badge(binary(), binary()) -> tuple().
get_badge(NS, Name) ->
  MaybeBs = case oc_badges_cache:lookup(NS, Name) of
              undefined ->
                request_build_system_async(NS, Name);
              BuildSystem ->
                BuildSystem
            end,
  Info = oc_database_holder:get_info(NS, Name),
  prepare_bagde(Info, MaybeBs).


%% @private
prepare_bagde([], MaybeBs) ->
  Bs = get_build_system(MaybeBs),
  {<<"not found">>, Bs, <<"unknown">>};
prepare_bagde(Info, MaybeBs) ->
  [{Highest, _} | _] = lists:sort(fun({KeyA, _}, {KeyB, _}) -> KeyA > KeyB end, Info),
  Erlang = get_erlang(Highest, Info),
  Bs = get_build_system(MaybeBs),
  {Erlang, Bs, Highest}.

%% @private
% Get all Erlang versions by highest package version
get_erlang(Highest, Info) ->
  Filtered = lists:filter(fun({Vsn, _}) -> Vsn =:= Highest end, Info),
  lists:map(fun({_, Erl}) -> Erl end, Filtered).

%% @private
%% Clone remote git repo with depth1 and determine a build system.
%% Works only with github.
request_build_system_async(NS, Name) ->
  Self = self(),
  spawn(
    fun() ->
      Bs = oc_github_mngr:request_build_system(NS, Name),
      oc_badges_cache:remember(NS, Name, Bs),
      Self ! {build_system, Bs}
    end).

%% @private
get_build_system(MaybeBs) when is_binary(MaybeBs) -> MaybeBs;
get_build_system(_) ->
  receive
    {build_system, Bs} -> Bs
  after ?BS_GET_TIMEOUT -> <<"undefined">>
  end.