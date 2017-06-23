%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Jun 2017 18:48
%%%-------------------------------------------------------------------
-module(oc_loader_mngr).
-author("tihon").

-include("oc_loader.hrl").
-include("oc_error.hrl").
-define(REF_NAME, <<"ref">>).
-define(REF_TYPE, <<"ref_type">>).
-define(REPO_INFO, <<"repository">>).

%% API
-export([add_package/1, add_package/3]).

-spec add_package(map()) -> true.
add_package(#{?REF_NAME := Tag, ?REF_TYPE := <<"tag">>, ?REPO_INFO := Repo}) ->
  #{<<"clone_url">> := Url, <<"full_name">> := Name} = Repo,
  % TODO get user email
  case oc_namespace_limiter:check_package(Name) of
    true ->
      spawn(fun() -> add_package(Name, Url, Tag) end), % wait for compilation async, to release hook
      true;
    false ->
      oc_namespace_limiter:postpone_build(Name, Url, Tag),
      % TODO send build postponed email
      throw({error, ?REACH_NS_LIMIT})
  end;
add_package(_) ->  % make only tag support configurable?
  throw({error, ?TAG_ONLY}).

-spec add_package(binary(), binary(), binary()) -> ok.
add_package(Name, Url, Tag) ->
  poolboy:transaction(?BUILDER_POOL,
    fun(Worker) -> oc_loader:load_package(Worker, Name, Url, Tag) end,
    infinity),
  ok.