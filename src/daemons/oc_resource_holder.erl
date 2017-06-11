%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 14:31
%%%-------------------------------------------------------------------
-module(oc_resource_holder).
-author("tihon").

-behaviour(gen_server).

%% API
-export([start_link/0, get_email_resource/1, get_email_resource/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).
-define(EMAIL_ETS, emails).
-define(DEFAULT_LANG, <<"en">>).
-define(RES_UPDATE_INTERVAL, 900000). %15 min
-define(EMAIL_DIR, "email").

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
-spec get_email_resource(binary()) -> undefined | string().
get_email_resource(ResourceName) -> get_email_resource(undefined, ResourceName).

%% Get html file's content for sending mails to user.
-spec get_email_resource(binary() | undefined, binary()) -> undefined | string().
get_email_resource(Lang, ResourceName) ->
  case ets:lookup(?EMAIL_ETS, {Lang, ResourceName}) of
    [] ->
      case ets:lookup(?EMAIL_ETS, {?DEFAULT_LANG, ResourceName}) of
        [] -> undefined;
        [{_, Content}] -> Content
      end;
    [{_, Content}] -> Content
  end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  ets:new(?EMAIL_ETS, [named_table, protected, {read_concurrency, true}, {write_concurrency, true}]),
  load_files(),
  erlang:send_after(?RES_UPDATE_INTERVAL, self(), update_res),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(update_res, State) ->  % update html files
  load_files(),
  erlang:send_after(?RES_UPDATE_INTERVAL, self(), update_res),
  {noreply, State, hibernate};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @private
load_files() ->
  Path = oc_utils:get_priv_dir() ++ ?EMAIL_DIR,
  case file:list_dir(Path) of
    {ok, []} -> io:format("No res files in path ~p", [Path]);
    {ok, Locales} ->
      load_locales(Path, Locales);
    Other ->
      io:format("Can't find res files in path ~p : ~p", [Path, Other])
  end.

%% @private
-spec load_locales(string(), list()) -> ok.
load_locales(Path, Locales) ->
  lists:foreach(
    fun(Locale) ->
      Full = Path ++ "/" ++ Locale,
      {ok, Files} = file:list_dir(Full),
      load_htmls(list_to_binary(Locale), Full ++ "/", Files)
    end, Locales).

%% @private
-spec load_htmls(binary(), string(), list()) -> ok.
load_htmls(Locale, Path, Files) ->
  lists:foreach(
    fun(File) ->
      case lists:suffix(".html", File) of
        true ->  % html file, load it
          {ok, Content} = file:read_file(Path ++ File),
          ets:insert(?EMAIL_ETS, {{oc_utils:to_lower(Locale), list_to_binary(File)}, binary_to_list(Content)});
        false -> ok  % not html file
      end
    end, Files).
