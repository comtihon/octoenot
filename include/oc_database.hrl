%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Jul 2017 14:38
%%%-------------------------------------------------------------------
-author("tihon").

-define(PACKAGES_STORAGE, coon_packages_db).
-define(PACKAGES_TABLE, packages).

-define(NAMESPACE_FIELD, namespace).
-define(PACKAGE_FIELD, package).
-define(VERSION_FIELD, vsn).
-define(ERLANG_FIELD, erl).

-define(PACKAGES_TABLE_SCHEMA,
  [
    {?NAMESPACE_FIELD, text},
    {?PACKAGE_FIELD, text},
    {?VERSION_FIELD, text},
    {?ERLANG_FIELD, text}
  ]).

-define(TASKS_STORAGE, coon_tasks_db).
-define(TASKS_TABLE, tasks).

-define(NAME_FIELD, name).
-define(URL_FIELD, url).
-define(TAG_FIELD, tag).

-define(TASKS_TABLE_SCHEMA, [{?NAME_FIELD, text}, {?URL_FIELD, text}, {?TAG_FIELD, text}]).
