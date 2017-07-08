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