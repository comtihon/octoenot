%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Jun 2017 18:55
%%%-------------------------------------------------------------------
-author("tihon").

-define(TASKS_STORAGE, coon_tasks_db).
-define(TASKS_TABLE, tasks).

-define(NAME_FIELD, name).
-define(URL_FIELD, url).
-define(TAG_FIELD, tag).

-define(TASKS_TABLE_SCHEMA, [{?NAME_FIELD, text}, {?URL_FIELD, text}, {?TAG_FIELD, text}]).
