%%%-------------------------------------------------------------------
%%% @author tihon
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%% Send email via gmail.com
%%% Credentials should be configured in config.
%%% @end
%%% Created : 11. Jun 2017 14:11
%%%-------------------------------------------------------------------
-module(oc_email_mngr).
-author("tihon").

-define(MESSAGE_SKEL, "Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\n~s").

%% API
-export([init/0, send_mail/3]).

%% Just check if credentials are configured
init() ->
  {ok, Conf} = application:get_env(octoenot, email),
  #{username := _, password := _} = Conf,
  ok.

-spec send_mail(string(), string(), string()) -> boolean().
send_mail(To, Subject, Message) ->
  {ok, Conf} = application:get_env(octoenot, email),
  #{username := From, password := Pass} = Conf,
  Msg = format_message(From, To, Subject, Message),
  Res = gen_smtp_client:send_blocking({From, [To], Msg},
    [{relay, "smtp.gmail.com"}, {username, From}, {password, Pass}, {ssl, true}, {auth, always}]),
  case is_binary(Res) of
    true -> true;
    false ->
      oc_logger:warn("Error sending email to ~p : ~p", [To, Res]),
      false
  end.


%% @private
format_message(From, To, Subject, Message) ->
  lists:flatten(io_lib:format(?MESSAGE_SKEL, [Subject, From, To, Message])).
