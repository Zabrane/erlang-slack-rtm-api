-module(slack_rtm).
-compile([{parse_transform, lager_transform}]).
-export([connect/1, connect/2, connect/3]).
-export([printer/1, printer_loop/1, printer_loop/0]).

-define(SLACK_RTM_START_URI, "https://slack.com/api/rtm.start").

-spec connect(binary() | list()) -> {ok, pid()} | {error, any()}.
connect(Token) ->
    connect(Token, record).

connect(Token, Mode) when is_binary(Token) ->
    connect(binary_to_list(Token), Mode, ?SLACK_RTM_START_URI).

connect(Token, Mode, Url) when is_binary(Token) ->
    connect(binary_to_list(Token), Mode, Url);
connect(Token, Mode, Url) when is_binary(Url) ->
    connect(Token, Mode, binary_to_list(Url));
connect(Token, Mode, Url) when is_list(Token) andalso is_list(Url) ->
    {ok, Pid} = supervisor:start_child(slack_rtm_sup, [self(), Url, Token, Mode]),
    true = link(Pid),
    {ok, Pid}.

printer_loop(Token) ->
    {ok, _} = connect(Token),
    printer_loop().

printer_loop() ->
    receive A -> io:format("~s~n", [io_lib_pretty:print(A)]) end,
    slack_rtm:printer_loop().

printer(Token) ->
    spawn(slack_rtm, printer_loop, [Token]).
