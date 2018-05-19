-module(slack_rtm).
-compile([{parse_transform, lager_transform}]).
-export([connect/1, connect/2]).
-export([printer/1, printer_loop/1, printer_loop/0]).

-spec connect(binary() | list()) -> {ok, pid()} | {error, any()}.
connect(Token) ->
    connect(Token, record).
connect(Token, Mode) when is_list(Token) ->
    connect(list_to_binary(Token));
connect(Token, Mode) when is_binary(Token) ->
    supervisor:start_child(slack_rtm_sup, [self(), Token, Mode]).

printer_loop(Token) ->
    {ok, _} = connect(Token),
    printer_loop().

printer_loop() ->
    receive A -> io:format("~s~n", [io_lib_pretty:print(A)]) end,
    slack_rtm:printer_loop().

printer(Token) ->
    spawn(slack_rtm, printer_loop, [Token]).
