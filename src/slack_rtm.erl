-module(slack_rtm).
-compile([{parse_transform, lager_transform}]).
-export([connect/1]).

-spec connect(binary() | list()) -> {ok, pid()} | {error, any()}.
connect(Token) when is_list(Token) ->
    connect(list_to_binary(Token));
connect(Token) when is_binary(Token) ->
    supervisor:start_child(slack_rtm_sup, [self(), Token]).
