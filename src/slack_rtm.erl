-module(slack_rtm).
-compile([{parse_transform, lager_transform}]).
-export([connect/1]).

connect(Token) when is_list(Token) ->
    connect(list_to_binary(Token));
connect(Token) when is_binary(Token) ->
    ok.
