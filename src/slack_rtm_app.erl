-module(slack_rtm_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    slack_rtm_sup:start_link().

stop(State) ->
    ok.
