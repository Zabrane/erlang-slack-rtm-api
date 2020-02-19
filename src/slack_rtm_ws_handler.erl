-module(slack_rtm_ws_handler).
-behaviour(websocket_client_handler).

-export([
    init/2,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

-define(PING_RATE, 60000).

-record(state, {client_pid}).

init([ClientPid], _ConnState) ->
    {ok, #state{client_pid=ClientPid}, ?PING_RATE}.

websocket_handle({text, Message}, _ConnState, State) ->
    handle_payload(State, Message),
    {ok, State};
websocket_handle({ping, _}, _ConnState, State) ->
    {reply, pong, State};
websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};
websocket_handle(Payload, _ConnState, State) ->
    lager:info("[WS ~p] Unhandled payload: ~p~n", [self(), Payload]),
    {ok, State}.

websocket_info(start, _ConnState, State) ->
    handle_connect(State),
    {ok, State};
websocket_info(Info, _ConnState, State) ->
    lager:info("[WS ~p] info: ~p", [self(), Info]),
    {ok, State}.

websocket_terminate(Reason, _ConnState, State) ->
    lager:info("[WS ~p] terminate: ~p", [self(), Reason]),
    handle_disconnect(State, Reason),
    ok.

handle_connect(State) ->
    State#state.client_pid ! {ws_connected, self()}.

handle_payload(State, Message) ->
    State#state.client_pid ! {ws_message, self(), Message}.

handle_disconnect(State, Reason) ->
    State#state.client_pid ! {ws_down, self(), Reason}.
