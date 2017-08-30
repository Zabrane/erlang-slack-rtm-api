-module(slack_rtm_client).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("include/records.hrl").

-define(SLACK_HOST, "slack.com").
-define(SLACK_RTM_START_URI, <<"/api/rtm.start">>).

-define(BASE_RECONNECT_COOLDOWN, 1000).
-define(MAX_RECONNECT_COOLDOWN, 60000).

%% Supervisor callback
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    slack_token :: binary(),
    callback :: pid(),
    reconnect_cooldown :: pos_integer(),
    gun :: any()
}).

start_link(Callback, SlackToken) ->
    gen_server:start_link(?MODULE, [Callback, SlackToken], []).

% Gen server callbacks

init([Callback, SlackToken]) ->
    State = #state{
       slack_token=SlackToken,
       callback=Callback,
       reconnect_cooldown=?BASE_RECONNECT_COOLDOWN
    },
    lager:info("Started archivist with token ~p~n", [SlackToken]),
    gen_server:cast(self(), reconnect),
    {ok, State}.

handle_call(Request, _From, State) ->
    lager:info("Unexpected call ~p~n", [Request]),
    {noreply, State}.

handle_cast(reconnect, State) ->
    % Reset the gun connection
    {ok, State1} = reconnect_websocket(State),
    {noreply, State1};
handle_cast(Msg, State) ->
    lager:info("Unexpected cast ~p~n", [Msg]),
    {noreply, State}.

handle_info({gun_ws, _Gun, Message}, State) ->
    handle_slack_ws_message(State, Message),
    {noreply, State};
handle_info({gun_down, Gun, _Proto, Reason, [], []}, State) ->
    lager:info("Gun down (reason: ~p)~n", [Reason]),
    gun:close(Gun),
    gen_server:cast(self(), reconnect),
    {noreply, State};
handle_info({gun_ws_upgrade, _Gun, ok, _Headers}, State) ->
    {noreply, State};
handle_info(Info, State) ->
    lager:info("Unexpected info ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(OldVsn, State, _Extra) ->
    lager:info("~p updated from vsn ~p", [?MODULE, OldVsn]),
    {ok, State}.

% Internal logic

reconnect_delayed(Time) ->
    Self = self(),
    timer:apply_after(Time, gen_server, cast, [Self, reconnect]).

increase_cooldown(Cooldown) ->
    NewCooldown = Cooldown * 2,
    case NewCooldown > ?MAX_RECONNECT_COOLDOWN of
        true -> ?MAX_RECONNECT_COOLDOWN;
        false -> NewCooldown
    end.

reconnect_websocket(State=#state{slack_token=Token}) ->
    {ok, RtmStartJson} = request_rtm_start(Token),
    case proplists:get_value(<<"ok">>, RtmStartJson) of
        false ->
            lager:info("Got bad start json: ~p~n", [RtmStartJson]),
            Cooldown = State#state.reconnect_cooldown,
            lager:info("Reconnecting in ~p ms~n", [Cooldown]),
            reconnect_delayed(Cooldown),
            {ok, State#state{
                reconnect_cooldown=increase_cooldown(Cooldown)
            }};
        true ->
            {<<"url">>, WsUrl}  = proplists:lookup(<<"url">>, RtmStartJson),
            {ok, Gun, _StreamRef} = connect_websocket(WsUrl),
            {ok, State#state{
                gun=Gun,
                reconnect_cooldown=?BASE_RECONNECT_COOLDOWN
            }}
    end.

connect_websocket(WsUri) ->
    {ok, {wss, [], Host, 443, Fragment, []}} = http_uri:parse(
        erlang:binary_to_list(WsUri),
        [{scheme_defaults, [{wss, 443}]}]
    ),
    {ok,Pid} = gun:open(Host, 443, #{protocols => [http]}),
    StreamRef = gun:ws_upgrade(Pid, Fragment),
    {ok, Pid, StreamRef}.

%% Send a request to the RTM start API endpoint
request_rtm_start(Token) ->
    {ok, Gun} = gun:open(?SLACK_HOST, 443, #{protocols => [http]}),
    Path = <<?SLACK_RTM_START_URI/binary, "?token=", Token/binary>>,
    StreamRef = gun:get(Gun, Path),
    Result = case gun:await(Gun, StreamRef) of
          {response, fin, _Status, _ResponseHeaders} ->
            {error, no_websocket};
          {response, nofin, Status, ResponseHeaders} ->
              {ok, ResponseBody} = gun:await_body(Gun, StreamRef),
              case Status of
                  200 ->
                      JsonBody = jsx:decode(ResponseBody),
                      {ok, JsonBody};
                  _ ->
                      {error, {Status, ResponseBody, ResponseHeaders}}
              end;
          {error, timeout} ->
                {error, timeout};
          Anything ->
              {error, Anything}
             end,
    gun:shutdown(Gun),
    Result.

%% Decode a JSON payload from slack, then call the appropriate handler
handle_slack_ws_message(State, {text, Json}) ->
    WsPayload = jsx:decode(Json),
    SlackRecord = parse_slack_payload(proplists:get_value(<<"type">>, WsPayload), WsPayload),
    case SlackRecord of
        undefined -> ok;
        _ ->
            State#state.callback ! {slack_msg, self(), SlackRecord}
    end.

parse_slack_channel(Payload) ->
    #slack_rtm_channel{
          id=proplists:get_value(<<"id">>, Payload),
          is_channel=proplists:get_value(<<"is_channel">>, Payload),
          name=proplists:get_value(<<"name">>, Payload),
          name_normalized=proplists:get_value(<<"name_normalized">>, Payload),
          created=proplists:get_value(<<"created">>, Payload),
          creator=proplists:get_value(<<"creator">>, Payload),
          is_shared=proplists:get_value(<<"is_shared">>, Payload),
          is_org_shared=proplists:get_value(<<"is_org_shared">>, Payload)
      }.

parse_slack_item(Payload) ->
    parse_slack_item(proplists:get_value(<<"type">>, Payload), Payload).
parse_slack_item(<<"message">>, Payload) ->
    #slack_rtm_item{
       type=message,
       channel=proplists:get_value(<<"channel">>, Payload),
       ts=proplists:get_value(<<"ts">>, Payload)
    };
parse_slack_item(<<"file">>, Payload) ->
    #slack_rtm_item{
       type=message,
       file=proplists:get_value(<<"file">>, Payload)
    };
parse_slack_item(<<"file_comment">>, Payload) ->
    #slack_rtm_item{
       type=file_comment,
       file=proplists:get_value(<<"file">>, Payload),
       file_comment=proplists:get_value(<<"file_comment">>, Payload)
    }.

parse_slack_message_attachment(Payload) ->
    #slack_rtm_message_attachment{
        id=proplists:get_value(<<"id">>, Payload),
        fallback=proplists:get_value(<<"fallback">>, Payload),
        color=proplists:get_value(<<"color">>, Payload),
        mrkdwn_in=proplists:get_value(<<"mrkdwn_in">>, Payload),
        fields=[
            parse_slack_message_attachment_field(F) ||
            F <- proplists:get_value(<<"fields">>, Payload, [])
        ]
    }.

parse_slack_message_attachment_field(Payload) ->
    #slack_rtm_message_attachment_field{
        title=proplists:get_value(<<"title">>, Payload),
        value=proplists:get_value(<<"value">>, Payload),
        short=proplists:get_value(<<"short">>, Payload)
    }.

parse_slack_subscription(Payload) ->
    parse_slack_subscription(proplists:get_value(<<"type">>, Payload), Payload).
parse_slack_subscription(<<"thread">>, Payload) ->
    #slack_rtm_subscription{
        type=thread,
        channel=proplists:get_value(<<"channel">>, Payload),
        thread_ts=proplists:get_value(<<"thread_ts">>, Payload),
        date_create=proplists:get_value(<<"date_create">>, Payload),
        active=proplists:get_value(<<"active">>, Payload),
        last_read=proplists:get_value(<<"last_read">>, Payload),
        unread_count=proplists:get_value(<<"unread_count">>, Payload)
    }.

parse_slack_payload(<<"reconnect_url">>, _Payload) ->
    undefined;
parse_slack_payload(<<"hello">>, _Payload) ->
    undefined;
parse_slack_payload(<<"presence_change">>, Payload) ->
    Presence = case proplists:get_value(<<"presence">>, Payload) of
        <<"active">> -> active;
        <<"away">> -> away;
        Other -> Other
    end,
    #slack_rtm_presence_change{
        user=proplists:get_value(<<"user">>, Payload),
        presence=Presence
    };
parse_slack_payload(<<"message">>, Payload) ->
    #slack_rtm_message{
        user=proplists:get_value(<<"user">>, Payload),
        channel=proplists:get_value(<<"channel">>, Payload),
        text=proplists:get_value(<<"text">>, Payload),
        ts=proplists:get_value(<<"ts">>, Payload),
        source_team=proplists:get_value(<<"source_team">>, Payload),
        team=proplists:get_value(<<"team">>, Payload),
        subtype=proplists:get_value(<<"subtype">>, Payload),
        bot_id=proplists:get_value(<<"bot_id">>, Payload),
        attachments=[
            parse_slack_message_attachment(A) ||
            A <- proplists:get_value(<<"attachments">>, Payload, [])
        ]
    };
parse_slack_payload(<<"channel_marked">>, Payload) ->
    #slack_rtm_channel_marked{
        channel=proplists:get_value(<<"channel">>, Payload),
        ts=proplists:get_value(<<"ts">>, Payload),
        event_ts=proplists:get_value(<<"event_ts">>, Payload),
        unread_count=proplists:get_value(<<"unread_count">>, Payload),
        unread_count_display=proplists:get_value(<<"unread_count_display">>, Payload),
        num_mentions=proplists:get_value(<<"num_mentions">>, Payload),
        num_mentions_display=proplists:get_value(<<"num_mentions_display">>, Payload),
        mention_count=proplists:get_value(<<"mention_count">>, Payload),
        mention_count_display=proplists:get_value(<<"mention_count_display">>, Payload)
    };
parse_slack_payload(<<"user_typing">>, Payload) ->
    #slack_rtm_user_typing{
        user=proplists:get_value(<<"user">>, Payload),
        channel=proplists:get_value(<<"channel">>, Payload)
    };
parse_slack_payload(<<"reaction_added">>, Payload) ->
    #slack_rtm_reaction_added{
        user=proplists:get_value(<<"user">>, Payload),
        ts=proplists:get_value(<<"ts">>, Payload),
        event_ts=proplists:get_value(<<"event_ts">>, Payload),
        reaction=proplists:get_value(<<"reaction">>, Payload),
        item_user=proplists:get_value(<<"item_user">>, Payload),
        item=parse_slack_item(proplists:get_value(<<"item">>, Payload))
    };
parse_slack_payload(<<"reaction_removed">>, Payload) ->
    #slack_rtm_reaction_removed{
        user=proplists:get_value(<<"user">>, Payload),
        ts=proplists:get_value(<<"ts">>, Payload),
        event_ts=proplists:get_value(<<"event_ts">>, Payload),
        reaction=proplists:get_value(<<"reaction">>, Payload),
        item_user=proplists:get_value(<<"item_user">>, Payload),
        item=parse_slack_item(proplists:get_value(<<"item">>, Payload))
    };
parse_slack_payload(<<"channel_created">>, Payload) ->
    #slack_rtm_channel_created{
        channel=parse_slack_channel(proplists:get_value(<<"channel">>, Payload)),
        event_ts=proplists:get_value(<<"event_ts">>, Payload)
    };
parse_slack_payload(<<"thread_marked">>, Payload) ->
    #slack_rtm_thread_marked{
        event_ts=proplists:get_value(<<"event_ts">>, Payload),
        subscription=parse_slack_subscription(proplists:get_value(<<"subscription">>, Payload))
    };
parse_slack_payload(<<"desktop_notification">>, Payload) ->
    #slack_rtm_desktop_notification{
        title=proplists:get_value(<<"title">>, Payload),
        subtitle=proplists:get_value(<<"subtitle">>, Payload),
        msg=proplists:get_value(<<"msg">>, Payload),
        event_ts=proplists:get_value(<<"event_ts">>, Payload),
        content=proplists:get_value(<<"content">>, Payload),
        channel=proplists:get_value(<<"channel">>, Payload),
        launch_uri=proplists:get_value(<<"launchUri">>, Payload),
        avatar_image=proplists:get_value(<<"avatarImage">>, Payload),
        ssb_filename=proplists:get_value(<<"ssbFilename">>, Payload),
        image_uri=proplists:get_value(<<"imageUri">>, Payload),
        is_shared=proplists:get_value(<<"is_shared">>, Payload)
    };
parse_slack_payload(<<"member_joined_channel">>, Payload) ->
    #slack_rtm_member_joined_channel{
        user=proplists:get_value(<<"user">>, Payload),
        channel=proplists:get_value(<<"channel">>, Payload),
        channel_type=proplists:get_value(<<"channel_type">>, Payload),
        ts=proplists:get_value(<<"ts">>, Payload),
        event_ts=proplists:get_value(<<"event_ts">>, Payload)
    };

parse_slack_payload(<<"im_marked">>, Payload) ->
    #slack_rtm_im_marked{
        channel=proplists:get_value(<<"channel">>, Payload),
        ts=proplists:get_value(<<"ts">>, Payload),
        event_ts=proplists:get_value(<<"event_ts">>, Payload),
        dm_count=proplists:get_value(<<"dm_count">>, Payload),
        unread_count_display=proplists:get_value(<<"unread_count_display">>, Payload),
        num_mentions_display=proplists:get_value(<<"num_mentions_display">>, Payload),
        mention_count_display=proplists:get_value(<<"mention_count_display">>, Payload)
    };
parse_slack_payload(Type, Payload) ->
    lager:info("Ignoring payload type ~p: ~p ~n", [Type, Payload]),
    undefined.
