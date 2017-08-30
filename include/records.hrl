% Single-user presence change event
-record(slack_rtm_presence_change, {
          user :: binary(),
          presence :: atom() | binary()
}).

-record(slack_rtm_message, {
        channel :: binary(),
        user :: binary(),
        text :: binary(),
        ts :: binary(),
        source_team :: binary(),
        team :: binary()
}).

-record(slack_rtm_channel_marked, {
        channel :: binary(),
        ts :: binary(),
        event_ts :: binary(),
        unread_count :: pos_integer(),
        unread_count_display :: pos_integer(),
        num_mentions :: pos_integer(),
        num_mentions_display :: pos_integer(),
        mention_count :: pos_integer(),
        mention_count_display :: pos_integer()
}).

-record(slack_rtm_user_typing, {
          user :: binary(),
          channel :: binary()
}).

-record(slack_rtm_item, {
          type :: 'message' | 'file' | 'file_comment',
          channel :: binary(),
          ts :: binary(),
          file :: binary(),
          file_comment :: binary()
}).

-record(slack_rtm_reaction_added, {
        user :: binary(),
        ts :: binary(),
        event_ts :: binary(),
        reaction :: binary(),
        item :: #slack_rtm_item{},
        item_user :: binary()
}).

-record(slack_rtm_reaction_removed, {
        user :: binary(),
        ts :: binary(),
        event_ts :: binary(),
        reaction :: binary(),
        item :: #slack_rtm_item{},
        item_user :: binary()
}).

-record(slack_rtm_channel, {
          id :: binary(),
          is_channel :: boolean(),
          name :: binary(),
          name_normalized :: binary(),
          created :: pos_integer(),
          creator :: binary(),
          is_shared :: boolean(),
          is_org_shared :: boolean()
}).

-record(slack_rtm_channel_created, {
          channel :: #slack_rtm_channel{},
          event_ts :: binary()
}).

