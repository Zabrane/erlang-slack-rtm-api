% Single-user presence change event
-record(slack_rtm_presence_change, {
          user :: binary(),
          presence :: atom() | binary()
}).

-record(slack_rtm_message_attachment_field, {
        title :: binary(),
        value :: binary(),
        short :: boolean()
}).

-record(slack_rtm_message_attachment, {
        fallback :: binary(),
        id :: pos_integer(),
        color :: binary(),
        fields :: [#slack_rtm_message_attachment_field{}],
        mrkdwn_in :: [binary()]
}).

-record(slack_rtm_message, {
        channel :: binary(),
        user :: binary(),
        text :: binary(),
        ts :: binary(),
        event_ts :: binary(),
        source_team :: binary(),
        team :: binary(),
        subtype :: binary(),
        bot_id :: binary(),
        attachments :: [#slack_rtm_message_attachment{}],
        hidden :: boolean(),
        message :: #slack_rtm_message{},
        previous_message :: #slack_rtm_message{}
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

-record(slack_rtm_subscription, {
          type :: 'thread',
          channel :: binary(),
          thread_ts :: binary(),
          date_create :: binary(),
          active :: boolean(),
          last_read :: binary(),
          unread_count :: pos_integer()
}).

-record(slack_rtm_thread_marked, {
          event_ts :: binary(),
          subscription :: #slack_rtm_subscription{}
}).

-record(slack_rtm_desktop_notification, {
          title :: binary(),
          subtitle :: binary(),
          msg :: binary(),
          content :: binary(),
          channel :: binary(),
          launch_uri :: binary(),
          avatar_image :: binary(),
          ssb_filename :: binary(),
          image_uri :: binary(),
          is_shared :: boolean(),
          event_ts :: boolean()
}).

-record(slack_rtm_member_joined_channel, {
          user :: binary(),
          channel :: binary(),
          channel_type :: binary(),
          event_ts :: binary(),
          ts :: binary()
}).

-record(slack_rtm_im_marked, {
          channel :: binary(),
          ts :: binary(),
          event_ts :: binary(),
          dm_count :: pos_integer(),
          unread_count_display :: pos_integer(),
          num_mentions_display :: pos_integer(),
          mention_count_display :: pos_integer()
}).

-record(slack_rtm_user_profile, {
          first_name :: binary(),
          last_name :: binary(),
          image_24 :: binary(),
          image_32 :: binary(),
          image_48 :: binary(),
          image_72 :: binary(),
          image_192 :: binary(),
          image_512 :: binary(),
          image_1024 :: binary(),
          image_original :: binary(),
          title :: binary(),
          skype :: binary(),
          phone :: binary(),
          avatar_hash :: binary(),
          status_text :: binary(),
          status_emoji :: binary(),
          real_name :: binary(),
          real_name_normalized :: binary(),
          email :: binary(),
          team :: binary()
}).

-record(slack_rtm_user, {
          id :: binary(),
          team_id :: binary(),
          name :: binary(),
          deleted :: boolean(),
          color :: binary(),
          real_name :: binary(),
          tz :: binary(),
          tz_label :: binary(),
          tz_offset :: integer(),
          profile :: #slack_rtm_user_profile{},
          is_admin :: boolean(),
          is_owner :: boolean(),
          is_primary_owner :: boolean(),
          is_restricted :: boolean(),
          is_ultra_restricted :: boolean(),
          is_bot :: boolean(),
          updated :: pos_integer(),
          is_app_user :: boolean()
}).

-record(slack_rtm_user_change, {
          event_ts :: binary(),
          cache_ts :: binary(),
          user :: #slack_rtm_user{}
}).

-record(slack_rtm_unknown_datagram, {
          type :: binary(),
          data :: any()
}).
