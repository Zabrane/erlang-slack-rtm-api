%% @private
-module(slack_rtm_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [{
      slack_rtm_client_sup,
      {slack_rtm_client_sup, start_link, []},
      transient, 5000, supervisor, [slack_rtm_client_sup]
    }],
	{ok, {{simple_one_for_one, 10, 10}, Procs}}.
