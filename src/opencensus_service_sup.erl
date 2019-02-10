%%%-------------------------------------------------------------------
%% @doc opencensus_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(opencensus_service_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_child/5]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Name, ChannelName, Endpoints, Options, SupFlags) ->
    supervisor:start_child(?SERVER, [Name, ChannelName, Endpoints, Options, SupFlags]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 10},
    ChildSpec = #{id => oc_service_client_sup,
                  start => {oc_service_client_sup, start_link, []},
                  shutdown => 1000},
    {ok, {SupFlags, [ChildSpec]}}.
