%%%-------------------------------------------------------------------
%% @doc opencensus_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(opencensus_service_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Name = application:get_env(opencensus_service, channel_name, opencensus_service),
    Endpoints = application:get_env(opencensus_service, endpoints, [{http, "localhost", 55678, []}]),
    Options = application:get_env(opencensus_service, channel_options, #{}),

    SupFlags = application:get_env(opencensus_service, sup_flags, #{strategy => one_for_one,
                                                                    intensity => 1,
                                                                    period => 5}),

    ChannelSpec = grpcbox_channel_sup:channel_spec(Name, Endpoints, Options),
    Client = #{id => oc_reporter_client,
               start => {oc_reporter_client, start_link, []}},
    {ok, {SupFlags, [ChannelSpec, Client]}}.
