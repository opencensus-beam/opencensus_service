%%%-------------------------------------------------------------------
%% @doc
%% @end
%%%-------------------------------------------------------------------

-module(oc_service_client_sup).

-behaviour(supervisor).

-export([start_link/5]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Name, ChannelName, Endpoints, Options, SupFlags) ->
    supervisor:start_link(?MODULE, [Name, ChannelName, Endpoints, Options, SupFlags]).

init([Name, ChannelName, Endpoints, Options, SupFlags]) ->
    ChannelSpec = grpcbox_channel_sup:channel_spec(ChannelName, Endpoints, Options),
    ClientSpec = #{id => oc_reporter_client,
                   start => {oc_reporter_client, start_link, [Name, ChannelName]}},
    {ok, {SupFlags, [ChannelSpec, ClientSpec]}}.
