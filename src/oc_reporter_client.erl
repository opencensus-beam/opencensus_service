-module(oc_reporter_client).

-behaviour(gen_statem).

-export([start_link/0,
         report_spans/1]).

-export([init/1,
         callback_mode/0,
         disconnected/3,
         connected/3,
         handle_event/3,
         code_change/3,
         terminate/2]).

-record(data, {stream :: grpcbox_client:stream() | undefined}).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

report_spans(Spans) ->
    case gen_statem:call(?MODULE, get_stream) of
        {ok, Stream} ->
            _ = grpcbox_client:send(Stream, #{spans => Spans});
        _ ->
            {error, no_stream}
    end.

init([]) ->
    {ok, disconnected, #data{stream=undefined}}.

callback_mode() ->
    [state_functions, state_enter].

disconnected({call, _From}, get_stream, _Data) ->
    {keep_state_and_data, [{next_event, internal, connect}, postpone]};
disconnected(internal, connect, Data=#data{stream=undefined}) ->
    try oc_trace_client:export(ctx:new(), #{channel => opencensus_service}) of
        {ok, Stream} ->
            {next_state, connected, Data#data{stream=Stream}}
    catch
        _:_ ->
            {keep_state, Data#data{stream=undefined}}
    end;
disconnected(enter, _, _Data) ->
    keep_state_and_data;
disconnected(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

%% first message on the stream must be one with node information
connected(enter, _, #data{stream=Stream}) ->
    ServiceName = application:get_env(opencensus, service_name, <<"no-service-name">>),
    {ok, Hostname} = inet:gethostname(),
    {ok, ExporterVersion} = application:get_key(opencensus_service, vsn),
    {ok, CoreLibraryVersion} = application:get_key(opencensus, vsn),
    Timestamp = erlang:system_time(nanosecond),
    Node = #{identifier => #{host_name => Hostname,
                             pid => 0,
                             start_timestamp => #{nanos => Timestamp rem 1000000000,
                                                  seconds => Timestamp div 1000000000}},
             library_info => #{language => 'ERLANG',
                               exporter_version => ExporterVersion,
                               core_library_version => CoreLibraryVersion},
             service_info => #{name => ServiceName},
             attributes => #{}},

    _ = grpcbox_client:send(Stream, #{node => Node,
                                      spans => []}),
    keep_state_and_data;
connected({call, _From}, get_stream, Data=#data{stream=undefined}) ->
    {next_state, disconnected, Data, [postpone]};
connected({call, From}, get_stream, #data{stream=Stream}) ->
    {keep_state_and_data, [{reply, From, {ok, Stream}}]};
connected(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

%% client stream process went down
handle_event(info, {'DOWN', Ref, process, _, _}, Data=#data{stream=#{monitor_ref := Ref}}) ->
    {next_state, disconnected, Data#data{stream=undefined}};
handle_event(_M, _S, _Data) ->
    keep_state_and_data.

code_change(_, Data, _) ->
    {ok, Data}.

terminate(_, _) ->
    ok.
