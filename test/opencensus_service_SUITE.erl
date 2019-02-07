%%% ---------------------------------------------------------------------------
%%% @doc
%%% @end
%%% ---------------------------------------------------------------------------
-module(opencensus_service_SUITE).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-include_lib("opencensus/include/opencensus.hrl").

-ifdef(OTP_RELEASE).
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-endif.

all() ->
    [round_trip].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(opencensus),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(opencensus),
    application:unload(opencensus).

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

round_trip(_Config) ->
    %% with defaults for optional fields
    SpanName = <<"round-trip-span-1">>,
    Span = #span{name=SpanName,
                 trace_id=30880772589364415633133741367674805915,
                 span_id=8257235887736323202,
                 start_time={-576460693321990995, 2091207418848923700},
                 end_time={-576460679697900156, 2091207418848923700}},
    PbSpan = oc_reporter_service:to_proto_map(Span),
    Proto = trace_service_pb:encode_msg(PbSpan, span),

    PbSpan1 = maps:filter(fun(_, V) -> V =/= undefined end, PbSpan),
    ?assertEqual(trace_service_pb:decode_msg(Proto, span), PbSpan1),

    %% in case something changes in the stacktrace format use an actual
    %% result of get_stacktrace instead of hardcoding
    StackTrace = try
                     throw(x)
                 catch
                     ?WITH_STACKTRACE(_, _, Stacktrace)
                         Stacktrace
                 end,

    %% set all optional fields
    SpanName2 = <<"round-trip-span-2">>,
    Span2 = #span{name=SpanName2,
                  trace_id=30880772589364415633133741367674805915,
                  span_id=8257235887736323202,
                  parent_span_id=7257235887736323202,
                  start_time={-576460693321990995, 2091207418848923700},
                  end_time={-576460679697900156, 2091207418848923700},
                  attributes=#{<<"attr-1">> => 1,
                               <<"attr-2">> => true,
                               <<"attr-3">> => <<"value">>},
                  stack_trace=StackTrace,
                  links=[#link{type='CHILD_LINKED_SPAN',
                               trace_id=40880772589364415633133741367674805915,
                               span_id=50880772589364415633133741367674805915,
                               attributes = #{<<"link-attr-1">> => <<"link-attr-value">>}}],
                  time_events=[{{-576460679697900156, 2091207418848923700},
                                #annotation{description = <<"annotation description">>,
                                            attributes = #{<<"anno-attr-1">> => false}}},
                               {{-576460693321990995, 2091207418848923700},
                                #message_event{type = 'SENT',
                                               id=101,
                                               uncompressed_size=304,
                                               compressed_size=134}}],
                  status=#status{code=100,
                                 message = <<"some status message">>},
                  same_process_as_parent_span=false,
                  child_span_count=3},
    PbSpan2 = oc_reporter_service:to_proto_map(Span2),
    Proto2 = trace_service_pb:encode_msg(PbSpan2, span),

    PbSpan3 = maps:filter(fun(_, V) -> V =/= undefined end, PbSpan2),
    ?assertEqual(trace_service_pb:decode_msg(Proto2, span), PbSpan3),

    ok.
