%%%------------------------------------------------------------------------
%% Copyright 2019, OpenCensus Authors
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc Exports spans to Opencensus agent
%% @end
%%%------------------------------------------------------------------------

-module(oc_reporter_service).

-behaviour(oc_reporter).

-include_lib("opencensus/include/opencensus.hrl").

-export([init/1,
         report/2]).

-export([to_oc_proto/1]).

init(_Opts) ->
    %% in case this is called before the app has booted
    application:ensure_all_started(opencensus_service).

report(Spans, _) ->
    ProtoSpans = [to_oc_proto(Span) || Span <- Spans],
    oc_reporter_client:report_spans(ProtoSpans),
    ok.

to_oc_proto(#message_event{type=Type,
                            id=Id,
                            uncompressed_size=UncompressedSize,
                            compressed_size=CompressedSize}) ->
    #{type => Type,
      id => Id,
      uncompressed_size => UncompressedSize,
      compressed_size => CompressedSize};
to_oc_proto(#status{code=Code,
                     message=Message}) ->
    #{code => Code,
      message => Message};
to_oc_proto(#span{name=Name,
                   trace_id=TraceId,
                   span_id=SpanId,
                   parent_span_id=MaybeParentSpanId,
                   kind=Kind,
                   start_time=StartTime,
                   end_time=EndTime,
                   attributes=Attributes,
                   links=Links,
                   stack_trace=StackTrace,
                   time_events=TimeEvents,
                   status=Status,
                   same_process_as_parent_span=SameProcessAsParentSpan,
                   child_span_count=ChildSpanCount}) ->
    TraceIdBytes = <<TraceId:128>>,
    SpanIdBytes = <<SpanId:64>>,
    ParentSpanId = case MaybeParentSpanId of undefined -> <<>>; _ -> <<MaybeParentSpanId:64>> end,
    #{name => trunc_string(Name),
      trace_id => TraceIdBytes,
      span_id => SpanIdBytes,
      parent_span_id => ParentSpanId,
      start_time => to_oc_timestamp(StartTime),
      end_time => to_oc_timestamp(EndTime),
      kind => Kind,
      attributes => to_oc_attributes(Attributes),
      stack_trace => to_oc_stack_trace(StackTrace),
      time_events => to_oc_time_events(TimeEvents),
      links => to_oc_links(Links),
      status => to_oc_proto(Status),
      same_process_as_parent_span => case SameProcessAsParentSpan of
                                         undefined -> undefined;
                                         _ -> #{value => SameProcessAsParentSpan}
                                     end,
      child_span_count => case ChildSpanCount of
                              undefined -> undefined;
                              _ -> #{value => ChildSpanCount}
                          end};
to_oc_proto(undefined) ->
    undefined.

to_oc_links(Links) ->
    {LinksPb, DroppedLinks} =
        lists:foldl(fun(#link{type=Type,
                              trace_id=TraceId,
                              span_id=SpanId,
                              attributes=Attributes}, {Acc, DroppedAcc}) ->
                            TraceIdBytes = <<TraceId:128>>,
                            SpanIdBytes = <<SpanId:64>>,
                            {[#{type => Type,
                                trace_id => TraceIdBytes,
                                span_id => SpanIdBytes,
                                attributes => to_oc_attributes(Attributes)} | Acc], DroppedAcc}
                    end, {[], 0}, Links),
    #{link => LinksPb,
      dropped_links_count => DroppedLinks}.

to_oc_timestamp({T, O}) ->
    Time = T + O,
    #{seconds => erlang:convert_time_unit(Time, native, second),
      nanos => erlang:convert_time_unit(Time, native, nanosecond) rem 1000000000}.

-spec to_oc_stack_frame(erlang:stack_item()) -> oc_trace_pb:stack_frame_pb().
to_oc_stack_frame({Module, Function, _Arity, Location}) ->
    Filename = proplists:get_value(file, Location, undefined),
    Line = proplists:get_value(line, Location, 0),
    #{function_name => trunc_string(Function),
      original_function_name => trunc_string(Function),
      file_name => trunc_string(Filename),
      line_number => Line,
      column_number => 0,
      load_module => #{module => trunc_string(Module)
                       %% build_id  =>  undefined
                      }
      %% source_version => undefined
     }.

-spec to_oc_stack_trace([erlang:stack_item()] | undefined) -> oc_trace_pb:stack_trace_pb() | undefined.
to_oc_stack_trace(undefined) -> undefined;
to_oc_stack_trace(Frames) ->
    #{stack_frames => #{frame => [to_oc_stack_frame(F) || F <- Frames],
                        dropped_frames_count => 0},
      stack_trace_hash_id => 0}.

to_oc_attributes(Map) ->
    #{attribute_map => maps:map(fun(_, V) when is_binary(V) ->
                                        #{value => {string_value, trunc_string(V)}};
                                   (_, V) when is_integer(V) ->
                                        #{value => {int_value, V}};
                                   (_, V) when is_boolean(V) ->
                                        #{value => {bool_value, V}}
                                end, Map),
      dropped_attributes_count => 0}.

to_oc_time_events(TimeEvents) ->
    {Events, DroppedAnnotations, DroppedMessageEvents} =
        lists:foldl(fun to_oc_time_event/2, {[], 0, 0}, TimeEvents),
    #{time_event => Events,
      dropped_annotations_count => DroppedAnnotations,
      dropped_message_events_count => DroppedMessageEvents}.

to_oc_time_event({Time, #annotation{description=Description,
                                    attributes=Attributes}},
                 {Acc, DroppedAnnotationsAcc, DroppedMessageAcc}) ->
    Annotation = #{description => trunc_string(Description),
                   attributes => to_oc_attributes(Attributes)},
    {[#{time => to_oc_timestamp(Time),
        value => {annotation, Annotation}} | Acc],
     DroppedAnnotationsAcc, DroppedMessageAcc};
to_oc_time_event({Time, MessageEvent=#message_event{}},
                 {Acc, DroppedAnnotationsAcc, DroppedMessageAcc}) ->
    MessageEventPb  =  to_oc_proto(MessageEvent),
    {[#{time => to_oc_timestamp(Time),
        value => {message_event, MessageEventPb}} | Acc],
     DroppedAnnotationsAcc, DroppedMessageAcc}.

trunc_string(undefined) ->
    undefined;
trunc_string(V) when is_atom(V) ->
    trunc_string(atom_to_binary(V, utf8));
trunc_string(V) when is_list(V) ->
    trunc_string(list_to_binary(V));
trunc_string(V) when is_binary(V) ->
    #{value => V,
      truncated_byte_count => 0}.

