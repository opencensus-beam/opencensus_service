%%%-------------------------------------------------------------------
%% @doc Client module for grpc service opencensus.proto.agent.trace.v1.TraceService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-01-28T23:04:56+00:00 and should not be modified manually

-module(oc_trace_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(SERVICE, 'opencensus.proto.agent.trace.v1.TraceService').
-define(PROTO_MODULE, 'trace_service_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output), #grpcbox_def{service=?SERVICE,
                                         marshal_fun=?MARSHAL_FUN(Input),
                                         unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc 
-spec config(ctx:t()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
config(Ctx) ->
    config(Ctx, #{}).

-spec config(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
config(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/opencensus.proto.agent.trace.v1.TraceService/Config">>, ?DEF(current_library_config, updated_library_config), Options).

%% @doc 
-spec export(ctx:t()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
export(Ctx) ->
    export(Ctx, #{}).

-spec export(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
export(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/opencensus.proto.agent.trace.v1.TraceService/Export">>, ?DEF(export_trace_service_request, export_trace_service_response), Options).

