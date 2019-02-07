%%%-------------------------------------------------------------------
%% @doc Client module for grpc service opencensus.proto.agent.trace.v1.TraceService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-02-07T00:47:38+00:00 and should not be modified manually

-module(oc_trace_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'opencensus.proto.agent.trace.v1.TraceService').
-define(PROTO_MODULE, 'trace_service_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc 
-spec config() ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
config() ->
    config(ctx:new(), #{}).

-spec config(ctx:t() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
config(Ctx) when ?is_ctx(Ctx) ->
    config(Ctx, #{});
config(Options) ->
    config(ctx:new(), Options).

-spec config(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
config(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/opencensus.proto.agent.trace.v1.TraceService/Config">>, ?DEF(current_library_config, updated_library_config, <<"opencensus.proto.agent.trace.v1.CurrentLibraryConfig">>), Options).

%% @doc 
-spec export() ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
export() ->
    export(ctx:new(), #{}).

-spec export(ctx:t() | grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
export(Ctx) when ?is_ctx(Ctx) ->
    export(Ctx, #{});
export(Options) ->
    export(ctx:new(), Options).

-spec export(ctx:t(), grpcbox_client:options()) ->
    {ok, grpcbox_client:stream()} | grpcbox_stream:grpc_error_response().
export(Ctx, Options) ->
    grpcbox_client:stream(Ctx, <<"/opencensus.proto.agent.trace.v1.TraceService/Export">>, ?DEF(export_trace_service_request, export_trace_service_response, <<"opencensus.proto.agent.trace.v1.ExportTraceServiceRequest">>), Options).

