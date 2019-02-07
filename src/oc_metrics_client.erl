%%%-------------------------------------------------------------------
%% @doc Client module for grpc service opencensus.proto.agent.metrics.v1.MetricsService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2019-02-07T00:47:39+00:00 and should not be modified manually

-module(oc_metrics_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'opencensus.proto.agent.metrics.v1.MetricsService').
-define(PROTO_MODULE, 'metrics_service_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

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
    grpcbox_client:stream(Ctx, <<"/opencensus.proto.agent.metrics.v1.MetricsService/Export">>, ?DEF(export_metrics_service_request, export_metrics_service_response, <<"opencensus.proto.agent.metrics.v1.ExportMetricsServiceRequest">>), Options).

