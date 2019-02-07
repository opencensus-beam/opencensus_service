OpenCensus Agent Reporter
=====

This reporter exports spans to the [OpenCensus Agent](https://github.com/census-instrumentation/opencensus-service) in the standard [protobuf format](https://github.com/census-instrumentation/opencensus-proto) over [grpc](https://grpc.io/).

To use, add `opencensus_service` dependency as a runtime application (in rebar3 this means add to the applications list of `.app.src`) and set as the reporter in the `opencensus` configuration:

``` erlang
{opencensus, [
    {reporter, {oc_reporter_service, []}
...]}
```
