# OpenCensus Agent Reporter

This reporter exports spans to the [OpenCensus Agent](https://github.com/census-instrumentation/opencensus-service) in the standard [protobuf format](https://github.com/census-instrumentation/opencensus-proto) over [grpc](https://grpc.io/).

## Installation and Configuration

### Erlang

Take a dependency on `opencensus_service`, e.g. with `rebar3` by adding it
to your applications list in `.app.src`.

Configure `:opencensus`' `:reporters` to use `:oc_reporter_service`:

``` erlang
{opencensus, [
    {reporters, [{oc_reporter_service, #{}}]}
...]}
```

### Elixir

Take a dependency on `opencensus_service` by adding to your `deps` in
`mix.exs`:

```elixir
{:opencensus, "~> 0.9.0", override: true},
{:opencensus_service, "~> 0.1"}
```

(You'll need the `override` because the `opencensus_service` package expects
version 0.7 of `opencensus`. It seems to work fine with 0.9, however.)

Configure `:opencensus`' `:reporters` in `config/config.exs` to use
`:oc_reporter_service`:

```elixir
config :opencensus, :reporters, [{:oc_reporter_service, %{}}]
```

The map (`%{}`) at the end gives `oc_reporter_service` its options. Its
defaults are:

```elixir
%{
  channel_name: :channel_service,
  endpoints: [{:http, 'localhost', 55678, []}],
  options: %{},
  sup_flags: %{intensity: 1, period: 5, strategy: :one_for_one}
}
```

*WARNING:* the hostname in `endpoints` is a charlist, _not_ a binary. If
you're pulling the endpoint out of an environment variable, pipe the result
from `System.get_env/1` through `String.to_charlist/1`.
