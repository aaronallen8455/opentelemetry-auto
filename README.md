# Open Telemetry Auto Instrumentation

This is a GHC plugin for automatically instrumenting a Haskell application with
open telemetry spans based on user configuration. The instrumentation
functionality is provided by
[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry).

- [Quick start](#quick-start)
- [Configuration](#configuration)
  - [Config structure](#config-structure)
  - [Example config](#example-config)
- [Known pitfalls](#known-pitfalls)

### Quick start

- Add this package as a project dependency
- Create a file called `auto-instrument-config.toml` in the project root directory:
  ```toml
  [[targets]]
  type = "constructor"
  value = "MyAppMonad"
  ```
  Replace `MyAppMonad` with your application's primary monad. This monad needs
  to have an instance for `MonadUnliftIO`, otherwise you'll get a type error.
- Initialize the global tracer provider as part of application startup. The
  plugin will not insert spans until after the gobal tracer provider has been
  initialized. See the
  [`hs-opentelemetry-sdk` documentation](https://hackage.haskell.org/package/hs-opentelemetry-sdk)
  for directions.
- Pass the `-fplugin AutoInstrument` argument to GHC when compiling the project.
  This can be done project-wide in the `*.cabal` or `package.yaml` file using
  `ghc-options: -fplugin AutoInstrument`, or by adding
  `{- OPTIONS_GHC -fplugin AutoInstrument -}` to individual modules.
- Only top-level functions that have type signatures with a return type that
  matches the target monad will be instrumented.

### Configuration

Configuration is supplied by a user defined TOML file and is used to determine
which functions should be instrumented. By default the plugin looks for a
config file called `auto-instrument-config.toml` in the project root. You can
change this by passing a config file path as a plugin option, for example:
`-fplugin AutoInstrument -fplugin-opt AutoInstrument:my-config.toml`.

#### Config structure

- The `targets` key is an array of tables that specify how to identify a function
  to instrument based on its type signature.
- These tables have a `type` field that can either `"constructor"` or `"constraints"`
  and a `value` key with the value corresponding to the chosen `type`.
  - `"constructor"` is used to target the return type of the function. This will
    typically be your application's monad. It is not necessary to provide all
    arguments to this type and arguments that should be ignored can replaced with
    an underscore.
  - `"constraints"` allows for a set of constraints to be specified which must
    all be present in the constraint context of a function in order for it to
    be instrumented. The `value` field should be an array of constraint types
    which do not need to be fully applied and can have underscore wildcards.
- The `exclusions` key is an array with the same structure as `targets`. If any
  of these rules match a type signature, the corresponding declaration(s) will
  not be instrumented.

#### Example config

```toml
# Targets are things that should be auto instrumented for tracing.
# "constructor" means that it should match the return type of the function
# while "constraints" means that all the constraints in the "value" array must
# be present in the constraint context of the function.

[[targets]]
type = "constructor"
value = "AppMonad"

[[targets]]
type = "constraints"
value = ["MonadUnliftIO"]

# Exclusions denote types that should not be instrumented. This is primarily
# needed for when a target constraint appears in a definition's context but
# doesn't apply directly to the return type, for example:
# server :: MonadUnliftIO m => ServerT Api m

[[exclusions]]
type = "constructor"
value = "ServerT"

[[exclusions]]
type = "constructor"
value = "ConduitT"
```

### Known pitfalls

Functions that loop can be problematic when instrumented if a new span is
entered for each iteration. For example, if an application has a process that
continually performs some polling action in a loop, then instrumenting that
process would result in a space leak due to the mass of nested spans being
allocated and retained on the heap. One way for dealing with this is to define
a type synonym `type NotInstrumented a = a`, add an exclusion rule for it to
the config, and apply it to the result type of any such looping functions:

```haskell
type NotInstrumented a = a

loop :: NotInstrumented (MyApp ())
loop = do
  ...
  loop
```
---
```toml
[[exclusions]]
type = "constructor"
value = "NotInstrumented"
```
