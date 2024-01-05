# Open Telemetry Auto Instrumentation

This is a GHC plugin for automatically instrumenting a Haskell application with
open telemetry spans based on user configuration. The instrumentation
functionality is provided by the
[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry) project.

### Quick start

- Add this package as a project dependency
- Create a file called `auto-instrument-config.toml` in the project root directory:
  ```toml
  [[targets]]
  type = "constructor"
  value = "MyAppMonad"
  ```
  Replace `MyAppMonad` with your application's primary monad. This monad needs
  to have an instance for `MonadUnliftIO`, otherwise you'll get
  type errors.
- Initialize the global tracer provider as part of application startup. The
  plugin will not insert spans until after the gobal tracer provider has been
  initialized. See the
  [`hs-opentelemetry-sdk` documentation](https://hackage.haskell.org/package/hs-opentelemetry-sdk)
  for directions.
- Pass the `-fplugin AutoInstrument` argument to GHC when compiling the project.
- Only top-level functions that have type signatures with a return type that
  matches the target monad will be instrumented.

### Configuration options

Configuration is supplied by a user defined TOML file and is used to determine
which functions to instrument:
- The `targets` key is an array of tables that specify how to identify a function
  to instrument based on its type signature.
- These tables have a `type` field that can either `"constructor"` or `"constraints"`
  and a `value` key with the value corresponding to the chosen `type`.
  - `"constructor"` is used to target the return type of the function. This will
    typically be your application's monad. It is not necessary to provide all
    arguments to this type and arguments that should be ignored can replaced with
    an underscore.
  - `"constraints"` allows for a set of constraints to be specified which must
    be contained by the constraint context of a function in order for it to
    be instrumented. The `value` field should be an array of constraint types
    which do not need to be fully applied and can have underscore wildcards.
- The `exclusions` key is an array with the same structure as `targets`. If any
  of these rules match a type signature, the corresponding declaration(s) will
  not be instrumented.

By default the plugin looks for a config file
called `auto-instrument-config.toml` in the project root. You can change this
by passing a config file path as a plugin option, for example: `-fplugin
AutoInstrument -fplugin-opt AutoInstrument:my-config.toml`.
