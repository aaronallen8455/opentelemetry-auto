# Open Telemetry Auto Instrumentation

This is a GHC plugin for automatically instrumenting a Haskell application with
open telemetry spans based on user configuration. The instrumentation
functionality is provided by the
[`hs-opentelemetry`](https://github.com/iand675/hs-opentelemetry) project.

This package is under development and not quite production ready.

### Quick start

- Add this package as a project dependency
- Create a file called `auto-instrument-config.json` in the project root directory:
  ```json
  { "targets": [{
      "type": "constructor",
      "value": "MyAppMonad" }
  ]}
  ```
  Replace `MyAppMonad` with your application's primary monad. This monad needs
  to have instances for `MonadTracer` and `MonadUnliftIO`, otherwise you'll get
  type errors.
- Pass the `-fplugin AutoInstrument` argument to GHC when compiling the project.
- Only top-level functions that have type signatures with a return type that
  matches the target monad will be instrumented.

### Configuration options

Configuration is supplied by a user defined JSON file and is used to determine
which functions to instrument:
- The `targets` field is an array of objects that specify how to identify a function
  to instrument based on its type signature.
- These objects have a `type` field that can either `"constructor"` or `"constraints"`
  and a `value` field with the value corresponding to the chosen `type`.
  - `"constructor"` is used to target the return type of the function. This will
    typically be your application's monad. It is not necessary to provide all
    arguments to this type and arguments that should be ignored can replaced with
    an underscore.
  - `"constraints"` allows for a set of constraints to be specified which must
    be contained by the constraint context of a function in order for it to
    be instrumented. The `value` field should be an array of constraint types
    which do not need to be fully applied and can have underscore wildcards.

By default the plugin looks for a config file
called `auto-instrument-config.json` in the project root. You can change this
by passing a config file path as a plugin option, for example: `-fplugin
AutoInstrument -fplugin-opt AutoInstrument:my-config.json`.
