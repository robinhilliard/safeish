# Safe-ish

NOT FOR PRODUCTION USE

Safe-ish is an _experimental_, minimally restrictive sandbox for BEAM modules 
that examines and rejects BEAM bytecode at load time containing instructions 
that could cause side effects such as:

- Spawning processes
- Sending and receiving messages
- File system access
- Network access
- Compilation
- System level introspection and diagnostics
- various apply functions and creating atoms dynamically at runtime (which would
  allow calls to non-whitelisted modules)

You can provide an optional whitelist of modules, functions and language features that the
loaded module is allowed to use. Whitelists are applied to calls and also function literals,
because the latter can be used to construct calls in beam assembly without using apply().

## Use
You can call:

- `Safeish.check(bytecode)` to check binary bytecode `bytecode` without loading the module
- `Safeish.load_bytecode(bytecode)` to check `bytecode` and then load the module if it is ok
- `Safeish.load_file(path)` to read the bytecode from the beam file at `path`, check and load it if it is ok

All the above functions take a second optional whitelist argument of calls and language features to allow.
The following list entries are allowed:

- `Module` to allow calls to any function in Elixir module `Module`
- `{Module, function}` to allow calls to an Elixir function with any arity
- `{Module, function, arity}` to allow calls to an Elixir function with a specific arity
- `:module` to allow calls to any function in Erlang module `:module`
- `{:module, function}` to allow calls to an Erlang function with any arity
- `{:module, function, arity}` to allow calls to an Erlang function with a specific arity
- `:send` to allow sending of messages
- `:receive` to allow receipt of messages

The return value for all functions is either `{:ok, Module}` or `{:error, ["reason 1", "reason 2", ...]}`

## Example

```
> Safeish.load_file("CallFileRead.beam")
{:error, CallFileRead, ["Elixir.File.read/1 not whitelisted"]}
> Safeish.load_file("CallFileRead.beam", [{File, read, 1}])
{:ok, CallFileRead}
> CallFileRead.somefunc()
```

## Installation

The package can be installed by adding `safeish` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:safeish, "~> 0.4.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
Docs can be found at [https://hexdocs.pm/safeish](https://hexdocs.pm/safeish).

