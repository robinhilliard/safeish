# Safeish

NOT FOR PRODUCTION USE

Safe-ish is an _experimental_ minimally restrictive sandbox for BEAM modules 
that examines and rejects BEAM bytecode containing instructions that could 
cause side effects outside an approved whitelist of callable modules. These 
instructions include:

- Spawning processes
- Sending and receiving messages
- File system access
- Network access
- Compilation
- System level introspection and diagnostics
- Creating atoms dynamically at runtime (which would allow calls to non-whitelisted modules)

To achieve any of these things the modules would have to go via mediating code in a 
whitelisted module. For example, you might provide an implementation of spawn that 
registered a limited number of PIDs and a send that could only message registered PIDs,
along with macros to make the change transparent.

A helper is also included for processes that call safeish-approved modules to limit memory use
and number of reductions per call.

## Use

```
> Safeish.load_file("Unsafe")
{:error, "Unsafe.beam: Calling String.to_atom() not allowed"}
> Safeish.load_file("Safe")
[{FirstSafeModule, <<...>>}, {SecondSafeModule, <<...>>}, ...]
```

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `safeish` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:safeish, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/safeish](https://hexdocs.pm/safeish).

