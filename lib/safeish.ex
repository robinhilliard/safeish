defmodule Safeish do
  @moduledoc """
  NOT FOR PRODUCTION USE

  Safe-ish is an _experimental_ minimally restrictive sandbox for BEAM modules
  that examines and rejects BEAM bytecode at load time containing instructions
  that could cause side effects outside an approved whitelist of callable modules.
  These instructions include:
  
  - Spawning processes
  - Sending and receiving messages
  - File system access
  - Network access
  - Compilation
  - System level introspection and diagnostics
  - Creating atoms dynamically at runtime (which would allow calls to non-whitelisted modules)
  
  To achieve any of these things the modules would have to go via mediating code in a
  whitelisted module. For example, a whitelisted module might provide an implementation of
  spawn that registered a limited number of PIDs and a send that could only message registered
  PIDs, along with spawn and send macros that delegated to the whitelisted module to make the
  change transparent.
  
  A helper is also included for processes that call safeish-approved modules to limit
  memory use/time elapsed/number of reductions per call.
  """

  @doc """
  load_file

  ## Examples
  ```
      iex> Safeish.load_file("path/to/Safe", [WhitelistedModuleA, WhitelistedModuleB])
      {:ok, [SafeModule1, SafeModule2, ...]}
  ```
  """
  
  def load_file(filename) do
  
  end
  
  
  def load_bytecode(bytecode) do
  
  end
  
  
  def module_calls(bytecode) when is_binary(bytecode) do
    module_calls(:beam_lib.chunks(bytecode, [:abstract_code]), MapSet.new())
  end
  def module_calls(
        {:call, _line_no, {:remote, _, {:atom, _, m}, {:atom, _, f}}, args},
        acc
      ) do
    module_calls(
      args,
      acc
      |> MapSet.put({m, f, length(args)})
    )
  end
  def module_calls({:receive, _line_no, clauses}, acc) do
    module_calls(
      clauses,
      acc
      |> MapSet.put({:builtin, :receive, 1})
    )
  end
  def module_calls([], acc), do: acc
  def module_calls([chunk | chunks], acc),
      do: module_calls(chunk, module_calls(chunks, acc))
  def module_calls(t, acc) when is_tuple(t),
      do: module_calls(Tuple.to_list(t), acc)
  def module_calls(_, acc), do: acc
end
