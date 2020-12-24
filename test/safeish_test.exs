defmodule SafeishTest do
  use ExUnit.Case
  doctest Safeish

  test "module calls Enum.map" do
    [{ModuleCallsEnumMap, bytecode}] = Code.compile_string(
      """
      defmodule ModuleCallsEnumMap do
        def f() do
          Enum.map([1,2,3], fn x -> x + 1 end)
        end
      end
      """
    )
    assert MapSet.equal?(
             MapSet.new([{Enum, :map, 2}, {:erlang, :get_module_info, 2}]),
             Safeish.module_calls(bytecode)
           )
  end
  
  
  test "module calls File.read" do
    [{ModuleCallsFileRead, bytecode}] = Code.compile_string(
      """
      defmodule ModuleCallsFileRead do
        def f() do
          File.read("nofile.txt")
        end
      end
      """
    )
    assert MapSet.equal?(
             MapSet.new([{File, :read, 1}, {:erlang, :get_module_info, 2}]),
             Safeish.module_calls(bytecode)
           )
  end
  
  
  test "module spawns process" do
    [{ModuleSpawnsProcess, bytecode}] = Code.compile_string(
      """
      defmodule ModuleSpawnsProcess do
        def f() do
          spawn(ModuleSpawnsProcess, fn x -> x end, [])
        end
      end
      """
    )
    assert MapSet.equal?(
             MapSet.new([{:erlang, :spawn, 3}, {:erlang, :get_module_info, 2}]),
             Safeish.module_calls(bytecode)
           )
  end
  
  
  test "module receives message" do
    [{ModuleReceivesMessage, bytecode}] = Code.compile_string(
      """
      defmodule ModuleReceivesMessage do
        def f() do
          receive do
            _ ->
              true
          end
        end
      end
      """
    )
    assert MapSet.equal?(
             MapSet.new([{:builtin, :receive, 1}, {:erlang, :get_module_info, 2}]),
             Safeish.module_calls(bytecode)
           )
  end
  
  test "module sends message" do
    [{ModuleSendsMessage, bytecode}] = Code.compile_string(
      """
      defmodule ModuleSendsMessage do
        def f() do
          send self(), :msg
        end
      end
      """
    )
    assert MapSet.equal?(
             MapSet.new([{:erlang, :self, 0}, {:erlang, :send, 2}, {:erlang, :get_module_info, 2}]),
             Safeish.module_calls(bytecode)
           )
  end
  
  
  test "module calls to_atom" do
    [{ModuleCallsToAtom, bytecode}] = Code.compile_string(
      """
      defmodule ModuleCallsToAtom do
        def f() do
          String.to_atom("not_allowed")
        end
      end
      """
    )
    assert MapSet.equal?(
             MapSet.new([{:erlang, :binary_to_atom, 2}, {:erlang, :get_module_info, 2}]),
             Safeish.module_calls(bytecode)
           )
  end
  
end
