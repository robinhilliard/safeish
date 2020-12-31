defmodule SafeishTest do
  use ExUnit.Case
  
  
  # Note we can't use Code.compile_string() because that actually loads the module,
  # defeating the whole purpose of Safeish doing the check
  #
  # Run `mix fixtures` to clean and rebuild beam files
  
  defp get_fixture_bytecode(fixture) do
    {:ok, file} = File.open("#{File.cwd!()}/test/fixtures_build/#{Atom.to_string(fixture)}.beam", [:read])
    bytecode = IO.binread(file, :all)
    File.close(file)
    bytecode
  end
  
  
  test "invalid bytecode" do
    assert {:error, :not_beam_bytecode} = Safeish.module_risks(<<0>>)
  end

  
  test "module_risk lists call to Enum.map" do
    {:ok, ModuleCallsEnumMap, risks} = Safeish.module_risks(get_fixture_bytecode(ModuleCallsEnumMap))
    assert MapSet.equal?(
             MapSet.new([
               :call_ext_only,
               :func_info,
               :gc_bif2,
               :label,
               :line,
               :make_fun2,
               :move,
               :return,
               :select_val,
               {Enum, :map, 2},
               {:erlang, :+, 2},
               {:erlang, :get_module_info, 1},
               {:erlang, :get_module_info, 2}
             ]),
             risks)
  end
  
  
  test "module_risk lists call to File.read" do
    {:ok, ModuleCallsFileRead, risks} =
      Safeish.module_risks(get_fixture_bytecode(ModuleCallsFileRead))
    assert MapSet.equal?(
             MapSet.new([
               :call_ext_only,
               :func_info,
               :label,
               :line,
               :move,
               :return,
               :select_val,
               {File, :read, 1},
               {:erlang, :get_module_info, 1},
               {:erlang, :get_module_info, 2}
             ]),
             risks)
  end
  
  
  test "module_risk lists spawning process" do
    {:ok, ModuleSpawnsProcess, risks} =
      Safeish.module_risks(get_fixture_bytecode(ModuleSpawnsProcess))
    assert MapSet.equal?(
             MapSet.new([
               :call_ext_only,
               :func_info,
               :label,
               :line,
               :make_fun2,
               :move,
               :return,
               :select_val,
               {:erlang, :get_module_info, 1},
               {:erlang, :get_module_info, 2},
               {:erlang, :spawn, 3}
             ]),
             risks)
  end
  
  
  test "module_risk lists receiving message" do
    {:ok, ModuleReceivesMessage, risks} =
      Safeish.module_risks(get_fixture_bytecode(ModuleReceivesMessage))
    assert MapSet.equal?(
             MapSet.new([
               :allocate,
               :call_ext_only,
               :deallocate,
               :func_info,
               :label,
               :line,
               :loop_rec,
               :move,
               :remove_message,
               :return,
               :select_val,
               :wait,
               {:erlang, :get_module_info, 1},
               {:erlang, :get_module_info, 2}
             ]),
             risks)
  end
  
  
  test "module_risk lists sending message" do
    {:ok, ModuleSendsMessage, risks} =
      Safeish.module_risks(get_fixture_bytecode(ModuleSendsMessage))
    assert MapSet.equal?(
             MapSet.new([
               :bif0,
               :call_ext_only,
               :func_info,
               :label,
               :line,
               :move,
               :return,
               :select_val,
               {:erlang, :get_module_info, 1},
               {:erlang, :get_module_info, 2},
               {:erlang, :self, 0},
               {:erlang, :send, 2}
             ]),
             risks)
  end
  
  
  test "module_risk lists call to binary_to_atom" do
    {:ok, ModuleCallsToAtom, risks} =
      Safeish.module_risks(get_fixture_bytecode(ModuleCallsToAtom))
    assert MapSet.equal?(
             MapSet.new([
               :allocate,
               :call_ext,
               :call_ext_last,
               :call_ext_only,
               :func_info,
               :label,
               :line,
               :move,
               :return,
               :select_val,
               {Enum, :random, 1},
               {:erlang, :binary_to_atom, 2},
               {:erlang, :get_module_info, 1},
               {:erlang, :get_module_info, 2}
             ]),
             risks)
  end
  
  
  test "check responds ok to call Enum.map" do
    assert {:ok, ModuleCallsEnumMap} = get_fixture_bytecode(ModuleCallsEnumMap) |> Safeish.check
  end
  
  
  test "check responds not ok to call File.read" do
    assert {:error, ModuleCallsFileRead, ["Elixir.File.read/1 not whitelisted"]} =
                    get_fixture_bytecode(ModuleCallsFileRead) |> Safeish.check
  end
  
  
  test "check responds not ok to spawning process" do
    assert {:error, ModuleSpawnsProcess, [":erlang.spawn/3 not whitelisted"]} =
                    get_fixture_bytecode(ModuleSpawnsProcess) |> Safeish.check
  end
  
  
  test "check responds not ok to receiving message" do
    assert {:error, ModuleReceivesMessage, ["receive (remove_message) not allowed"]} =
                    get_fixture_bytecode(ModuleReceivesMessage) |> Safeish.check
  end
  
  
  test "check responds not ok to sending message" do
    assert {:error, ModuleSendsMessage, [":erlang.send/2 not whitelisted"]} =
                    get_fixture_bytecode(ModuleSendsMessage) |> Safeish.check
  end
  
  
  test "check responds not ok to calling to_atom" do
    assert {:error, ModuleCallsToAtom, [":erlang.binary_to_atom/2 not whitelisted"]} =
                    get_fixture_bytecode(ModuleCallsToAtom) |> Safeish.check
  end


  test "check allows whitelisting of File module" do
    assert {:ok, ModuleCallsFileRead} =
             get_fixture_bytecode(ModuleCallsFileRead) |> Safeish.check([File])
  end
  
  
  test "check allows whitelisting of spawn" do
    assert {:ok, ModuleSpawnsProcess} =
             get_fixture_bytecode(ModuleSpawnsProcess) |> Safeish.check([{:erlang, :spawn, 3}])
  end
  
  test "check allows whitelisting of send" do
    assert {:ok, ModuleSendsMessage} =
             get_fixture_bytecode(ModuleSendsMessage) |> Safeish.check([:send])
  end
  
  test "check allows whitelisting of receive" do
    assert {:ok, ModuleReceivesMessage} =
             get_fixture_bytecode(ModuleReceivesMessage) |> Safeish.check([:receive])
  end
end
