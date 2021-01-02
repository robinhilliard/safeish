defmodule DecompilerTest do
  use ExUnit.Case
  
  defp get_fixture_bytecode(fixture) do
    {:ok, file} = File.open("#{File.cwd!()}/test/fixtures_build/#{Atom.to_string(fixture)}.beam", [:read])
    bytecode = IO.binread(file, :all)
    File.close(file)
    bytecode
  end
  
  test "parse atoms" do
    assert {ModuleCallsEnumMap, :__info__, :attributes} = Decompile.parse_atoms(
                 <<0, 0, 0, 3, 25, 69, 108, 105, 120, 105, 114,
                   46, 77, 111, 100, 117, 108, 101, 67, 97, 108,
                   108, 115, 69, 110, 117, 109, 77, 97, 112, 8,
                   95, 95, 105, 110, 102, 111, 95, 95, 10, 97,
                   116, 116, 114, 105, 98, 117, 116, 101, 115>>)
  end
  
#  test "tag value under 16" do
#    IO.inspect Decompile.decompile(get_fixture_bytecode(ModuleTagValueUnder16))
#  end
  
  test "decompile module calling enum map" do
    assert {
              :ok,
              ModuleCallsEnumMap,
              %Decompile{
                atoms: {
                  ModuleCallsEnumMap,
                  :__info__,
                  :attributes,
                  :compile,
                  :deprecated,
                  :functions,
                  :macros,
                  :md5,
                  :module,
                  :erlang,
                  :get_module_info,
                  :f,
                  Enum,
                  :map,
                  :module_info,
                  :"-f/0-fun-0-",
                  :+
                },
                code: [
                  label: 1,
                  line: '',
                  func_info: [ModuleCallsEnumMap, :__info__, 1],
                  label: 2,
                  select_val: [
                    {:x, 0},
                    {:f, 1},
                    [
                      [:attributes, {:f, 6}],
                      [:compile, {:f, 6}],
                      [:deprecated, {:f, 5}],
                      [:functions, {:f, 4}],
                      [:macros, {:f, 5}],
                      [:md5, {:f, 6}],
                      [:module, {:f, 3}]
                    ]
                  ],
                  label: 3,
                  move: [ModuleCallsEnumMap, {:x, 0}],
                  return: '',
                  label: 4,
                  move: [[f: 0], {:x, 0}],
                  return: '',
                  label: 5,
                  move: [nil, {:x, 0}],
                  return: '',
                  label: 6,
                  move: [x: 0, x: 1],
                  move: [ModuleCallsEnumMap, {:x, 0}],
                  line: '',
                  call_ext_only: [2, {:erlang, :get_module_info, 2}],
                  label: 7,
                  line: 1,
                  func_info: [ModuleCallsEnumMap, :f, 0],
                  label: 8,
                  make_fun2: [0],
                  move: [x: 0, x: 1],
                  move: [[1, 2, 3], {:x, 0}],
                  line: 2,
                  call_ext_only: [2, {Enum, :map, 2}],
                  label: 9,
                  line: '',
                  func_info: [ModuleCallsEnumMap, :module_info, 0],
                  label: 10,
                  move: [ModuleCallsEnumMap, {:x, 0}],
                  line: '',
                  call_ext_only: [1, {:erlang, :get_module_info, 1}],
                  label: 11,
                  line: '',
                  func_info: [ModuleCallsEnumMap, :module_info, 1],
                  label: 12,
                  move: [x: 0, x: 1],
                  move: [ModuleCallsEnumMap, {:x, 0}],
                  line: '',
                  call_ext_only: [2, {:erlang, :get_module_info, 2}],
                  label: 13,
                  line: 2,
                  func_info: [ModuleCallsEnumMap, :"-f/0-fun-0-", 1],
                  label: 14,
                  line: 2,
                  gc_bif2: [
                    {:f, 0},
                    1,
                    {:erlang, :+, 2},
                    {:x, 0},
                    1,
                    {:x, 0}
                  ]
                ],
                exports: {
                  {:module_info, 1, {:label, 12}},
                  {:module_info, 0, {:label, 10}},
                  {:f, 0, {:label, 8}},
                  {:__info__, 1, {:label, 2}}
                },
                functions: {{:"-f/0-fun-0-", 1, 14, 0, 0, 108111326}},
                imports: {
                  {:erlang, :get_module_info, 2},
                  {Enum, :map, 2},
                  {:erlang, :get_module_info, 1},
                  {:erlang, :+, 2}
                },
                instruction_set: 0,
                literals: {[f: 0], [1, 2, 3]},
                number_of_functions: 5,
                number_of_labels: 15,
                opcode_max: 169,
                opcodes: MapSet.new([:call_ext_only, :func_info, :gc_bif2, :label, :line,
                  :make_fun2, :move, :return, :select_val]),
                strings: ""
              }
            } == Decompile.decompile(get_fixture_bytecode(ModuleCallsEnumMap))
  end
  
end
