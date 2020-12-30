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
    IO.inspect Decompile.decompile(get_fixture_bytecode(ModuleCallsEnumMap)), limit: :infinity
  end
  
end
