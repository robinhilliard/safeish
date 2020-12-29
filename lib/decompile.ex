defmodule Decompile do
  @moduledoc false
  
  
  defstruct [
    instruction_set: nil,
    number_of_functions: 0,
    number_of_labels: 0,
    opcode_max: 0,
    atoms: [],
    literals: [],
    code: []
  ]
  
  
  def decompile(bytecode) do
    {:ok, {module, [{'AtU8', atoms}, {'LitT', literals}, {'Code', code}]}} =
      :beam_lib.chunks(bytecode, ['AtU8', 'LitT', 'Code'])
    [result | code] = parse_code(code)
    parsed_atoms = parse_atoms(atoms)
    parsed_literals = parse_literals(literals)
    {
      :ok,
      module,
      %{result |
        code: code |> decode_tags(parsed_atoms, parsed_literals) |> fix_labels,
        atoms: parsed_atoms,
        literals: parsed_literals
      }
    }
  end
  
  
 
  def decode_tags([{:tag_u, value} | rest], a, l), do: [value | decode_tags(rest, a, l)]
  def decode_tags([{:tag_i, value} | rest], a, l), do: [value | decode_tags(rest, a, l)]
  def decode_tags([{:tag_a, 0} | rest], a, l), do: [nil | decode_tags(rest, a, l)]
  def decode_tags([{:tag_a, index} | rest], a, l), do: [elem(a, index - 1) | decode_tags(rest, a, l)]
  def decode_tags([{:tag_x, index} | rest], a, l), do: [:"X#{index}" | decode_tags(rest, a, l)]
  def decode_tags([{:tag_y, index} | rest], a, l), do: [:"Y#{index}" | decode_tags(rest, a, l)]
  def decode_tags([{:tag_f, value} | rest], a, l), do: [{:label, value} | decode_tags(rest, a, l)]
  def decode_tags([{:tag_h, c} | rest], a, l), do: [String.next_codepoint(c) | decode_tags(rest, a, l)]
  def decode_tags([{:tag_extended_literal, index} | rest], a, l), do: [elem(l, index) | decode_tags(rest, a, l)]
  def decode_tags([{:tag_extended_list, list} | rest ], a, l), do: [decode_tags(list, a, l) | decode_tags(rest, a, l)]
  def decode_tags([{key, value} | rest], a, l) when is_atom(key), do: [{key, decode_tags(value, a, l)} | decode_tags(rest, a, l)]
  def decode_tags([x | rest], a, l), do: [decode_tags(x, a, l) | decode_tags(rest, a, l)]
  def decode_tags([], _, _), do: []
  def decode_tags(x, _, _), do: x

  
  def fix_labels([{:label, [num]} | rest]) when is_number(num), do: [{:label, num} | fix_labels(rest)]
  def fix_labels([x | rest]), do: [fix_labels(x) | fix_labels(rest)]
  def fix_labels([]), do: []
  def fix_labels(x), do: x
  
  def parse_atoms(<<atom_count::integer-size(32), atoms::binary>>) do
    parse_atoms(atoms, atom_count, [])
  end
  
  def parse_atoms(_, 0, names), do: names |> Enum.reverse |> List.to_tuple
  
  def parse_atoms(<<length::integer, name::binary-size(length), atoms::binary>>,
        atoms_remaining, names) do
    parse_atoms(atoms, atoms_remaining - 1, [String.to_atom(name) | names])
  end
  
  
  def parse_literals(<<_compressed_table_size::integer-size(32), compressed::binary>>) do
    <<_number_of_literals::integer-size(32), table::binary>> = :zlib.uncompress(compressed)
    parse_literals(table, [])
  end
  
  def parse_literals(<<>>, literals), do: literals |> Enum.reverse |> List.to_tuple
  
  def parse_literals(<<size::integer-size(32), literal::binary-size(size), rest::binary>>, literals) do
    parse_literals(rest, [:erlang.binary_to_term(literal) | literals])
  end
  
  
  def parse_code(code=<<sub_size::integer-size(32), chunk::binary>>) do
    size = byte_size(code) - sub_size - 8
    <<info_and_opcodes::binary-size(size), _align::binary>> = chunk
    <<
      instruction_set::integer-size(32),
      opcode_max::integer-size(32),
      number_of_labels::integer-size(32),
      number_of_functions::integer-size(32),
      opcodes::binary
    >> = info_and_opcodes

    parse_code(
      opcodes,
      [
        %Decompile{
          number_of_functions: number_of_functions,
          number_of_labels: number_of_labels,
          opcode_max: opcode_max,
          instruction_set: instruction_set
        }
      ])
  end
  
  def parse_code(<<>>, decoded) do
    Enum.reverse(decoded)
  end
  
  def parse_code(<<opcode::integer, rest::binary>>, decoded) do
    {name, arity} = :beam_opcodes.opname(opcode)
    {rest_after_args, args} = parse_compact_terms(rest, arity, [])
    parse_code(rest_after_args, [{name, args} | decoded])
  end
  
  
  def parse_compact_terms(rest, 0, parsed_terms), do: {rest, Enum.reverse(parsed_terms)}
  
  def parse_compact_terms(<<value::integer-size(4), 0::integer-size(1), tag::integer-size(3), rest::binary>>,
        remaining_terms, parsed_terms) when tag < 7 do
      parse_compact_terms(rest, remaining_terms - 1, [{decode_tag(tag), value} | parsed_terms])
  end
  
  def parse_compact_terms(<<value_msb::integer-size(3), 0b10::integer-size(2), tag::integer-size(3), value_lsb::integer-size(8), rest::binary>>,
        remaining_terms, parsed_terms) when tag < 7 do
    parse_compact_terms(rest, remaining_terms - 1, [{decode_tag(tag), value_lsb + value_msb * 512} | parsed_terms])
  end
  
  def parse_compact_terms(<<size_minus_2::integer-size(3), 0b11::integer-size(2), tag::integer-size(3), rest::binary>>,
        remaining_terms, parsed_terms) when tag < 7 do
    size = (size_minus_2 + 2) * 8
    <<value::signed-integer-size(size), rest_after_value::binary>> = rest
    parse_compact_terms(rest_after_value, remaining_terms - 1, [{decode_tag(tag), value} | parsed_terms])
  end
  
  def parse_compact_terms(<<0b11111::integer-size(5), tag::integer-size(3), rest::binary>>,
        remaining_terms, parsed_terms) when tag < 7 do
    {rest_after_size, [{:tag_u, size_minus_9}]} = parse_compact_terms(rest, 1, [])
    size = (size_minus_9 + 9) * 8
    <<value::signed-integer-size(size), rest_after_value::binary>> = rest_after_size
    parse_compact_terms(rest_after_value, remaining_terms - 1, [{decode_tag(tag), value} | parsed_terms])
  end
  
  def parse_compact_terms(<<0b00010111, rest::binary>>,
        remaining_terms, parsed_terms) do
    {rest_after_length, [{:tag_u, list_length}]} = parse_compact_terms(rest, 1, [])
    {rest_after_list, terms} = parse_compact_terms(rest_after_length, list_length, [])
    pairs = terms |> Enum.chunk_every(2)
    parse_compact_terms(rest_after_list, remaining_terms - 1, [{:tag_extended_list, pairs} | parsed_terms])
  end
  
  def parse_compact_terms(<<0b01000111, rest::binary>>,
        remaining_terms, parsed_terms) do
    {rest_after_index, [{:tag_u, index}]} = parse_compact_terms(rest, 1, [])
    parse_compact_terms(rest_after_index, remaining_terms - 1, [{:tag_extended_literal, index} | parsed_terms])
  end
  
  # TODO rest of TBB section 6.2.14, probably doing negative numbers wrong too,
  # TODO see https://github.com/erlang/otp/blob/master/lib/compiler/src/beam_asm.erl
  
  
  def decode_tag(0), do: :tag_u   # literal
  def decode_tag(1), do: :tag_i   # integer
  def decode_tag(2), do: :tag_a   # atom
  def decode_tag(3), do: :tag_x   # X register
  def decode_tag(4), do: :tag_y   # Y register
  def decode_tag(5), do: :tag_f   # label
  def decode_tag(6), do: :tag_h   # character

  # Following only since OTP 20
  def decode_tag(0b00010111), do: :tag_extended_list
  def decode_tag(0b00100111), do: :tag_extended_fp_register
  def decode_tag(0b00110111), do: :tag_extended_allocation_list
  def decode_tag(0b01000111), do: :tag_extended_literal

end
