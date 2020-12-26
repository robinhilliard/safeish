defmodule Safeish do
  @moduledoc """
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
  - Creating atoms dynamically at runtime (which would allow calls to non-whitelisted modules)
  
  You can provide an optional whitelist of modules, functions and language features that the
  loaded module is allowed to use.
  """
  
  # Following lists were compiled for Elixir 1.10.4 and OTP release 23
  
  # Within the erlang module only the following BIFs are whitelisted
  # TODO check arguments to apply(), and specify arity
  
  @whitelisted_erlang_bifs MapSet.new([
    :abs, :adler32, :adler32_combine, :append_element, :atom_to_binary, :atom_to_list, :binary_part,
    :binary_to_float, :binary_to_integer, :binary_to_list, :bit_size, :bitstring_to_list, :byte_size,
    :cancel_timer, :ceil, :convert_time_unit, :crc32, :crc32_combine, :date, :delete_element, :display,
    :element, :erase,  :external_size, :float, :float_to_binary, :float_to_list, :floor, :fun_info,
    :fun_to_list, :function_exported, :get, :get_keys, :get_module_info, :insert_element, :integer_to_binary,
    :integer_to_list, :iolist_size, :iolist_to_binary, :iolist_to_iovec, :is_alive, :is_atom, :is_binary,
    :is_bitstring, :is_boolean, :is_builtin, :is_float, :is_function, :is_integer, :is_list, :is_map,
    :is_map_key, :is_number, :is_pid, :is_port, :is_process_alive, :is_record, :is_reference, :is_tuple,
    :length, :list_to_binary, :list_to_bitstring, :list_to_float, :list_to_integer, :list_to_ref,
    :list_to_tuple, :loaded, :localtime, :localtime_to_universaltime, :make_ref, :make_tuple, :map_get,
    :map_size, :match_spec_test, :max, :md5, :md5_final, :md5_init, :md5_update, :min, :monotonic_time,
    :now, :phash, :phash2, :pid_to_list, :put, :read_timer, :ref_to_list, :round, :self, :setelement,
    :size, :split_binary, :start_timer, :system_time, :term_to_binary, :term_to_iovec, :throw, :time,
    :time_offset, :timestamp, :tl, :trunc, :tuple_size, :tuple_to_list, :unique_integer, :universaltime,
    :universaltime_to_localtime
  ])
  
  
  # Skipped beam_lib, c, dets, digraph, digraph_utils, epp, erl_anno, erl_eval, erl_expand_records,
  # erl_id_trans, erl_internal, erl_lint, erl_parse, erl_scan, erl_tar, ets, file_sorter, file_lib,
  # gen_event, gen_fsm, gen_server, gen_statem, io, io_lib, ms_transform, pool, proc_lib, qlc, shell
  # shell_default, shell_docs, slave, supervisor, supervisor_bridge, sys, win32reg, zip
  @whitelisted_erlang_stdlib_modules MapSet.new([
    :array, :base64, :binary, :calendar, :dict, :erl_pp, :filename, :gb_sets, :gb_trees, :lists, :maps,
    :math, :orddict, :ordsets, :proplists, :queue, :rand, :re, :sets, :sofs, :string, :timer, :unicode,
    :uri_string
  ])
  
  
  # Skipped modules with problem functions: Function, Module, String, IO (:stdio/:stderr are problem),
  # Agent, Application, Config, Config.Provider, Config.Reader, DynamicSupervisor, GenServer, Node,
  # Process, Registry, Supervisor, Task, Task.Supervisor, Code, Kernel.ParallelCompiler, Macro, Macro.Env
  # Assume all Kernel.* inlined as erlang in bytecode TODO confirm
  @whitelisted_elixir_modules MapSet.new([
    Atom, Base, Bitwise, Date, DateTime, Exception, Float, Integer, NaiveDateTime, Record, Regex,
    Time, Tuple, URI, Version, Version.Requirement, Access, Date.Range, Enum, Keyword, Map,
    MapSet, Range, Stream, OptionParser, Path, StringIO, Calendar, Calendar.ISO, Calendar.TimeZoneDatabase,
    Calendar.UTCOnlyTimeZoneDatabase, Collectable, Enumerable, Inspect, Inspect.Algebra, Inspect.Opts,
    List.Chars, Protocol, String.Chars, BadFunctionError, BadMapError, BadStructError, CaseClauseError,
    Code.LoadError, CompileError, CondClauseError, Enum.EmptyError, Enum.OutOfBoundsError, ErlangError,
    File.CopyError, File.Error, File.LinkError, File.RenameError, FunctionClauseError, IO.StreamError,
    Inspect.Error, KeyError, MatchError, Module.Types.Error, OptionParser.ParseError, Protocol.UndefinedError,
    Regex.CompileError, RuntimeError, SyntaxError, SystemLimitError, TokenMissingError, TryClauseError,
    UndefinedFunctionError, UnicodeConversionError, Version.InvalidRequirementError, Version.InvalidVersionError,
    WithClauseError
  ])
  
  # Explicitly list safe functions in skipped modules
  @whitelisted_elixir_functions MapSet.new([
  
    # Can't allow Function.capture() TODO or check for literal safe arguments to function capture
    {Function, :identity, 1},
    {Function, :info, 1},

    # Can't allow List.to_atom(), List.to_existing_atom()
    {List, :ascii_printable?, 2},
    {List, :delete, 2},
    {List, :delete_at, 2},
    {List, :duplicate, 2},
    {List, :first, 1},
    {List, :flatten, 1},
    {List, :flatten, 2},
    {List, :foldl, 3},
    {List, :foldr, 3},
    {List, :improper?, 1},
    {List, :insert_at, 3},
    {List, :keydelete, 3},
    {List, :keyfind, 4},
    {List, :keymember?, 3},
    {List, :keyreplace, 4},
    {List, :keysort, 2},
    {List, :keystore, 4},
    {List, :keytake, 3},
    {List, :last, 1},
    {List, :myers_difference, 2},
    {List, :myers_difference, 3},
    {List, :pop_at, 3},
    {List, :replace_at, 3},
    {List, :starts_with?, 2},
    {List, :to_charlist, 1},
    {List, :to_float, 1},
    {List, :to_integer, 1},
    {List, :to_integer, 2},
    {List, :to_string, 1},
    {List, :to_tuple, 1},
    {List, :update_at, 3},
    {List, :wrap, 1},
    {List, :zip, 1},
  
    # Can't allow String.to_atom(), String.to_existing_atom()
    {String, :at, 2},
    {String, :bag_distance, 2},
    {String, :capitalize, 2},
    {String, :chunk, 2},
    {String, :codepoints, 1},
    {String, :contains?, 2},
    {String, :downcase, 2},
    {String, :duplicate, 2},
    {String, :ends_with?, 2},
    {String, :equivalent?, 2},
    {String, :first, 1},
    {String, :graphemes, 1},
    {String, :jaro_distance, 2},
    {String, :last, 1},
    {String, :length, 1},
    {String, :match?, 2},
    {String, :myers_difference, 2},
    {String, :next_codepoint, 1},
    {String, :next_grapheme, 2},
    {String, :next_grapheme_size, 1},
    {String, :normalize, 2},
    {String, :pad_leading, 3},
    {String, :pad_trailing, 2},
    {String, :printable?, 2},
    {String, :replace, 4},
    {String, :replace_leading, 3},
    {String, :replace_prefix, 3},
    {String, :replace_suffix, 3},
    {String, :replace_trailing, 3},
    {String, :reverse, 1},
    {String, :slice, 2},
    {String, :slice, 3},
    {String, :split, 1},
    {String, :split, 3},
    {String, :split_at, 2},
    {String, :splitter, 3},
    {String, :starts_with?, 2},
    {String, :to_charlist, 1},
    {String, :to_float, 1},
    {String, :to_integer, 1},
    {String, :to_integer, 2},
    {String, :trim, 1},
    {String, :trim, 2},
    {String, :trim_leading, 1},
    {String, :trim_leading, 2},
    {String, :trim_trailing, 1},
    {String, :trim_trailing, 2},
    {String, :upcase?, 2},
    {String, :valid?, 1},

    # Safe System functions
    {System, :build_info, 0},
    {System, :compiled_endianness, 0},
    {System, :convert_time_unit, 3},
    {System, :endianness, 0},
    {System, :monotonic_time, 0},
    {System, :monotonic_time, 1},
    {System, :os_time, 0},
    {System, :os_time, 1},
    {System, :otp_release, 0},
    {System, :schedulers, 0},
    {System, :schedulers_online, 0},
    {System, :system_time, 0},
    {System, :system_time, 1},
    {System, :time_offset, 0},
    {System, :time_offset, 1},
    {System, :unique_integer, 1},
    {System, :version, 0}
  ])
  
  
  @doc """
  Check and load module bytecode from a file path
  
  ## Params
  filename:         Path to beam file to check and load if content "safe"
  whitelist:        A list of call targets and language features allowed in the bytecode:
                    - Module
                    - {Module, :function}
                    - {Module, :function, arity}
                    - :send
                    - :receive

  ## Examples
  ```
    iex> Safeish.load_bytecode(<<...>>, [WhitelistedModuleA, {WhitelistedModuleB, :some_func}])
    {:ok, SomeSafeModule}
    iex> SomeSafeModule.func()
    
  ```
  """
  
  
  def load_file(filename, whitelist \\ []) do
    {:ok, file} = File.open(filename, [:read])
    bytecode = IO.binread(file, :all)
    File.close(file)
    load_bytecode(bytecode, whitelist)
  end
  

  @doc """
  Check and load binary module bytecode
  
  ## Params
  bytecode:         Bytecode of module to check and load if content "safe"
  whitelist:        A list of call targets and language features allowed in the bytecode:
                    - Module
                    - {Module, :function}
                    - {Module, :function, arity}
                    - :send
                    - :receive

  ## Examples
  ```
    iex> Safeish.load_bytecode(<<...>>, [WhitelistedModuleA, {WhitelistedModuleB, :some_func}])
    {:ok, SomeSafeModule}
    iex> SomeSafeModule.func()
      
  ```
  """
  def load_bytecode(bytecode, whitelist \\ []) do
    case check(bytecode, whitelist) do
      {:ok, module} ->
        :code.load_binary(module, module, bytecode)
        {:ok, module}
      error ->
        error
    end
  end
  
  
  @doc """
  Check binary module bytecode
  
  ## Params
  bytecode:         Bytecode of module to check and load if content "safe"
  whitelist:        A list of call targets and language features allowed in the bytecode:
                    - Module
                    - {Module, :function}
                    - {Module, :function, arity}
                    - :send
                    - :receive

  ## Examples
  ```
    iex> Safeish.load_bytecode(<<...>>, [WhitelistedModuleA, {WhitelistedModuleB, :some_func}])
    {:ok, SomeSafeModule}
  ```
  """
  def check(bytecode, whitelist \\ []) do
    {:ok, module, risks} = module_risks(bytecode)
    check_list = risks |> Enum.map(&risk_acceptable?(&1, whitelist))
    if Enum.all?(check_list, &match?(:ok, &1)) do
      {:ok, module}
    else
      {:error, module, check_list
                        |> Enum.filter(&match?({:error, _}, &1))
                        |> Enum.map(fn {:error, msg} -> msg end)}
    end
  end
  
  
  def risk_acceptable?({module, _, _}, [module | _whitelist]), do: :ok
  def risk_acceptable?({module, function, _}, [{module, function} | _whitelist]), do: :ok
  def risk_acceptable?(mfa, [mfa | _whitelist]), do: :ok
  def risk_acceptable?(risk, [_not_that_risk | whitelist]), do: risk_acceptable?(risk, whitelist)
  
  def risk_acceptable?({:erlang, function, _}, []) do
    if MapSet.member?(@whitelisted_erlang_bifs, function) do
        :ok
    else
        {:error, ":erlang.#{Atom.to_string(function)} not whitelisted"}
    end
  end
  
  def risk_acceptable?(mfa = {module, function, arity}, []) do
    if MapSet.member?(@whitelisted_erlang_stdlib_modules, module) or
       MapSet.member?(@whitelisted_elixir_modules, module) or
       MapSet.member?(@whitelisted_elixir_functions, mfa) do
      :ok
    else
      {
        :error,
        "#{Atom.to_string(module)}.#{Atom.to_string(function)}/#{Integer.to_string(arity)} not whitelisted"
      }
    end
  end
  
  def risk_acceptable?(:send, []) do
    {:error, "send not allowed"}
  end
    
  def risk_acceptable?(:receive, []) do
    {:error, "receive not allowed"}
  end
    
  def risk_acceptable?(_, _), do: {:error, "unknown risk not whitelisted"}
  
  
  # TODO detect __STACKTRACE__/0, non literal apply arguments
  
  def module_risks(bytecode) when is_binary(bytecode) do
    case :beam_lib.chunks(bytecode, [:abstract_code]) do
      {:ok, {module, [abstract_code: code]}} ->
        {:ok,
          module,
          module_risks(code, MapSet.new())}
      _ ->
        {:error, :badarg}
    end
  end
  
  
  def module_risks(
        {:call, _line_no, {:remote, _, {:atom, _, :erlang}, {:atom, _, :send}}, args},
        acc
      ) do
    module_risks(
      args,
      acc
      |> MapSet.put(:send)
    )
  end
  
  def module_risks(
        {:call, _line_no, {:remote, _, {:atom, _, m}, {:atom, _, f}}, args},
        acc
      ) do
    module_risks(
      args,
      acc
      |> MapSet.put({m, f, length(args)})
    )
  end
  
  def module_risks({:receive, _line_no, clauses}, acc) do
    module_risks(
      clauses,
      acc
      |> MapSet.put(:receive)
    )
  end
  
  def module_risks([], acc), do: acc
  
  def module_risks([chunk | chunks], acc),
      do: module_risks(chunk, module_risks(chunks, acc))
      
  def module_risks(t, acc) when is_tuple(t),
      do: module_risks(Tuple.to_list(t), acc)
      
  def module_risks(_, acc), do: acc
end
