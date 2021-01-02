defmodule Safeish.MixProject do
  use Mix.Project

  def project do
    [
      app: :safeish,
      version: "0.5.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      deps: deps(),
      aliases: aliases(),
      source_url: "https://github.com/robinhilliard/safeish",
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [{:ex_doc, ">= 0.0.0", only: :dev, runtime: false}]
  end
  
  defp description() do
    "NOT FOR PRODUCTION USE: Safe-ish is an experimental sandbox for BEAM modules that examines and rejects BEAM bytecode containing instructions that could cause side effects. You can provide an optional whitelist of opcodes and functions the module can use."
  end
  
  defp package() do
    [
      name: "safeish",
      files: ["lib", "mix.exs", "README.md", "LICENSE"],
      exclude_patterns: [".DS_Store"],
      licenses: ["MIT"],
      links: %{"Github" => "https://github.com/robinhilliard/safeish"},
      maintainers: ["Robin Hilliard"]
    ]
  end
  
  defp aliases, do: [fixtures: &make_fixtures/1]
  
  defp make_fixtures(_) do
    source_path =  "#{File.cwd!()}/test/fixtures.ex"
    build_path = "#{File.cwd!()}/test/fixtures_build"
    
    Mix.Shell.IO.info("Compiling modules in #{source_path} and saving to #{build_path}")
    
    File.rm_rf(build_path)
    File.mkdir(build_path)
    
    for {module, bytecode} <- Code.compile_file(source_path) do
      Mix.Shell.IO.info("...#{build_path}/#{Atom.to_string(module)}.beam")
      {:ok, file} = File.open("#{build_path}/#{Atom.to_string(module)}.beam", [:write])
      IO.binwrite(file, bytecode)
      File.close(file)
    end
    
  end
end
