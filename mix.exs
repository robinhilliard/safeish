defmodule Safeish.MixProject do
  use Mix.Project

  def project do
    [
      app: :safeish,
      version: "0.1.0",
      elixir: "~> 1.10",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
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
    [
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
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
