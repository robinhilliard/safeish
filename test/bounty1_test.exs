defmodule Bounty1 do
  @moduledoc """
  I, Robin Hilliard, will pay the submitter of the first pull request to
  my github repo for this project that causes this test to pass by
  changing the contents of `Elixir.Bounty1.beam` AND NOTHING ELSE 100 USD.
  Payment will be made by PayPal.
  
  The current beam file was compiled from this source:
  
  ```
  defmodule Bounty1 do
    def guess(), do: -1
  end
  ```
  
  You can use Erlang, LFE or anything you like to generate the beam file, but
  this must be done separately to the test run (the idea is that the BEAM file
  was generated somewhere else and that Safeish is testing and loading it here).
  
  Curious to see what people can do, I am by no means across all the ins and outs
  of the BEAM and there may be an easy exploit - good luck :-).
  """
  use ExUnit.Case
  
  
  test "checked code can guess the secret" do
    <<secret::128>> = :crypto.strong_rand_bytes(16)
    Safeish.load_file("test/Elixir.Bounty1.beam")
    assert ^secret = Bounty1.guess()
  end
  
  
end
