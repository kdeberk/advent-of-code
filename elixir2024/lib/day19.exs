defmodule Day19 do
  def day, do: 19
  def name, do: "Linen Layout"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    [towels, designs] =
      contents
      |> String.split("\n\n")

    towels = towels
    |> String.split(", ")
    |> MapSet.new()

    designs = designs
    |> String.split("\n")

    cache = designs
    |> Enum.reduce(Map.new([{"", 1}]), fn design, cache ->
      count(design, towels, cache)
    end)

    %{designs: designs, cache: cache }
  end

  def count(design, towels, cache) do
    if Map.has_key?(cache, design) do
      cache
    else
      suffixes = towels
      |> Enum.filter(fn towel -> String.starts_with?(design, towel) end)
      |> Enum.map(fn towel -> String.slice(design, String.length(towel), String.length(design)) end)

      cache = suffixes
      |> Enum.reduce(cache, fn suffix, cache ->
        count(suffix, towels, cache)
      end)

      c = suffixes
      |> Enum.map(fn suffix -> Map.get(cache, suffix, 0) end)
      |> Enum.sum

      Map.put(cache, design, c)
    end
  end

  def part1(%{designs: designs, cache: cache}) do
    designs
    |> Enum.count(fn design -> Map.has_key?(cache, design) end)
  end

  def part2(%{designs: designs, cache: cache}) do
    designs
    |> Enum.map(fn design -> Map.get(cache, design, 0) end)
    |> Enum.sum
  end
end
