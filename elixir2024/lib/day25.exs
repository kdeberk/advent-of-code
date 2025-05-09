defmodule Day25 do
  def day, do: 25
  def name, do: "Code Chronicle"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    contents
    |> String.split("\n\n")
    |> Enum.map(fn schematic ->
      schematic
      # Convert to grid where each pair {x,y} maps to either # or .
      |> Grid.from_multiline_string
      |> Map.get(:grid)
      # Extract the {x,y} coords that point to #
      |> Enum.filter(fn {_, v} -> v == "#" end)
      |> Enum.map(fn {k, _} -> k end)
      # Put that in a map
      |> MapSet.new
    end)
  end

  def part1(schematics) do
    Utils.combinations(2, schematics)
    # Don't need to consider whether they are locks or keys or what the peaks are. Only
    #  need to check whether they 'fit' into each other by not having any overlapping points
    |> Enum.count(fn [a, b] -> MapSet.disjoint?(a, b) end)
  end

  def part2(_schematics) do
  end
end
