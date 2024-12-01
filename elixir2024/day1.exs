defmodule Matrix do
  def transpose([[] | _]), do: []

  def transpose(m) do
    [Enum.map(m, &hd/1) | transpose(Enum.map(m, &tl/1))]
  end
end

defmodule Day1 do
  def parse_int(str) do
    {int, _frac} = Integer.parse(str)
    int
  end

  # count_items returns map storing each unique item in lst along with the number of times it appeared in lst.
  def count_items(lst) do
    inc = fn (n) -> n+1 end

    Enum.reduce(lst, %{}, fn (el, acc) ->
      Map.update(acc, el, 1, inc)
    end)
  end

  def prepare_input() do
    {:ok, contents} = File.read("input/day1.txt")

    contents
    |> String.split("\n")
    # Each line consists of two numbers separates by three spaces. Convert each line to an integer pair.
    |> Enum.map(fn line ->
      String.split(line, "   ")
      |> Enum.map(&parse_int/1)
    end)
    # The input is specified as rows of two columns, but the values are easier to work with if we the columns into two rows.
    |> Matrix.transpose()
  end

  # Sort both columns and sum the differences between the matching numbers
  def part1(cols) do
    cols
    |> Enum.map(&Enum.sort/1)
    |> List.zip
    |> Enum.map(fn ({a, b}) -> abs(a-b) end)
    |> Enum.sum
  end

  # Find the common items and sum the similarity score for each common item
  def part2([col_a, col_b]) do
    counts_a = count_items(col_a)
    counts_b = count_items(col_b)
    score = fn (id) ->
      id * counts_a[id] * counts_b[id]
    end

    MapSet.intersection(MapSet.new(col_a), MapSet.new(col_b))
    |> Enum.map(score)
    |> Enum.sum
  end
end

input = Day1.prepare_input()
IO.puts "Day 1: Historian Hysteria"
IO.puts "Part 1: #{Day1.part1(input)}"
IO.puts "Part 2: #{Day1.part2(input)}"
