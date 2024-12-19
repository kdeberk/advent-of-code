defmodule Day1 do
  def day, do: 1
  def name, do: "Historian Hysteria"

  def prepare_input(path) do
    IO.puts(path)
    {:ok, contents} = File.read(path)

    contents
    |> String.split("\n")
    # Each line consists of two numbers separates by three spaces. Convert each line to an integer pair.
    |> Enum.map(fn line ->
      String.split(line, "   ")
      |> Enum.map(&Utils.parse_int/1)
    end)
    # The input is specified as rows of two columns, but the values are easier to work with if we the columns into two rows.
    |> Matrix.transpose()
  end

  # count_items returns map storing each unique item in lst along with the number of times it appeared in lst.
  def count_items(lst) do
    inc = fn n -> n + 1 end

    Enum.reduce(lst, %{}, fn el, acc ->
      Map.update(acc, el, 1, inc)
    end)
  end

  # Sort both columns and sum the differences between the matching numbers
  def part1(cols) do
    cols
    |> Enum.map(&Enum.sort/1)
    |> List.zip()
    |> Enum.map(fn {a, b} -> abs(a - b) end)
    |> Enum.sum()
  end

  # Find the common items and sum the similarity score for each common item
  def part2([col_a, col_b]) do
    counts_a = count_items(col_a)
    counts_b = count_items(col_b)

    score = fn id ->
      id * counts_a[id] * counts_b[id]
    end

    MapSet.intersection(MapSet.new(col_a), MapSet.new(col_b))
    |> Enum.map(score)
    |> Enum.sum()
  end
end
