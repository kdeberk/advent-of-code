defmodule Day4 do
  def day, do: 4
  def name, do: "Ceres Search"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)
    Grid.from_multiline_string(contents)
  end

  def part1(puzzle) do
    xmas_at? = fn {{x, y}, {dx, dy}} ->
      Grid.at(puzzle, {x + dx * 0, y + dy * 0}) == "X" &&
        Grid.at(puzzle, {x + dx * 1, y + dy * 1}) == "M" &&
        Grid.at(puzzle, {x + dx * 2, y + dy * 2}) == "A" &&
        Grid.at(puzzle, {x + dx * 3, y + dy * 3}) == "S"
    end

    for(xy <- Grid.coordinates(puzzle), dx <- -1..1, dy <- -1..1, do: {xy, {dx, dy}})
    |> Enum.count(xmas_at?)
  end

  def part2(puzzle) do
    mas_at? = fn {x, y}, {dx, dy} ->
      Grid.at(puzzle, {x + dx * -1, y + dy * -1}) == "M" &&
        Grid.at(puzzle, {x + dx * 0, y + dy * 0}) == "A" &&
        Grid.at(puzzle, {x + dx * 1, y + dy * 1}) == "S"
    end

    x_mas_at? = fn xy ->
      Grid.at(puzzle, xy) == "A" &&
        for(dx <- [-1, 1], dy <- [-1, 1], do: {dx, dy})
        |> Enum.count(fn dxy -> mas_at?.(xy, dxy) end) == 2
    end

    Grid.coordinates(puzzle)
    |> Enum.count(fn xy -> x_mas_at?.(xy) end)
  end
end
