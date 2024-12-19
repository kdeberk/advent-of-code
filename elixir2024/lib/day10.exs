Code.require_file("utils.exs")

defmodule Day10 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day10.txt")

    map = Grid.from_multiline_string(contents)

    grid =
      map[:grid]
      |> Enum.map(fn {k, h} -> {k, Utils.parse_int(h)} end)
      |> Enum.into(%{})

    %{map | grid: grid}
  end

  def neighbors(%{grid: grid, width: width, height: height}, {x, y, z}) do
    [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
    |> Enum.map(fn {dx, dy} ->
      {x + dx, y + dy, grid[{x + dx, y + dy}]}
    end)
    |> Enum.filter(fn {x_, y_, z_} ->
      0 <= x_ && x_ < width && 0 <= y_ && y_ < height && z + 1 == z_
    end)
  end

  def count_reachable_peaks(_, [], visited) do
    Enum.count(visited, fn {_, _, z} -> z == 9 end)
  end

  def count_reachable_peaks(map, poss, visited) do
    next =
      poss
      |> Enum.flat_map(fn xyz -> neighbors(map, xyz) end)

    count_reachable_peaks(map, next, MapSet.union(visited, MapSet.new(next)))
  end

  def part1(map) do
    map[:grid]
    |> Enum.filter(fn {_xy, z} -> z == 0 end)
    |> Enum.map(fn {{x, y}, z} -> count_reachable_peaks(map, [{x, y, z}], MapSet.new()) end)
    |> Enum.sum()
  end

  # Don't keep track of loops as path can only go upwards so loops back to lower place are not possible.
  def count_unique_paths(_m, {_, _, 9}), do: 1

  def count_unique_paths(map, pos) do
    neighbors(map, pos)
    |> Enum.map(fn n -> count_unique_paths(map, n) end)
    |> Enum.sum()
  end

  def part2(map) do
    map[:grid]
    |> Enum.filter(fn {_xy, z} -> z == 0 end)
    |> Enum.map(fn {{x, y}, z} -> count_unique_paths(map, {x, y, z}) end)
    |> Enum.sum()
  end
end

input = Day10.prepare_input()
IO.puts("Day 10: Hoof It")
IO.puts("Part 1: #{Day10.part1(input)}")
IO.puts("Part 2: #{Day10.part2(input)}")
