Code.require_file("utils.exs")

defmodule Day12 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day12.txt")

    contents
    |> Grid.from_multiline_string()
    |> walk_regions
  end

  def neighbors({x, y}) do
    for {dx, dy} <- [{-1, 0}, {1, 0}, {0, -1}, {0, 1}], do: {x + dx, y + dy}
  end

  def boundary_line({px, py}, {cx, cy}) do
    {dx, dy} = {cx - px, cy - py}
    {px + 0.4 * dx, py + 0.4 * dy}
  end

  def walk_region(result, farm, id, prev, cur) do
    inc = fn x -> x + 1 end
    visited_before = Map.has_key?(result, cur)
    same_region = Grid.char_at(farm, id) == Grid.char_at(farm, cur)

    cond do
      visited_before && same_region ->
        result

      same_region ->
        result =
          result
          |> Map.put(cur, id)
          |> Map.update!({id, :area}, inc)

        neighbors(cur)
        |> Enum.reduce(
          result,
          fn neighbor, acc ->
            walk_region(acc, farm, id, cur, neighbor)
          end
        )

      true ->
        # Walked outside the region, so we crossed a perimeter
        result
        |> Map.update!({id, :perimeter}, inc)
        |> Map.update!({id, :boundary}, fn set -> MapSet.put(set, boundary_line(prev, cur)) end)
    end
  end

  def walk_regions(farm) do
    Grid.coordinates(farm)
    |> Enum.reduce(
      %{ids: MapSet.new},
      fn pos, acc ->
        if Map.has_key?(acc, pos) do
          acc
        else
          id = pos

          acc
          |> Map.put({id, :area}, 0)
          |> Map.put({id, :perimeter}, 0)
          |> Map.put({id, :boundary}, MapSet.new())
          |> Map.update!(:ids, fn set -> MapSet.put(set, id) end)
          |> walk_region(farm, id, nil, pos)
        end
      end
    )
  end

  def part1(walked) do
    walked[:ids]
    |> Enum.map(fn id -> walked[{id, :area}] * walked[{id, :perimeter}] end)
    |> Enum.sum()
  end

  def count_disjoint_ranges([]), do: 0
  def count_disjoint_ranges([_n]), do: 1

  def count_disjoint_ranges([n | ns]) do
    if n + 1 == hd(ns) do
      count_disjoint_ranges(ns)
    else
      1 + count_disjoint_ranges(ns)
    end
  end

  def count_sides(boundary) do
    boundary
    |> Enum.reduce(%{}, fn {x, y}, acc ->
      # Group points alongside the horizontal or vertical lines
      if floor(x) == x do
        Map.update(acc, {nil, y}, [x], fn xs -> xs ++ [x] end)
      else
        Map.update(acc, {x, nil}, [y], fn ys -> ys ++ [y] end)
      end
    end)
    |> Enum.map(fn {_, coords} -> count_disjoint_ranges(Enum.sort(coords)) end)
    |> Enum.sum()
  end

  def part2(walked) do
    walked[:ids]
    |> Enum.map(fn id -> walked[{id, :area}] * count_sides(walked[{id, :boundary}]) end)
    |> Enum.sum()
  end
end

input = Day12.prepare_input()
IO.puts("Day 12: Garden Groups")
IO.puts("Part 1: #{Day12.part1(input)}")
IO.puts("Part 2: #{Day12.part2(input)}")
