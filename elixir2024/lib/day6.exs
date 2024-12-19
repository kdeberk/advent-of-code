# TODO: cleanup
defmodule Day6 do
  def day, do: 6
  def name, do: "Guard Gallivant"

  def north, do: %{ dx:  0, dy: -1 }
  def south, do: %{ dx:  0, dy:  1 }
  def east, do:  %{ dx:  1, dy:  0 }
  def west, do:  %{ dx: -1, dy:  0 }

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    Grid.from_multiline_string(contents)
  end

  def right_turn(dir) do
    cond do
      dir == north() -> east()
      dir == east() -> south()
      dir == south() -> west()
      dir == west() -> north()
    end
  end

  def move(%{x: x, y: y}, %{dx: dx, dy: dy}) do
    %{ x: x+dx, y: y+dy}
  end

  def step(%{grid: grid}, pos, dir) do
    nxt = move(pos, dir)

    %{x: x, y: y} = nxt
    if Map.get(grid, [x,y]) != "#" do
      [nxt, dir]
    else
      dir = right_turn(dir)
      [pos, dir]
    end
  end

  def walk(map, pos, dir), do: walk(map, pos, dir, MapSet.new([pos]))
  def walk(map, pos, dir, visited) do
    [pos, dir] = step(map, pos, dir)
    %{x: x, y: y} = pos
    %{width: width, height: height} = map

    if 0 <= x && x < width && 0 <= y && y < height do
      walk(map, pos, dir, MapSet.put(visited, pos))
    else
      visited
    end
  end

  def find_guard(%{grid: grid}) do
    {{x, y}, _} = Enum.find(grid, fn ({_, v}) -> v == "^" end)
    %{x: x, y: y}
  end

  def part1(map) do
    guard = find_guard(map)
    visited = walk(map, guard, north())
    MapSet.size(visited)
  end

  def walk_avoid_loop(map, pos, dir), do: walk_avoid_loop(map, pos, dir, MapSet.new([pos]))
  def walk_avoid_loop(map, pos, dir, visited) do
    [pos, dir] = step(map, pos, dir)
    %{x: x, y: y} = pos
    %{width: width, height: height} = map

    cond do
      MapSet.member?(visited, [pos, dir]) ->
        "stuck"
      0 <= x && x < width && 0 <= y && y < height ->
        walk_avoid_loop(map, pos, dir, MapSet.put(visited, [pos, dir]))
      true ->
        visited
    end
  end


  def part2(map) do
    guard = find_guard(map)
    visited = walk(map, guard, north())

    visited
    |> Enum.count(fn (pos) ->
      %{grid: grid} = map
      %{x: x, y: y} = pos
      Map.get(grid, {x, y}) == "." &&
        walk_avoid_loop(%{map | grid: Map.put(grid, {x, y}, "#")}, guard, north()) == "stuck"
    end)
  end
end
