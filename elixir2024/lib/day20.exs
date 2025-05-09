Code.require_file("lib/utils.exs")

defmodule Day20 do
  def day, do: 20
  def name, do: "Race Condition"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    grid = Grid.from_multiline_string(contents)

    grid
    |> Map.put(:start, Grid.find(grid, fn {_, tile} -> tile == "S" end))
    |> Map.put(:end, Grid.find(grid, fn {_, tile} -> tile == "E" end))
  end

  def bfs(maze, queue, distances) do
    if Queue.len(queue) == 0 do
      distances
    else
      {{cur, d}, queue} = Queue.pop(queue)
      cond do
        Map.has_key?(distances, cur) ->
          bfs(maze, queue, distances)
        Grid.at(maze, cur) == "#" ->
          bfs(maze, queue, distances)
        true ->
          {queue, distances} = Grid.neighbors(maze, cur)
          |> Enum.reduce({queue, distances}, fn pos, {queue, distances} ->
            if Map.has_key?(distances, pos) do
              {queue, distances}
            else
              {Queue.push(queue, {pos, d+1}), distances}
            end
          end)

          bfs(maze, queue, Map.put(distances, cur, d))
      end
    end
  end

  def determine_distances(maze, from) do
    %{end: end_} = maze

    bfs(maze, Queue.new |> Queue.push({from, 0}), Map.new)
  end

  def part1(maze) do
    %{end: end_, start: start} = maze
    from_end = determine_distances(maze, end_)

    jumps = [{-2, 0}, {-1, -1}, {-1, 1}, {0, -2}, {0, 2}, {1, -1}, {1, 1}, {2, 0}]

    Grid.coordinates(maze)
    |> Enum.filter(fn xy -> Grid.at(maze, xy) != "#" end)
    |> Enum.map(fn xy ->
      normal_dist = Map.get(from_end, xy)

      {x, y} = xy
      jumps
      |> Enum.map(fn {dx, dy} -> {x+dx, y+dy} end)
      |> Enum.map(fn xy_ -> {xy, xy_, normal_dist, Map.get(from_end, xy_)} end)
      |> Enum.count(fn {_, _, d, d_} -> d_ <= (d - 102) end)
    end)
    |> Enum.sum
  end

  def dist({ax, ay}, {bx, by}) do
    abs(ax-bx) + abs(ay-by)
  end

  def part2(maze) do
    %{end: end_} = maze
    from_end = determine_distances(maze, end_)

    from_end
    |> Enum.map(fn {xy, d} ->
      from_end
      |> Enum.count(fn {xy_, d_} ->
        cheat = dist(xy, xy_)
        cheat <= 20 && 100 <= (d_-d-cheat)
      end)
    end)
    |> Enum.sum
  end
end
