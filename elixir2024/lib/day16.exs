
defmodule Day16 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day16/puzzle.txt")

    grid = Grid.from_multiline_string(contents)

    grid
    |> Map.put(:start, Grid.find(grid, fn {_, v} -> v == "S" end))
    |> Map.put(:end, Grid.find(grid, fn {_, v} -> v == "E" end))
  end

  def neighbors(pos, dir, score) do
    [{pos, Direction.left(dir), score+1000},
     {Grid.move_point(pos, dir), dir, score+1},
     {pos, Direction.right(dir), score+1000}]
  end

  def walk_back(maze, parents, cur, result) do
    if MapSet.member?(result, cur) do
      result
    else
      Map.get(parents, cur)
      |> Enum.reduce(result, fn par, result ->
        walk_back(maze, parents, par, MapSet.put(result, cur))
      end)
    end
  end

  def find_best_paths(maze, heap, visited, parents) do
    {pos, dir, score} = MinHeap.peek(heap)
    heap = MinHeap.pop(heap)

    if pos == maze[:end] do
      path = walk_back(maze, parents, {pos, dir}, MapSet.new)
      |> Enum.map(fn {pos, _dir} -> pos end)
      |> MapSet.new

      {score, path}
    else
      {heap, visited, parents} = neighbors(pos, dir, score)
      |> Enum.reduce({heap, visited, parents}, fn {p, d, score}, {heap, vis, pars} ->
        key = {p, d}

        cond do
          Grid.at(maze, p) == "#" ->
            # nei is a wall, ignore this spot
            {heap, vis, pars}

          !Map.has_key?(vis, key) ->
            # Haven't visited this spot in this direction before
            {MinHeap.push(heap, score, {p, d, score}),
             Map.put(vis, key, score),
             Map.put(pars, key, [{pos, dir}])}

          score < Map.get(vis, key) ->
            # Visited before, but current score is better than previous one
            {MinHeap.push(heap, score, {p, d, score}),
             Map.put(vis, key, score),
             Map.put(pars, key, [{pos, dir}])}

          score == Map.get(vis, key) ->
            {heap,
             vis,
             Map.update!(pars, key, fn ps -> ps ++ [{pos, dir}] end)}

          true ->
            # Visited before with a better score.
            {heap, vis, pars}
        end
      end)

      find_best_paths(maze, heap, visited, parents)
    end
  end

  def heap(%{start: start}) do
    MinHeap.new
    |> MinHeap.push(0, {start, Direction.east(), 0})
  end

  def part1(maze) do
    {score, _} = find_best_paths(maze, heap(maze), Map.new, Map.new)
    score
  end

  def part2(maze) do
    {_, path} = find_best_paths(maze, heap(maze), Map.new, Map.new)
    MapSet.size(path)
  end
end
