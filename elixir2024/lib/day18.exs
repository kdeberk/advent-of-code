Code.require_file("lib/utils.exs")

defmodule Day18 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day18/puzzle.txt")

    bytes = contents
    |> String.split("\n")
    |> Enum.map(fn xy -> xy |> String.split(",") |> Enum.map(&Utils.parse_int/1) end)

    # TODO: detect if we're reading test file or puzzle.txt

    %{bytes: bytes, start: {0,0}, dst: {70,70}, width: 71, height: 71}
    # %{bytes: bytes, start: {0,0}, dst: {6,6}, width: 7, height: 7}
  end


  def manhattan_dist({x, y}, {x_, y_}), do: abs(x-x_) + abs(y-y_)

  def find_path(computer, heap, visited) do
    if MinHeap.size(heap) == 0 do
      nil
    else
      {pos, len} = MinHeap.peek(heap)
      heap = MinHeap.pop(heap)

      if pos == computer[:dst] do
        len
      else
        {heap, visited} = Grid.neighbors(computer, pos)
        |> Enum.reduce({heap, visited}, fn nei, {heap, visited} ->

          len_ = len + 1
          score = len_ + manhattan_dist(nei, computer[:dst])

          cond do
            Grid.at(computer, nei) == "#" ->
              {heap, visited}

            !Map.has_key?(visited, nei) || score < Map.get(visited, nei) ->
              # Either first time visit or visited before with longer path
              {MinHeap.push(heap, score, {nei, len_}),
               Map.put(visited, nei, score)}

            true ->
              {heap, visited}
          end
        end)

        find_path(computer, heap, visited)
      end
    end
  end

  def drop_first_n_bytes(computer, n) do
    grid = computer[:bytes]
    |> Enum.slice(0..(n-1))
    |> Enum.map(fn [x,y] -> {{x, y}, "#"} end)
    |> Map.new

    computer
    |> Map.put(:grid, grid)
  end

  def part1(computer) do
    computer = drop_first_n_bytes(computer, 21)
    heap = MinHeap.new
    |> MinHeap.push(0, {computer[:start], 0})

    find_path(computer, heap, Map.new)
  end

  def part2(computer) do
    # TODO: don't need to calculate which path to take, just that a path is impossible

    # Can we do this in a single A* sweep?
    #

    n = 1024..length(computer[:bytes])
    |> Enum.find(fn n ->
      computer = drop_first_n_bytes(computer, n)
      heap = MinHeap.new
      |> MinHeap.push(0, {computer[:start], 0})

      find_path(computer, heap, Map.new) == nil
    end)

    n
  end
end
