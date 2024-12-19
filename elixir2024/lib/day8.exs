Code.require_file("utils.exs")

defmodule Day8 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day8.txt")

    %{grid: grid, width: width, height: height} = Utils.multiline_string_to_grid(contents)

    antennas = grid
    |> Enum.filter(fn {_pos, v} -> v != "." end)
    |> Enum.reduce(%{}, fn ({pos, antenna}, acc) ->
      Map.update(acc, antenna, [pos], fn poss -> poss ++ [pos] end)
    end)

    %{ antennas: antennas, width: width, height: height }
  end

  def part1(%{antennas: antennas, width: width, height: height}) do
    antennas
    |> Enum.flat_map(fn {_antenna, poss} ->
      for [a, b] <- Utils.combinations(2, poss), do: (
        [x, y] = a
        [x_, y_] = b
        dx = x - x_
        dy = y - y_
        MapSet.new([[x+dx,y+dy], [x_-dx,y_-dy]])
      )
    end)
    |> Enum.reduce(MapSet.new, &MapSet.union/2)
    |> Enum.filter(fn [x, y] -> 0 <= x && x < width && 0 <= y && y < height end)
    |> length
  end

  def follow_path(x, y, dx, dy, width, height) do
    if 0 <= x && x < width && 0 <= y && y < height do
      [[x, y]] ++ follow_path(x+dx,y+dy, dx,dy,width,height)
    else
      []
    end
  end

  def part2(%{antennas: antennas, width: width, height: height}) do
    antennas
    |> Enum.flat_map(fn {_antenna, poss} ->
      for [a, b] <- Utils.combinations(2, poss), do: (
        [x, y] = a
        [x_, y_] = b
        dx = x - x_
        dy = y - y_
        MapSet.new(
          follow_path(x,y,dx,dy,width,height) ++
            follow_path(x,y,-dx,-dy,width,height))
      )
    end)
    |> Enum.reduce(MapSet.new, &MapSet.union/2)
    |> MapSet.size
  end
end

input = Day8.prepare_input()
IO.puts("Day 8: Resonant Collinearity")
IO.puts("Part 1: #{Day8.part1(input)}")
IO.puts("Part 2: #{Day8.part2(input)}")
