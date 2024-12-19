Code.require_file("lib/utils.exs")

defmodule Day14 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day14/puzzle.txt")

    robots = contents
    |> String.split("\n")
    |> Enum.map(fn line ->
      [px, py, vx, vy] =
        Regex.scan(~r/-?\d+/, line)
        |> Enum.map(fn [m] -> Utils.parse_int(m) end)
      {{px, py}, {vx, vy}}
    end)

    %{robots: robots, width: 101, height: 103}
  end

  def take_n_steps(robot, %{width: width, height: height}, n) do
    {{px, py}, {vx, vy}} = robot

    {{ Integer.mod(px + vx*n, width), Integer.mod(py + vy*n, height) }, {vx, vy}}
  end

  def quadrant({x, y}, %{width: width, height: height}) do
    mx = floor(width / 2)
    my = floor(height / 2)
    cond do
      x < mx && y < my -> "NW"
      x < mx && my < y -> "SW"
      mx < x && y < my -> "NE"
      mx < x && my < y -> "SE"
      true -> nil
    end
  end

  def part1(input) do
    input[:robots]
    |> Enum.map(fn robot -> take_n_steps(robot, input, 100) end)
    |> Enum.map(fn {xy, _} -> quadrant(xy, input) end)
    |> Enum.filter(fn q -> q end)
    |> Enum.reduce(%{}, fn q, acc -> Map.update(acc, q, 1, fn x -> x + 1 end) end)
    |> Map.values
    |> Enum.product
  end

  def render_grid(robots, %{width: width, height: height}) do
    coords = robots
    |> Enum.map(fn {xy, _} -> xy end)
    |> MapSet.new

    (for y <- 0..(height-1), do: (
      (for x <- 0..(width-1), do: (
        if MapSet.member?(coords, {x,y}) do
          "X"
        else
          "."
        end
      ))
      |> Enum.join
    ))
    |> Enum.join("\n")
  end

  def find_tree(robots, input, seen) do
    rendered = render_grid(robots, input)
    if MapSet.member?(seen, rendered) do
      ~s(Rendered all of them, size: #{MapSet.size(seen)})
    else
      n = MapSet.size(seen)
      if Integer.mod(n, 103) == 76 do
        IO.puts MapSet.size(seen)
        IO.puts render_grid(robots, input)
      end

      find_tree(
        Enum.map(robots, fn robot -> take_n_steps(robot, input, 1) end),
        input,
        MapSet.put(seen, rendered)
      )
    end
  end

  def part2(input) do
    find_tree(input[:robots], input, MapSet.new)
  end
end
