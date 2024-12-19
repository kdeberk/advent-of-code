Code.require_file("utils.exs")

defmodule Day13 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day13.txt")

    contents
    |> String.split("\n\n")
    |> Enum.map(fn block ->
      [{ax, ay}, {bx, by}, {cx, cy}] =
        block
        |> String.split("\n")
        |> Enum.map(fn line ->
          [[x], [y]] = Regex.scan(~r/\d+/, line)
          {Utils.parse_int(x), Utils.parse_int(y)}
        end)

      [{ax, bx, cx}, {ay, by, cy}]
    end)
  end

  # The inputs form a pair of linear diophantine equations.
  def solve({a, b, c}, {d, e, f}) do
    x = (b * f - c * e) / (b * d - a * e)
    y = (c * d - a * f) / (b * d - a * e)

    fx = floor(x)
    fy = floor(y)

    if x == fx && y == fy do
      {fx, fy}
    else
      {nil, nil}
    end
  end

  def solve_all(equations) do
    Enum.reduce(equations, 0, fn [eq1, eq2], acc ->
      {a, b} = solve(eq1, eq2)

      if a && b do
        acc + (3 * a + b)
      else
        acc
      end
    end)
  end

  def part1(equations) do
    solve_all(equations)
  end

  def part2(equations) do
    equations
    |> Enum.map(fn [{a, b, c}, {d, e, f}] ->
      [{a, b, c + 10_000_000_000_000}, {d, e, f + 10_000_000_000_000}]
    end)
    |> solve_all
  end
end

input = Day13.prepare_input()
IO.puts("Day 13: Claw Contraption")
IO.puts("Part 1: #{Day13.part1(input)}")
IO.puts("Part 2: #{Day13.part2(input)}")
