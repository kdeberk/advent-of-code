Code.require_file("utils.exs")

defmodule Day7 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day7.txt")

    contents
    |> String.split("\n")
    |> Enum.map(fn line ->
      [sol, parts] = String.split(line, ": ")
      parts = String.split(parts, " ")
      |> Enum.map(&Utils.parse_int/1)

      %{solution: Utils.parse_int(sol), parts: parts}
    end)
  end

  def solvable_1?(parts, cur, solution)
  def solvable_1?([], cur, solution), do: cur == solution
  def solvable_1?([el | tl], cur, solution) do
    cur < solution &&
      (solvable_1?(tl, cur * el, solution) ||
        solvable_1?(tl, cur + el, solution))
  end

  def part1(input) do
    input
    |> Enum.filter(fn %{solution: solution, parts: [hd | tl]} ->
      solvable_1?(tl, hd, solution)
    end)
    |> Enum.map(fn %{solution: solution} -> solution end)
    |> Enum.sum
  end

  def solvable_2?(parts, cur, solution)
  def solvable_2?([], cur, solution), do: cur == solution
  def solvable_2?([el | tl], cur, solution) do
    cur < solution &&
      (solvable_2?(tl, cur * el, solution) ||
        solvable_2?(tl, cur + el, solution) ||
        solvable_2?(tl, Utils.parse_int(~s(#{cur}#{el})), solution))
  end

  def part2(input) do
    input
    |> Enum.filter(fn %{solution: solution, parts: [hd | tl]} ->
      solvable_2?(tl, hd, solution)
    end)
    |> Enum.map(fn %{solution: solution} -> solution end)
    |> Enum.sum
  end
end

input = Day7.prepare_input()
IO.puts("Day 7: Bridge Repair")
IO.puts("Part 1: #{Day7.part1(input)}")
IO.puts("Part 2: #{Day7.part2(input)}")
