defmodule Day3 do
  def day, do: 3
  def name, do: "Mull It Over"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)
    contents
  end

  # Capture all sum(m, n) lines, multiple the numbers and sum the result.
  def part1(program) do
    Regex.scan(~r/mul\((\d+),(\d+)\)/, program)
    |> Enum.map(fn [_, m, n] ->
      Utils.parse_int(m) * Utils.parse_int(n)
    end)
    |> Enum.sum()
  end

  # Capture all sums, do's and don'ts. Keep :active state and only increment :sum when
  #  active is true.
  def part2(program) do
    Regex.scan(~r/(mul)\((\d+),(\d+)\)|do\(\)|don't\(\)/, program)
    |> Enum.reduce(%{active: true, sum: 0}, fn match, acc ->
      case match do
        [_, "mul", m, n] ->
          if not acc[:active] do
            acc
          else
            product = Utils.parse_int(m) * Utils.parse_int(n)
            Map.update(acc, :sum, product, fn x -> x + product end)
          end

        ["do()"] ->
          Map.put(acc, :active, true)

        ["don't()"] ->
          Map.put(acc, :active, false)
      end
    end)
    |> Map.get(:sum)
  end
end
