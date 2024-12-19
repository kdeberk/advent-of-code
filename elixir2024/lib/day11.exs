Code.require_file("utils.exs")

defmodule Day11 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day11.txt")

    contents
    |> String.split(" ")
    |> Enum.map(&Utils.parse_int/1)
  end

  def blink(n, stone, cache) do
    str = inspect(stone)
    len = String.length(str)
    key = {n, stone}

    cond do
      n == 0 ->
        {1, cache}

      Map.has_key?(cache, key) ->
        {Map.get(cache, key), cache}

      stone == 0 ->
        {sol, cache} = blink(n - 1, 1, cache)
        {sol, Map.put(cache, key, sol)}

      rem(len, 2) == 0 ->
        h = floor(len / 2)
        {sol1, cache1} = blink(n - 1, Utils.parse_int(String.slice(str, 0..(h - 1))), cache)
        {sol2, cache2} = blink(n - 1, Utils.parse_int(String.slice(str, h..(len - 1))), cache1)
        {sol1 + sol2, Map.put(cache2, key, sol1 + sol2)}

      true ->
        {sol, cache} = blink(n - 1, stone * 2024, cache)
        {sol, Map.put(cache, key, sol)}
    end
  end

  def blink_stones(n, stones) do
    {sol, _} =
      stones
      |> Enum.reduce(
        {0, Map.new()},
        fn stone, {sol, cache} ->
          {sol_, cache_} = blink(n, stone, cache)
          {sol + sol_, cache_}
        end
      )

    sol
  end

  def part1(stones) do
    blink_stones(25, stones)
  end

  def part2(stones) do
    blink_stones(75, stones)
  end
end

input = Day11.prepare_input()
IO.puts("Day 11: Plutonian Pebbles")
IO.puts("Part 1: #{Day11.part1(input)}")
IO.puts("Part 2: #{Day11.part2(input)}")
