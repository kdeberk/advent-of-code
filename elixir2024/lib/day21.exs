defmodule Day21 do
  def day, do: 21
  def name, do: "Keypad Conundrum"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    contents
    |> String.split("\n")
  end

  def part1(keycodes) do
    enter_keycodes(keycodes, 2)
  end

  def part2(keycodes) do
    enter_keycodes(keycodes, 25)
  end

  def enter_keycodes(keycodes, n_robots) do
    lookup = shortest_paths()

    {score, _} =
      keycodes
      |> Enum.reduce({0, Map.new()}, fn keycode, {score, cache} ->
        numerical = Utils.parse_int(keycode)

        {sc, cache} =
          calc_path_length(
            ["A"] ++ String.graphemes(keycode),
            n_robots,
            lookup,
            cache
          )

        {sc * numerical + score, cache}
      end)

    score
  end

  def calc_path_length(path, depth, lookup, cache) do
    key = {path, depth}

    if Map.has_key?(cache, key) do
      {Map.get(cache, key), cache}
    else
      {score, cache} =
        path
        |> Enum.zip(tl(path))
        |> Enum.reduce({0, cache}, fn pair, {score, cache} ->
          path = Map.get(lookup, pair)
          if depth == 0 do
            {score + length(path), cache}
          else
            {sc, cache} = calc_path_length(["A"] ++ path, depth - 1, lookup, cache)
            {score + sc, cache}
          end
        end)

      {score, Map.put(cache, key, score)}
    end
  end

  def shortest_paths() do
    # Crafted by hand after 23:00 *chef's kiss*
    [
      {{"0", "2"}, "^A"},
      #
      {{"0", "3"}, "^>A"},
      #
      {{"0", "A"}, ">A"},
      #
      {{"1", "7"}, "^^A"},
      {{"2", "4"}, "<^A"},
      {{"2", "9"}, "^^>A"},
      {{"3", "7"}, "<<^^A"},
      #
      {{"3", "8"}, "<^^A"},
      #
      {{"3", "A"}, "vA"},
      {{"4", "5"}, ">A"},
      #
      {{"4", "6"}, ">>A"},
      {{"5", "6"}, ">A"},
      #
      {{"5", "A"}, "vv>A"},
      #
      {{"6", "3"}, "vA"},
      #
      {{"6", "5"}, "<A"},
      #
      {{"6", "A"}, "vvA"},
      #
      {{"7", "8"}, ">A"},
      {{"7", "9"}, ">>A"},
      #
      {{"8", "0"}, "vvvA"},
      #
      {{"8", "A"}, "vvv>A"},
      #
      {{"9", "6"}, "vA"},
      {{"9", "8"}, "<A"},
      {{"9", "A"}, "vvvA"},
      {{"A", "0"}, "<A"},
      {{"A", "1"}, "^<<A"},
      {{"A", "2"}, "<^A"},
      {{"A", "3"}, "^A"},
      {{"A", "4"}, "^^<<A"},
      {{"A", "6"}, "^^A"},
      {{"A", "7"}, "^^^<<A"},
      {{"A", "8"}, "<^^^A"},
      {{"A", "9"}, "^^^A"},
      {{"<", "v"}, ">A"},
      {{"<", "^"}, ">^A"},
      {{"<", ">"}, ">>A"},
      {{"<", "A"}, ">>^A"},
      {{"<", "<"}, "A"},
      {{"v", "<"}, "<A"},
      {{"v", "^"}, "^A"},
      {{"v", ">"}, ">A"},
      {{"v", "A"}, "^>A"},
      {{"v", "v"}, "A"},
      {{">", "<"}, "<<A"},
      {{">", "^"}, "<^A"},
      {{">", "v"}, "<A"},
      {{">", "A"}, "^A"},
      {{">", ">"}, "A"},
      {{"^", "<"}, "v<A"},
      {{"^", ">"}, "v>A"},
      {{"^", "v"}, "vA"},
      {{"^", "A"}, ">A"},
      {{"^", "^"}, "A"},
      {{"A", "<"}, "v<<A"},
      {{"A", ">"}, "vA"},
      {{"A", "v"}, "<vA"},
      {{"A", "^"}, "<A"},
      {{"A", "A"}, "A"}
    ]
    |> Enum.map(fn {k, v} -> {k, String.graphemes(v)} end)
    |> Map.new()
  end
end
