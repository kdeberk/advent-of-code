defmodule Day5 do
  def day, do: 5
  def name, do: "Print Queue"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    [rules, updates] = contents
    |> String.split("\n\n")
    |> Enum.map(fn block -> String.split(block,"\n") end)

    updates = updates
    |> Enum.map(fn update -> String.split(update, ",") end)

    %{rules: MapSet.new(rules), updates: updates}
  end

  def valid_order?(rules, a, b) do
    !MapSet.member?(rules, b <> "|" <> a)
  end

  def valid_update?(_rules, []), do: true
  def valid_update?(rules, [hd | tl]) do
    valid_update?(rules, tl) &&
      Enum.all?(tl, fn(el) -> valid_order?(rules, hd, el) end)
  end

  def correct_update(_rules, []), do: []
  def correct_update(rules, update) do
    nxt = update
    |> Enum.find(fn candidate ->
      Enum.all?(update, fn other -> valid_order?(rules, candidate, other) end)
    end)

    [nxt] ++ correct_update(rules, update--[nxt])
  end

  def middle(lst) do
    Enum.at(lst, floor(length(lst)/2))
  end

  def part1(%{rules: rules, updates: updates}) do
    updates
    |> Enum.filter(fn update -> valid_update?(rules, update) end)
    |> Enum.map(&middle/1)
    |> Enum.map(&Utils.parse_int/1)
    |> Enum.sum
  end

  def part2(%{rules: rules, updates: updates}) do
    updates
    |> Enum.filter(fn update -> !valid_update?(rules, update) end)
    |> Enum.map(fn update -> correct_update(rules, update) end)
    |> Enum.map(&middle/1)
    |> Enum.map(&Utils.parse_int/1)
    |> Enum.sum
  end
end
