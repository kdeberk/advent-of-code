defmodule Day2 do
  def day, do: 2
  def name, do: "Red-Nosed Reports"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    contents
    |> String.split("\n")
    |> Enum.map(fn line ->
      String.split(line, " ")
      |> Enum.map(&Utils.parse_int/1)
    end)
  end

  # safe_report divides the report up into pairs of adjacent elements. Each pair need to be in a
  #  specific order with some distance between them.
  def safe_report?(report) do
    check = fn lst ->
      Enum.zip(lst, tl(lst))
      |> Enum.reject(fn {a, b} -> a < b && b - a <= 3 end)
      |> Enum.count() == 0
    end

    check.(report) || check.(Enum.reverse(report))
  end

  def part1(reports) do
    Enum.count(reports, &safe_report?/1)
  end

  # remove_one removes all variations of the list where exactly one item is skipped.
  def remove_one([_el], 1), do: [[]]

  def remove_one([hd | tl], 1) do
    prefix_hd = fn x -> [hd] ++ x end

    Enum.map(remove_one(tl, 1), prefix_hd) ++ [tl]
  end

  # Count the number of reports that are either completely safe, or need one element
  # removed in order to be safe.
  def part2(reports) do
    Enum.count(reports, fn report ->
      safe_report?(report) || Enum.any?(remove_one(report, 1), &safe_report?/1)
    end)
  end
end
