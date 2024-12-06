Code.require_file("utils.exs")

defmodule Day4 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day4.txt")
    Utils.multiline_string_to_grid(contents)
  end

  def part1(%{ width: width, height: height, char_at: char_at }) do
    xmas_at? = fn [x, y, dx, dy] ->
      char_at.(x + dx * 0, y + dy * 0) == "X" &&
        char_at.(x + dx * 1, y + dy * 1) == "M" &&
        char_at.(x + dx * 2, y + dy * 2) == "A" &&
        char_at.(x + dx * 3, y + dy * 3) == "S"
    end

    for(x <- 0..width, y <- 0..height, dx <- -1..1, dy <- -1..1, do: [x, y, dx, dy])
    |> Enum.count(xmas_at?)
  end


  def part2(%{ width: width, height: height, char_at: char_at }) do
    mas_at? = fn x, y, dx, dy ->
      char_at.(x + dx * -1, y + dy * -1) == "M" &&
        char_at.(x + dx * 0, y + dy * 0) == "A" &&
        char_at.(x + dx * 1, y + dy * 1) == "S"
    end

    x_mas_at? = fn x, y ->
      char_at.(x, y) == "A" &&
        for(dx <- [-1, 1], dy <- [-1, 1], do: [dx, dy])
        |> Enum.count(fn [dx, dy] -> mas_at?.(x, y, dx, dy) end) == 2
    end

    for(x <- 0..width, y <- 0..height, do: [x, y])
    |> Enum.count(fn [x, y] -> x_mas_at?.(x, y) end)
  end
end

input = Day4.prepare_input()
IO.puts("Day 4: Ceres Search")
IO.puts("Part 1: #{Day4.part1(input)}")
IO.puts("Part 2: #{Day4.part2(input)}")
