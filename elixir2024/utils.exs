defmodule Utils do
  def parse_int(str) do
    {int, _frac} = Integer.parse(str)
    int
  end

  def permutations([]), do: [[]]
  def permutations(lst) do
    for hd <- lst, tl <- permutations(lst--[hd]), do: [hd | tl]
  end

  def multiline_string_to_grid(string) do
    lines = String.split(string, "\n")

    grid = lines
    |> Enum.with_index(fn row, y ->
      row
      |> String.graphemes()
      |> Enum.with_index(fn g, x -> {[x, y], g} end)
    end)
    |> List.flatten()
    |> Enum.into(%{})

    %{grid: grid,
      height: length(lines),
      width: String.length(hd(lines)),
      char_at: fn x, y -> grid[[x, y]] end,
    }
  end
end

defmodule Matrix do
  def transpose([[] | _]), do: []

  def transpose(m) do
    [Enum.map(m, &hd/1) | transpose(Enum.map(m, &tl/1))]
  end
end
