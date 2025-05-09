defmodule Utils do
  def parse_int(str) do
    {int, _frac} = Integer.parse(str)
    int
  end

  def permutations([]), do: [[]]

  def permutations(lst) do
    for hd <- lst, tl <- permutations(lst -- [hd]), do: [hd | tl]
  end

  def combinations(0, _), do: [[]]
  def combinations(_, []), do: []

  def combinations(m, [hd | tl]) do
    for(c <- combinations(m - 1, tl), do: [hd | c]) ++ combinations(m, tl)
  end

  # def neighbors(%{grid: grid, width: width, height: height}, {x, y, z}) do
  #   [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
  #   |> Enum.map(fn {dx, dy} ->
  #     {x+dx, y+dy, grid[{x+dx, y+dy}]}
  #   end)
  #   |> Enum.filter(fn {x_, y_, z_} ->
  #     0 <= x_ && x_ < width && 0 <= y_ && y_ < height && z+1 == z_
  #   end)
  # end
end

defmodule Grid do
  def from_multiline_string(string) do
    lines = String.split(string, "\n")

    grid =
      lines
      |> Enum.with_index(fn row, y ->
        row
        |> String.graphemes()
        |> Enum.with_index(fn g, x -> {{x, y}, g} end)
      end)
      |> List.flatten()
      |> Enum.into(%{})

    %{grid: grid, height: length(lines), width: String.length(hd(lines))}
  end

  def at(%{grid: grid}, {x, y}) do
    grid[{x, y}]
  end

  def put(input, xy, val) do
    %{input | grid: Map.put(input[:grid], xy, val)}
  end

  def neighbors(%{width: width, height: height}, {x, y}) do
    [{-1, 0}, {1, 0}, {0, -1}, {0, 1}]
    |> Enum.map(fn {dx, dy} ->
      {x + dx, y + dy}
    end)
    |> Enum.filter(fn {x_, _} -> x_ in 0..(width - 1) && y in 0..(height - 1) end)
  end

  def coordinates(%{width: width, height: height}) do
    for x <- 0..(width - 1), y <- 0..(height - 1), do: {x, y}
  end

  def move_point({x, y}, {dx, dy}), do: {x + dx, y + dy}

  def render(%{grid: grid, width: width, height: height}) do
    for(
      y <- 0..(height - 1),
      do:
        for(x <- 0..(width - 1), do: Map.get(grid, {x, y}))
        |> Enum.join()
    )
    |> Enum.join("\n")
  end

  def find(%{grid: grid}, fun) do
    found = Enum.find(grid, fun)

    if found == nil do
      nil
    else
      {c, _} = found
      c
    end
  end
end

defmodule Matrix do
  def transpose([[] | _]), do: []

  def transpose(m) do
    [Enum.map(m, &hd/1) | transpose(Enum.map(m, &tl/1))]
  end
end

defmodule MinHeap do
  def new() do
    %{values: %{}}
  end

  def push(heap, key, val) do
    %{values: values} = heap

    idx = map_size(values)

    %{heap | values: Map.put(values, idx, {key, val})}
    |> swim(idx)
  end

  def peek(heap) do
    %{values: values} = heap

    if map_size(values) == 0 do
      nil
    else
      {_key, val} = values[0]
      val
    end
  end

  # returns heap without the top value. Use peek to obtain that value first.
  def pop(heap) do
    %{values: values} = heap

    cond do
      map_size(values) == 0 ->
        heap

      map_size(values) == 1 ->
        %{heap | values: %{}}

      true ->
        heap
        |> swap(0, map_size(values) - 1)
        |> Map.update!(:values, fn values -> Map.delete(values, map_size(values) - 1) end)
        |> sink(0)
    end
  end

  defp swim(h, idx) do
    par = floor((idx - 1) / 2)

    cond do
      idx == 0 ->
        h

      cmp(h, idx, par) == :less_than ->
        # value is less than parent, swap and recurse
        swim(swap(h, idx, par), par)

      true ->
        h
    end
  end

  defp sink(h, idx) do
    %{values: values} = h
    [c1, c2] = [2 * idx + 1, 2 * idx + 2]

    len = map_size(values)

    nxt =
      if c1 < len && cmp(h, idx, c1) == :greater_than do
        c1
      else
        idx
      end

    nxt =
      if c2 < len && cmp(h, nxt, c2) == :greater_than do
        c2
      else
        nxt
      end

    if nxt == idx do
      h
    else
      sink(swap(h, idx, nxt), nxt)
    end
  end

  defp cmp(h, idx, jdx) do
    %{values: values} = h

    {a, _} = Map.get(values, idx)
    {b, _} = Map.get(values, jdx)
    cond do
      a < b -> :less_than
      a == b -> :equal
      b < a -> :greater_than
    end
  end

  defp swap(h, idx, jdx) do
    %{values: values} = h

    [a, b] = [Map.get(values, idx), Map.get(values, jdx)]

    values =
      values
      |> Map.put(idx, b)
      |> Map.put(jdx, a)

    %{h | values: values}
  end

  def size(%{values: values}), do: map_size(values)
end

defmodule Direction do
  def north(), do: {0, -1}
  def south(), do: {0, 1}
  def west(), do: {-1, 0}
  def east(), do: {1, 0}

  def left(dir) do
    cond do
      dir == north() -> west()
      dir == west() -> south()
      dir == south() -> east()
      dir == east() -> north()
    end
  end

  def right(dir) do
    cond do
      dir == north() -> east()
      dir == east() -> south()
      dir == south() -> west()
      dir == west() -> north()
    end
  end
end

defmodule Queue do
  def new(items \\ []) do
    q = (:queue.new())

    items
    |> Enum.reduce(q, fn (item, q) -> push(q, item) end)
  end

  def len(q) do
    :queue.len(q)
  end

  def push(q, value) do
    :queue.in({value}, q)
  end

  def pop(q) do
    if len(q) == 0 do
      {nil, q}
    else
      {{_, {value}}, q} = :queue.out(q)
      {value, q}
    end
  end
end
