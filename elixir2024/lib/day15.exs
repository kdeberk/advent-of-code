defmodule Day15 do
  def day, do: 15
  def name, do: "Warehouse Woes"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    [warehouse, movements] = String.split(contents, "\n\n")

    grid = Grid.from_multiline_string(warehouse)
    {robot, _} = Enum.find(grid[:grid], fn {_, v} -> v == "@" end)

    grid
    |> Map.put(
      :movements,
      movements
      |> String.split("\n")
      |> Enum.join()
      |> String.graphemes()
      |> Enum.map(fn mov ->
        case mov do
          "^" -> {0, -1}
          "v" -> {0, 1}
          "<" -> {-1, 0}
          ">" -> {1, 0}
        end
      end)
    )
    |> Map.put(:robot, robot)
  end

  def merge_optional_maps(maps) do
    Enum.reduce(
      maps,
      fn m, acc ->
        if m == nil || acc == nil do
          nil
        else
          Map.merge(acc, m)
        end
      end
    )
  end

  def move_wide_box_vertically(warehouse, {b1, b2}, dir) do
    [p1, p2] = [Grid.move_point(b1, dir), Grid.move_point(b2, dir)]
    [c1, c2] = [Grid.at(warehouse, p1), Grid.at(warehouse, p2)]
    moved = Map.new([{p1, "["}, {p2, "]"}, {b1, "."}, {b2, "."}])

    left = Grid.move_point(p1, {-1, 0})
    right = Grid.move_point(p2, {1, 0})

    cond do
      c1 == "." && c2 == "." ->
        moved

      c1 == "#" || c2 == "#" ->
        nil

      c1 == "[" && c2 == "]" ->
        merge_optional_maps([
          move_wide_box_vertically(warehouse, {p1, p2}, dir),
          moved
        ])

      c1 == "." ->
        merge_optional_maps([
          move_wide_box_vertically(warehouse, {p2, right}, dir),
          moved
        ])

      c2 == "." ->
        merge_optional_maps([
          move_wide_box_vertically(warehouse, {left, p1}, dir),
          moved
        ])

      true ->
        updated = move_wide_box_vertically(warehouse, {left, p1}, dir)

        if updated == nil do
          nil
        else
          merge_optional_maps([
            updated,
            move_wide_box_vertically(
              %{warehouse | grid: Map.merge(warehouse[:grid], updated)},
              {p2, right},
              dir
            ),
            moved
          ])
        end
    end
  end

  def move_wide_box_horizontally(warehouse, {b1, b2}, dir) do
    left = Grid.move_point(b1, {-1, 0})
    right = Grid.move_point(b2, {1, 0})

    [d1, d2] = [Grid.move_point(b1, dir), Grid.move_point(b2, dir)]
    [c1, c2] = [Grid.at(warehouse, left), Grid.at(warehouse, right)]
    moved = Map.new([{d1, "["}, {d2, "]"}])

    cond do
      c1 == "#" || c2 == "#" ->
        nil

      dir == {-1, 0} && c1 == "." ->
        moved

      dir == {-1, 0} && c1 == "]" ->
        merge_optional_maps([
          move_wide_box_horizontally(warehouse, {Grid.move_point(left, dir), left}, dir),
          moved
        ])

      dir == {1, 0} && c2 == "." ->
        moved

      dir == {1, 0} && c2 == "[" ->
        merge_optional_maps([
          move_wide_box_horizontally(warehouse, {right, Grid.move_point(right, dir)}, dir),
          moved
        ])
    end
  end

  def find_next_free_spot(warehouse, pos, dir) do
    cur = Grid.at(warehouse, pos)

    cond do
      cur == "." -> pos
      cur == "#" -> nil
      true -> find_next_free_spot(warehouse, Grid.move_point(pos, dir), dir)
    end
  end

  def move_small_box(warehouse, box, dir) do
    free = find_next_free_spot(warehouse, box, dir)

    if free == nil do
      nil
    else
      Map.new([
        {free, "O"},
        {box, "."}
      ])
    end
  end

  def move_wide_box(warehouse, box, dir) do
    case dir do
      {-1, 0} -> move_wide_box_horizontally(warehouse, box, dir)
      {1, 0} -> move_wide_box_horizontally(warehouse, box, dir)
      {0, -1} -> move_wide_box_vertically(warehouse, box, dir)
      {0, 1} -> move_wide_box_vertically(warehouse, box, dir)
    end
  end

  def move_box(warehouse, robot, dir) do
    if warehouse[:wide] == nil do
      move_small_box(warehouse, robot, dir)
    else
      to = Grid.move_point(robot, dir)
      left = Grid.move_point(to, {-1, 0})
      right = Grid.move_point(to, {1, 0})

      case Grid.at(warehouse, to) do
        "." ->
          # Nothing needs to move
          Map.new()

        "#" ->
          # Can't move
          nil

        "[" ->
          move_wide_box(warehouse, {to, right}, dir)

        "]" ->
          move_wide_box(warehouse, {left, to}, dir)
      end
    end
  end

  def move_robot(warehouse) do
    if length(warehouse[:movements]) == 0 do
      warehouse
    else
      %{movements: [mov | movs], robot: robot} = warehouse

      # IO.puts(inspect(robot))
      # IO.puts(inspect(mov))
      updated = move_box(warehouse, robot, mov)

      if updated == nil do
        move_robot(%{warehouse | movements: movs})
      else
        nxt = Grid.move_point(robot, mov)

        grid =
          warehouse[:grid]
          |> Map.merge(updated)
          |> Map.put(nxt, "@")
          |> Map.put(robot, ".")

        move_robot(%{warehouse | movements: movs, grid: grid, robot: nxt})
      end
    end
  end

  def part1(warehouse) do
    %{grid: grid} = move_robot(warehouse)

    grid
    |> Enum.filter(fn {_, v} -> v == "O" end)
    |> Enum.map(fn {{x, y}, _} -> y * 100 + x end)
    |> Enum.sum()
  end

  def widen(warehouse) do
    %{width: width, grid: grid, robot: {rx, ry}} = warehouse

    %{
      warehouse
      | width: 2 * width,
        robot: {2 * rx, ry},
        grid:
          grid
          |> Enum.flat_map(fn {{x, y}, v} ->
            case v do
              "#" -> [{{2 * x, y}, "#"}, {{2 * x + 1, y}, "#"}]
              "O" -> [{{2 * x, y}, "["}, {{2 * x + 1, y}, "]"}]
              "." -> [{{2 * x, y}, "."}, {{2 * x + 1, y}, "."}]
              "@" -> [{{2 * x, y}, "@"}, {{2 * x + 1, y}, "."}]
            end
          end)
          |> Map.new()
    }
    |> Map.put(:wide, true)
  end

  def part2(warehouse) do
    %{grid: grid} = move_robot(widen(warehouse))

    grid
    |> Enum.filter(fn {_, v} -> v == "[" end)
    |> Enum.map(fn {{x, y}, _} -> y * 100 + x end)
    |> Enum.sum()
  end
end
