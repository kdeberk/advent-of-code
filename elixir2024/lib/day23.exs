defmodule Day23 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day23/puzzle.txt")

    edges = contents
    |> String.split("\n")
    |> Enum.map(fn line -> String.split(line, "-") end)

    nodes = edges
    |> Enum.flat_map(fn [a, b] -> [a, b] end)
    |> MapSet.new

    connections = edges
    |> Enum.reduce(Map.new, fn [a, b], connected ->
      connected
      |> Map.update(a, MapSet.new([b]), fn xs -> MapSet.put(xs, b) end)
      |> Map.update(b, MapSet.new([a]), fn xs -> MapSet.put(xs, a) end)
    end)

    edges = edges
    |> Enum.flat_map(fn [a, b] -> [{a, b}, {b, a}] end)
    |> MapSet.new

    %{nodes: nodes, edges: edges, connections: connections}
  end

  def part1(input) do
    %{connections: connections} = input

    connections
    |> Enum.filter(fn {node, _} -> String.starts_with?(node, "t") end)
    |> Enum.reduce(MapSet.new, fn ({node_a, neighbors}, acc) ->
      neighbors
      |> Enum.reduce(acc, fn (node_b, acc) ->
        connections
        |> Map.get(node_b)
        |> MapSet.intersection(neighbors)
        |> Enum.reduce(acc, fn (node_c, acc) ->
          MapSet.put(acc, [node_a, node_b, node_c] |> Enum.sort)
        end)
      end)
    end)
    |> MapSet.size
  end

  def part2(input) do
    %{nodes: nodes, connections: connections, edges: edges} = input
    {_, visited} = nodes
    |> Enum.reduce({MapSet.new, MapSet.new}, fn (node, {largest, visited}) ->
      {largest_, visited} = grow(MapSet.new([node]), node, connections, edges, largest, visited)
      if MapSet.size(largest) < MapSet.size(largest_) do
        {largest_, visited}
      else
        {largest, visited}
      end
    end)

    visited
    |> Enum.max_by(&MapSet.size/1)
    |> Enum.sort
    |> Enum.join(",")
  end

  def grow(group, newest, connections, edges, largest, visited) do
    connections
    |> Map.get(newest)
    |> Enum.reduce({largest, visited}, fn (nei, {largest, visited}) ->
      cond do
        MapSet.member?(group, nei) ->
          {group, MapSet.put(visited, group)}

        MapSet.member?(visited, group) ->
          {group, visited}

        Enum.all?(group, fn cur -> MapSet.member?(edges, {cur, nei}) end) ->
          # Each member in group has connection to new neighbor
          {g, visited} = grow(MapSet.put(group, nei), nei, connections, edges, largest, visited)
          if MapSet.size(largest) < MapSet.size(g) do
            {g, visited}
          else
            {group, visited}
          end

        true ->
          {group, visited}
      end
    end)
  end
end
