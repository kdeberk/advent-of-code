
defmodule Day24 do
  def prepare_input() do
    {:ok, content} = File.read("input/day24/puzzle.txt")

    [wires, gates] = String.split(content, "\n\n")

    wires = wires
    |> String.split("\n")
    |> Enum.map(fn wire ->
      [name, value] = String.split(wire, ": ")
      {name, value == "1"}
    end)
    |> Map.new

    gates = gates
    |> String.split("\n")
    |> Enum.map(fn gate ->
      [a, op, b, _, c] = String.split(gate, " ")
      {c, {a, op, b}}
    end)
    |> Map.new

    %{wires: wires, gates: gates}
  end

  # resolve determines the final output value for gate. Intermediate
  #  values are stored in values.
  def resolve(gate, gates, values) do
    if Map.has_key?(values, gate) do
      {Map.get(values, gate), values}
    else
      {a, op, b} = Map.get(gates, gate)
      {a, values} = resolve(a, gates, values)
      {b, values} = resolve(b, gates, values)

      value = case op do
                "AND" -> a && b
                "OR" -> a || b
                "XOR" -> a != b
              end

      {value, Map.put(values, gate, value)}
    end
  end

  # run_machine takes a valuation on the x and y wires and resolves the number
  def run_machine(wires, gates) do
    zs = gates
    |> Map.keys
    |> Enum.filter(fn gate -> String.starts_with?(gate, "z") end)
    |> Enum.sort
    |> Enum.reverse

    values = zs
    |> Enum.reduce(wires, fn gate, values ->
      {_, values} = resolve(gate, gates, values)
      values
    end)

    zs
    |> Enum.map(fn gate -> Map.get(values, gate) end)
    |> Enum.reduce(0, fn bit, value ->
      # value <> if bit do "1" else "0" end
      2 * value + if bit do 1 else 0 end
    end)
  end

  def part1(%{wires: wires, gates: gates}) do
    run_machine(wires, gates)
  end

  def add(wires, gates, x, y) do
    wires = wires
    |> Enum.map(fn {name, _} ->
      s = String.slice(name, 0..0)
      n = Utils.parse_int(String.slice(name, 1..String.length(name)))

      {name, 0 < Bitwise.band(if s == "x" do x else y end, 2**n)}
    end)
    |> Map.new

    wires
    |> Enum.filter(fn {_, v} -> v end)
    |> Enum.map(fn {n, _} -> n end)
    |> inspect
    |> IO.puts

    run_machine(wires, gates)
  end

  def swap(gates, a, b) do
    gates
    |> Map.put(a, Map.get(gates, b))
    |> Map.put(b, Map.get(gates, a))
  end

  def check_n(gates, n) do
    x_in = "x" <> String.pad_leading(Integer.to_string(n), 2, "0")
    y_in = "y" <> String.pad_leading(Integer.to_string(n), 2, "0")
    z_in = "z" <> String.pad_leading(Integer.to_string(n), 2, "0")

    {xor_n, _} = Enum.find(gates, fn {_, {x, op, y}} ->
      ((x == x_in && y == y_in) || (y == x_in && x == y_in)) && op == "XOR"
    end)
    {and_n, _} = Enum.find(gates, fn {_, {x, op, y}} ->
      ((x == x_in && y == y_in) || (y == x_in && x == y_in)) && op == "AND"
    end)

    {za, zop, zc} = Map.get(gates, z_in)
    if zop != "XOR" do
      IO.puts ~s(There is something wrong with #{z_in})
    else
      carry_nm1 = if za == xor_n do zc else za end

      {_, cop, _} = Map.get(gates, carry_nm1)
      if cop != "OR" do
          IO.puts ~s(There is something wrong with #{carry_nm1} or #{xor_n})
      end
    end
  end

  def part2(%{wires: wires, gates: gates}) do
    gates = gates
    |> swap("msq","z39")
    |> swap("mps","z27")
    |> swap("vhm","z14")
    |> swap("cnk","qwf")

    a = 2**43-1
    b = 15
    add(wires, gates, a, b) == a+b
  end
end

# Idea:

# x_n AND y_n -> and_n
# xor_n XOR y_n -> xor_n
# xor_n AND carry_(n-1) -> other_n
# and_n OR other_n -> carry_n
# xor_n XOR carry_(n-1) -> z_n


# cnk,mps,msq,qwf,vhm,z14,z27,z39"
#


# msq,z39
# mps,z27
# vhm,z14
# cnk,qwf
