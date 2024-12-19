Code.require_file("lib/utils.exs")

defmodule Day17 do
  def day, do: 17
  def name, do: "Chronospatial Computer"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)

    [regs, prog] = String.split(contents, "\n\n")

    %{
      registers:
        regs
        |> String.split("\n")
        |> Enum.map(fn line ->
          [[_, n, v]] = Regex.scan(~r/^Register ([A-Z]): (\d+)$/, line)
          {n, Utils.parse_int(v)}
        end)
        |> Map.new(),
      program:
        prog
        |> String.split(" ")
        |> Enum.at(1)
        |> String.split(",")
        |> Enum.map(&Utils.parse_int/1)
    }
  end

  def combo(regs, n) do
    cond do
      n < 4 -> n
      n == 4 -> regs["A"]
      n == 5 -> regs["B"]
      n == 6 -> regs["C"]
    end
  end

  def ins_name(ins) do
    Map.get(%{
      0 => :adv,
      1 => :bxl,
      2 => :bst,
      3 => :jnz,
      4 => :bxc,
      5 => :out,
      6 => :bdv,
      7 => :cdv
    }, ins)
  end

  def execute_program(ip, prog, regs) do
    if length(prog) <= ip do
      []
    else
      [ins, op] = [Enum.at(prog, ip), Enum.at(prog, ip+1)]
      case ins_name(ins) do
        :adv -> (
          execute_program(
            ip + 2,
            prog,
            Map.put(regs, "A", floor(regs["A"] / 2 ** combo(regs, op)))
          ))
        :bxl -> (
          execute_program(
            ip + 2,
            prog,
            Map.put(regs, "B", Bitwise.bxor(regs["B"], op))
          ))
        :bst ->
          execute_program(
            ip + 2,
            prog,
            Map.put(regs, "B", rem(combo(regs, op), 8))
          )
        :jnz ->
          if regs["A"] == 0 do
            execute_program(ip+2, prog, regs)
          else
            execute_program(op, prog, regs)
          end
        :bxc ->
          execute_program(
            ip + 2,
            prog,
            Map.put(regs, "B", Bitwise.bxor(regs["B"], regs["C"]))
          )
        :out ->
          execute_program(
            ip + 2,
            prog,
            regs
          ) ++ [rem(combo(regs, op), 8)]
        :bdv ->
          execute_program(
            ip + 2,
            prog,
            Map.put(regs, "B", floor(regs["A"] / 2 ** combo(regs, op)))
          )
        :cdv ->
          execute_program(
            ip + 2,
            prog,
            Map.put(regs, "C", floor(regs["A"] / 2 ** combo(regs, op)))
          )
      end
    end
  end

  def part1(%{registers: registers, program: program}) do
    execute_program(0, program, registers)
    |> Enum.join(",")
  end

  # 2 4  # bst 4  # bst a  # b = a mod 8
  # 1 5  # bxl 5  # bxl 5  # b = b xor 5
  # 7 5  # cdv 5  # cdv b  # c = a/2**b
  # 4 5  # bxc 5  # bxc _  # b = b xor c
  # 0 3  # adv 3  # adv 3  # a = a div 8
  # 1 6  # bxl 6  # bxl 6  # b = b xor 6
  # 5 5  # out 5  # out b  # print(b%8)
  # 3 0  # jnz 0  # jnz 0  #
  def disasm(0), do: []
  def disasm(a) do
    b = Integer.mod(a, 8)
    b = Bitwise.bxor(b, 5)
    c = floor(a/(2**b))
    b = Bitwise.bxor(b, c)
    a = floor(a/8)
    b = Bitwise.bxor(b, 6)
    [Integer.mod(b, 8)] ++ disasm(a)
  end

  # Can generate the value of A by starting with the least significant triple of bits
  #  and slowly working our way up.
  def search(n, idx, output) do
    nxt = 0..7
    |> Enum.map(fn x -> n*8+x end)
    |> Enum.filter(fn x ->
      disasm(x) == Enum.slice(output, idx..length(output))
    end)

    if idx == 0 do
      nxt
    else
      Enum.flat_map(nxt, fn x -> search(x, idx-1, output) end)
    end
  end

  def part2(%{program: program}) do
    search(0, length(program)-1, program)
    |> Enum.min
  end
end
