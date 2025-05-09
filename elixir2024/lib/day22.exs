Code.require_file("lib/utils.exs")

defmodule Day22 do
  def day, do: 22
  def name, do: "Monkey Market"

  def prepare_input(path) do
    {:ok, contents} = File.read(path)
    contents
    |> String.split("\n")
    |> Enum.map(&Utils.parse_int/1)
  end

  def mix(secret, num), do: Bitwise.bxor(secret, num)
  def prune(secret), do: Integer.mod(secret, 16777216)

  def secret_numbers(seed) do
    Stream.iterate(seed, fn secret ->
      secret = prune(mix(secret, secret * 64))
      secret = prune(mix(secret, floor(secret / 32)))
      secret = prune(mix(secret, secret * 2048))

      secret
    end)
  end

  def part1(seeds) do
    seeds
    |> Enum.map(fn seed ->
      seed
      |> Day22.secret_numbers
      |> Enum.at(2000)
    end)
    |> Enum.sum
  end

  def part2(seeds) do
    seeds
    |> Enum.reduce(Map.new, fn seed, sum_of_prices ->
      {_, _, sum_of_prices, _} = seed
      |> Day22.secret_numbers
      # We need to see 2000 price changes, so at least 2001 items
      |> Enum.take(2001)
      |> Enum.map(fn x -> rem(x, 10) end)
      |> Enum.reduce({0, 0, sum_of_prices, MapSet.new}, fn (price, {prev_price, changes, sum_of_prices, seen_before}) ->
        change = (price - prev_price) + 9

        changes = changes
        |> Bitwise.band((2**15)-1) # Only keep the first 15 bits
        |> Bitwise.bsl(5)          # Move everything 5 bits to the left
        |> Bitwise.bor(change)     # Add last change

        if MapSet.member?(seen_before, changes) do
          {price, changes, sum_of_prices, seen_before}
        else
          {
            price,
            changes,
            Map.update(sum_of_prices, changes, price, fn x -> x + price end),
            MapSet.put(seen_before, changes)
          }
        end
      end)

      # sum_of_prices


      # # chunk_every creates a sliding window, showing each consecutive sequence of 5 values
      # |> Enum.chunk_every(5, 1, :discard)
      # |> Enum.map(fn [a,b,c,d,e] ->
      #   price = e

      #   # We're reducing the 4 diffs to a single 20 bit number by concatenating
      #   #  the last 5 bits of each diff (5 bit is enough to represent each number in -9 to 9)
      #   changes = [
      #     b-a+9,
      #     Bitwise.bsl(c-b+9, 5),
      #     Bitwise.bsl(d-c+9, 10),
      #     Bitwise.bsl(e-d+9, 15),
      #   ]
      #   |> Enum.reduce(&Bitwise.bor/2)

      #   { changes, price }
      # end)
      # |> Enum.reduce({sum_of_prices, MapSet.new}, fn {changes, price}, {sum_of_prices, seen_before} ->
      #   if MapSet.member?(seen_before, changes) do
      #     # Saw this set of changes before for this seed, ignore
      #     {sum_of_prices, seen_before}
      #   else
      #     {
      #       Map.update(sum_of_prices, changes, price, fn x -> x + price end),
      #       MapSet.put(seen_before, changes)
      #     }
      #   end
      # end)

      sum_of_prices
    end)
    |> Map.values
    |> Enum.max
  end
end
