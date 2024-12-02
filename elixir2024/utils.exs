defmodule Utils do
  def parse_int(str) do
    {int, _frac} = Integer.parse(str)
    int
  end
end

defmodule Matrix do
  def transpose([[] | _]), do: []

  def transpose(m) do
    [Enum.map(m, &hd/1) | transpose(Enum.map(m, &tl/1))]
  end
end
