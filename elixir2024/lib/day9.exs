Code.require_file("utils.exs")

# TODO: clean this one up

# List is a linked list.
# for this puzzle we need to navigate from both directions
#   can use Enum.reverse to quickly navigate backwards

# Part 1
# One list represents free nodes, one


defmodule Day9 do
  def prepare_input() do
    {:ok, contents} = File.read("input/day9.txt")

    {files, free, _offset} = contents
     |> String.graphemes
     |> Enum.map(&Utils.parse_int/1)
     |> Enum.with_index
     |> Enum.reduce({[], [], 0}, fn ({size, idx}, {files, free, offset}) ->
      cond do
        size == 0 ->
          {files, free, offset}
        rem(idx, 2) == 0 ->
          {files ++ [{offset, floor(idx/2), size}], free, offset+size}
        true ->
          {files, free ++ [{offset, size}], offset+size}
       end
     end)

    {files, free}
  end

  def file_score(id, idx, size) do
    # IO.puts inspect([id, idx, size, id*Enum.sum(idx..(idx+size-1))])
    id*Enum.sum(idx..(idx+size-1))
  end


  def move_byte_by_byte(free_lst, file_lst, score)
  def move_byte_by_byte([], [], score), do: score
  def move_byte_by_byte([], file_lst, score) do
    {idx, id, size} = hd(file_lst)
    move_byte_by_byte([], tl(file_lst), score + file_score(id, idx, size))
  end
  def move_byte_by_byte(free_lst, file_lst, score) do
    {free_idx, free_size} = hd(free_lst)
    {file_idx, file_id, file_size} = hd(file_lst)

    cond do
    file_idx < free_idx ->
        # We can no longer move files left. Start calculating the scores for the remaining files
        move_byte_by_byte([], file_lst, score)
    free_size < file_size ->
        # We can not fully move the file into the free spot
        move_byte_by_byte(
          tl(free_lst),
          [{file_idx, file_id, file_size-free_size}] ++ tl(file_lst),
          score + file_score(file_id, free_idx, free_size)
        )
      free_size == file_size ->
        # Free space and file perfectly match
        move_byte_by_byte(
          tl(free_lst),
          tl(file_lst),
          score + file_score(file_id, free_idx, free_size))
      true ->
        # Free space is bigger than file size
        move_byte_by_byte(
          [{free_idx+file_size, free_size-file_size}] ++ tl(free_lst),
          tl(file_lst),
          score + file_score(file_id, free_idx, file_size))
    end
  end

  def part1({files_lst, free_lst}) do
    move_byte_by_byte(free_lst, Enum.reverse(files_lst), 0)
  end

  # def move(blocks, 0), do: blocks
  # def move(blocks, idx) do
  #   {block_n, block_id, block_kind} = Enum.at(blocks, idx)

  #   if block_kind == :free do
  #     move(blocks, idx-1)
  #   else
  #     found = blocks
  #     |> Enum.with_index
  #     |> Enum.find(fn {{n, _, k}, free_idx} -> k == :free && block_n <= n && free_idx < idx end)

  #     if found == nil do
  #       # Can't move block, leave it and move on
  #       move(blocks, idx-1)
  #     else
  #       {{free_n, _, _}, free_idx} = found
  #       cond do
  #         free_n == block_n ->
  #           move(
  #             blocks
  #             |> List.replace_at(idx, {block_n, 0, :free})
  #             |> List.replace_at(free_idx, {block_n, block_id, :file}),
  #             idx
  #           )
  #         true ->
  #           move(
  #             blocks
  #             |> List.replace_at(idx, {block_n, 0, :free})
  #             |> List.replace_at(free_idx, {block_n, block_id, :file})
  #             |> List.insert_at(free_idx+1, {free_n-block_n, 0, :free}),
  #             idx
  #           )
  #       end
  #     end
  #   end
  # end

  # def part2(input) do
  #   blocks = input
  #   |> Enum.with_index
  #   |> Enum.map(fn {n, idx} ->
  #     if rem(idx,2) == 0 do
  #       {n, floor(idx/2), :file}
  #     else
  #       {n, idx, :free}
  #     end
  #   end)

  #   {_, score} = move(blocks, length(blocks)-1)
  #   |> Enum.reduce({0, 0}, fn ({n, id, kind}, {offset, score}) ->
  #     if kind == :free do
  #       {offset+n, score}
  #     else
  #       {offset+n, score + id * Enum.sum(offset..(offset+n-1))}
  #     end
  #   end)

  #   score
  # end
end

input = Day9.prepare_input()
IO.puts("Day 9: Disk Fragmenter")
# IO.puts("Part 1: #{Day9.part1(input)}")
# IO.puts("Part 2: #{Day9.part2(input)}")
