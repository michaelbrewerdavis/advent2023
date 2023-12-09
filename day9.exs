defmodule Day9 do
  def diff_seq(seq) do
    Stream.zip(seq, Stream.drop(seq, 1))
    |> Enum.to_list()
    |> Enum.map(fn {a, b} -> b - a end)
  end

  def dd(seq) do
    if Enum.all?(seq, &(&1 == 0)) do
      [seq]
    else
      [seq] ++ dd(diff_seq(seq))
    end
  end
end

lines = File.read!("day9.input") |> String.split("\n", trim: true)

seqs =
  lines
  |> Enum.map(fn s -> String.split(s, " ") |> Enum.map(&String.to_integer/1) end)

diffs = seqs |> Enum.map(&Day9.dd/1)

vals =
  diffs
  |> Enum.map(fn d ->
    d |> Enum.map(&List.last/1) |> Enum.sum()
  end)

Enum.sum(vals) |> IO.inspect()

reverse_vals =
  diffs
  |> Enum.map(fn d ->
    d
    |> Enum.map(&List.first/1)
    |> Enum.reverse()
    |> Enum.reduce(0, fn n, acc ->
      n - acc
    end)
  end)

Enum.sum(reverse_vals) |> IO.inspect()
