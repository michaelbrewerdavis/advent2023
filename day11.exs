defmodule Day11 do
  def parse_input(f) do
    file =
      File.read!(f)
      |> String.split("\n", trim: true)

    all =
      file
      |> Enum.with_index(0)
      |> Enum.flat_map(fn {row, r} ->
        String.split(row, "", trim: true)
        |> Enum.with_index(0)
        |> Enum.map(fn {text, c} -> {text, r, c} end)
      end)

    height = Enum.count(file)
    width = Integer.floor_div(Enum.count(all), height)

    galaxies =
      all
      |> Enum.filter(fn {val, r, c} -> val == "#" end)
      |> Enum.map(fn {val, r, c} -> {r, c} end)

    {height, width, galaxies}
  end

  def stretch(dim, hits, size) do
    Enum.reduce(0..(dim - 1), {0, %{}}, fn c, {i, cc} ->
      if MapSet.member?(hits, c) do
        {i, Map.put(cc, c, c + i)}
      else
        {i + size - 1, Map.put(cc, c, c + i)}
      end
    end)
    |> elem(1)
    |> IO.inspect()
  end

  def comb(0, _), do: [[]]
  def comb(_, []), do: []

  def comb(m, [h | t]) do
    for(l <- comb(m - 1, t), do: [h | l]) ++ comb(m, t)
  end

  def main(f) do
    {height, width, galaxies} = parse_input(f) |> IO.inspect()

    {hit_rows, hit_cols} =
      Enum.reduce(galaxies, {MapSet.new(), MapSet.new()}, fn {r, c}, {rows, cols} ->
        {MapSet.put(rows, r), MapSet.put(cols, c)}
      end)
      |> IO.inspect()

    new_rows = stretch(height, hit_rows, 1_000_000)
    new_cols = stretch(width, hit_cols, 1_000_000)

    new_galaxies =
      galaxies |> Enum.map(fn {r, c} -> {Map.get(new_rows, r), Map.get(new_cols, c)} end)

    comb(2, new_galaxies)
    |> Enum.map(fn [left, right] ->
      {lx, ly} = left
      {rx, ry} = right
      abs(lx - rx) + abs(ly - ry)
    end)
    |> IO.inspect()
    |> Enum.sum()
  end
end

# 82000210 is too low

Day11.main("day11.sample")
|> IO.inspect()
