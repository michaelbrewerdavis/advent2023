defmodule Day10 do
  def dirs(symbol) do
    case symbol do
      "|" -> [:north, :south]
      "-" -> [:east, :west]
      "L" -> [:north, :east]
      "J" -> [:north, :west]
      "7" -> [:south, :west]
      "F" -> [:south, :east]
      "." -> []
      "S" -> [:south, :east, :west, :north]
    end
  end

  def find_dir(dir1, dir2) do
    vals = %{:north => 3, :south => 5, :east => 7, :west => 2}
    product = vals[dir1] * vals[dir2]

    case product do
      15 -> "|"
      14 -> "-"
      21 -> "L"
      6 -> "J"
      10 -> "7"
      35 -> "F"
      _ -> "."
    end
  end

  def parse_input(f) do
    File.read!(f)
    |> String.split("\n", trim: true)
    |> Enum.map(fn s -> String.split(s, "", trim: true) |> List.to_tuple() end)
    |> List.to_tuple()
  end

  def map_width(m) do
    tuple_size(elem(m, 0))
  end

  def map_height(m) do
    tuple_size(m)
  end

  def range(m) do
    0..(map_height(m) - 1)
    |> Enum.flat_map(fn row ->
      0..(map_width(m) - 1)
      |> Enum.map(fn col -> {row, col} end)
    end)
  end

  def map_at(m, pos) do
    {row, col} = pos

    if row < 0 or row >= map_height(m) or col < 0 or col >= map_width(m) do
      "."
    else
      elem(elem(m, row), col)
    end
  end

  def move({row, col}, dir) do
    case dir do
      :north -> {row - 1, col}
      :south -> {row + 1, col}
      :east -> {row, col + 1}
      :west -> {row, col - 1}
    end
  end

  def get_start(m) do
    Enum.find(range(m), fn pos -> map_at(m, pos) == "S" end)
  end

  def opp(dir) do
    case dir do
      :north -> :south
      :south -> :north
      :east -> :west
      :west -> :east
      :none -> :none
    end
  end

  def can_move(m, pos, dir) do
    new_pos = move(pos, dir)
    new_val = map_at(m, new_pos)
    new_dirs = dirs(new_val)
    Enum.member?(new_dirs, opp(dir))
  end

  def follow_path(m, path, pos, dir, start) do
    new_pos = move(pos, dir)
    # IO.inspect({"follow", pos, dir, new_pos, start, map_at(m, new_pos)})

    cond do
      !Enum.empty?(path) and pos == start ->
        # IO.inspect("loop")
        path

      !can_move(m, pos, dir) ->
        # IO.inspect("ouch")
        nil

      true ->
        new_dir = Enum.find(dirs(map_at(m, new_pos)), fn x -> x != opp(dir) end)
        new_path = [{pos, dir}] ++ path
        follow_path(m, new_path, new_pos, new_dir, start)
    end
  end

  def find_loop(m, start) do
    Stream.map(
      [:north, :south, :east, :west],
      fn dir ->
        follow_path(m, [], start, dir, start)
      end
    )
    |> Stream.filter(fn n -> n != nil end)
    |> Enum.at(0)
  end

  def count_area(m, rawloop) do
    loop = MapSet.new(rawloop)

    Enum.map(0..(map_height(m) - 1), fn row ->
      Enum.map(0..(map_width(m) - 1), &{row, &1})
      |> Enum.map(fn c ->
        if MapSet.member?(loop, c) do
          map_at(m, c)
        else
          "."
        end
      end)
      |> Enum.filter(&(&1 != "-"))
      |> Enum.join("")
      |> String.replace(~r/(LJ)|(F7)/, "")
      |> String.replace(~r/(L7)|(FJ)/, "|")
      |> String.split("", trim: true)
      |> Enum.reduce({false, 0}, fn sym, {inside, count} ->
        cond do
          Enum.member?(["|"], sym) ->
            {!inside, count}

          inside == true ->
            {inside, count + 1}

          true ->
            {inside, count}
        end
      end)
    end)
    |> Enum.map(fn a ->
      elem(a, 1)
    end)
    |> Enum.sum()
  end

  def main(f) do
    m = Day10.parse_input(f)
    start = get_start(m)

    loop =
      find_loop(m, start)

    IO.inspect({"loop length", Enum.count(loop), "half length", Enum.count(loop) / 2})

    start_dir = opp(elem(List.first(loop), 1))
    end_dir = elem(List.last(loop), 1)
    start_char = find_dir(start_dir, end_dir)

    {start_row, start_col} = start
    new_row = put_elem(elem(m, start_row), start_col, start_char)
    new_m = put_elem(m, start_row, new_row)

    plain_loop = loop |> Enum.map(&elem(&1, 0))
    IO.inspect({"area", count_area(new_m, plain_loop)})
  end
end

Day10.main("day10.sample3")
