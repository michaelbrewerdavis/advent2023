defmodule Day13 do
  def parse_input(f) do
    File.read!(f)
    |> String.split("\n\n", trim: true)
    |> Enum.map(fn box ->
      String.split(box, "\n", trim: true)
      |> Enum.map(fn line ->
        String.split(line, "", trim: true)
        |> Enum.map(fn c ->
          if c == "#" do
            1
          else
            0
          end
        end)
      end)
    end)
  end

  def differing_points_at_n(strings, n) do
    0..tuple_size(strings)
    |> Stream.flat_map(fn delta ->
      if n + delta < tuple_size(strings) and n - delta - 1 >= 0 do
        Enum.zip(elem(strings, n + delta), elem(strings, n - delta - 1))
        |> Enum.with_index()
        |> Enum.map(fn {{a, b}, index} -> if a != b, do: n, else: nil end)
        |> Enum.filter(&(&1 != nil))
      else
        []
      end
    end)
  end

  def find_mirror_in_one_direction(strings) do
    1..(tuple_size(strings) - 1)
    |> Enum.find(fn n ->
      differing_points_at_n(strings, n)
      # |> Enum.empty?()
      |> Enum.take(2)
      |> (&(Enum.count(&1) == 1)).()
    end)
  end

  def find_split_x(box) do
    row_strings =
      box
      |> List.to_tuple()
      |> find_mirror_in_one_direction()
  end

  def find_split_y(box) do
    col_strings =
      Stream.zip(box)
      |> Stream.map(&Tuple.to_list/1)
      |> Enum.to_list()
      |> List.to_tuple()
      |> find_mirror_in_one_direction()
  end

  def find_split(box) do
    find_split_y(box) ||
      find_split_x(box) * 100
  end

  def main(f) do
    parse_input(f)
    # |> Enum.take(1)
    |> Enum.map(&find_split/1)
    |> IO.inspect()
    |> Enum.sum()
  end
end

Day13.main("day13.sample") |> IO.inspect()
