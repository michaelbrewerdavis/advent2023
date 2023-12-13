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

  def find_split_in_one_direction(strings) do
    IO.inspect(strings)

    1..(tuple_size(strings) - 1)
    |> Enum.find(fn n ->
      IO.inspect({"checking whether mirrors above n", n})

      0..tuple_size(strings)
      |> Enum.all?(fn delta ->
        if n + delta < tuple_size(strings) and n - delta - 1 >= 0 do
          IO.inspect(elem(strings, n + delta))
          IO.inspect(elem(strings, n - delta - 1))

          (elem(strings, n + delta) == elem(strings, n - delta - 1))
          |> IO.inspect()
        else
          true
        end
      end)
    end)
  end

  def find_split_x(box) do
    row_strings =
      box
      |> List.to_tuple()
      |> find_split_in_one_direction()
  end

  def find_split_y(box) do
    col_strings =
      Stream.zip(box)
      |> Enum.to_list()
      |> List.to_tuple()
      |> find_split_in_one_direction()
  end

  def find_split(box) do
    x =
      (find_split_y(box) ||
         find_split_x(box) * 100)
      |> IO.inspect()
  end

  def main(f) do
    parse_input(f) |> Enum.map(&find_split/1) |> IO.inspect() |> Enum.sum()
  end
end

Day13.main("day13.sample") |> IO.inspect()
