Code.require_file("common.ex")

# Realized that this doesn't work in the general case,
# since the first cycle isn't necessarily repeatable
# with the same cycle length.  But it worked, so
# not digging farther.

defmodule Day8 do
  def parse_input(file) do
    [first | rest] = Common.read_file(file)
    dirs = String.split(first, "", trim: true)

    cells =
      rest
      |> Enum.map(fn s ->
        [_, src, left, right] = Regex.run(~r/(...) = \((...), (...)\)/, s)
        {src, {left, right}}
      end)
      |> Map.new()

    {dirs, cells}
  end

  def num_steps(dirs, cells, start_loc, end_test) do
    Stream.cycle(dirs)
    |> Stream.scan(start_loc, fn dir, loc ->
      {left, right} = Map.get(cells, loc)

      new_loc = if dir == "L", do: left, else: right

      # IO.inspect({dir, loc, left, right, new_loc})
      new_loc
    end)
    |> Stream.with_index(1)
    |> Stream.drop_while(fn {loc, _} -> !end_test.(loc) end)
    |> Enum.at(0)
  end

  def main(file) do
    {dirs, cells} = parse_input(file)
    num_steps(dirs, cells, "AAA", fn loc -> loc == "ZZZ" end)
  end

  def main2(file) do
    {dirs, cells} = parse_input(file)

    cells
    |> Map.keys()
    |> Enum.filter(fn s -> Regex.match?(~r/..A/, s) end)
    |> Enum.map(&num_steps(dirs, cells, &1, fn loc -> Regex.match?(~r/..Z/, loc) end))
    |> IO.inspect()
    |> Enum.map(&elem(&1, 1))
    |> Enum.reduce(1, fn left, right ->
      Integer.floor_div(left * right, Integer.gcd(left, right))
    end)
  end
end

IO.inspect(Day8.main2("day8.sample3"))
