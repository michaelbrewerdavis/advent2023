Code.require_file("common.ex")

defmodule Day6 do
  def parse_line(s) do
    String.split(s, " ", trim: true, parts: :infinity)
    |> Enum.slice(1..-1)
    |> Enum.map(&String.to_integer/1)
  end

  def parse_input(filename) do
    [first, second] = Common.read_file(filename)
    times = parse_line(first)
    records = parse_line(second)
    {times, records}
  end

  def button_times(time, record) do
    # hold button for N seconds

    # distance is N * (time - N)
    # N * (time - N) > record
    # N * time - N^2 - record  > 0
    # N^2 - N*time + record < 0
    # x = (N +/- sqrt(time^2 - 4*record))/2

    radical = :math.sqrt(time * time - 4 * record)
    root1 = ((time - radical) / 2) |> (&:math.floor(&1 + 1)).() |> trunc
    root2 = ((time + radical) / 2) |> (&:math.ceil(&1 - 1)).() |> trunc
    Range.size(root1..root2)
  end

  def main1(filename) do
    {times, records} = parse_input(filename)

    ranges =
      Enum.zip(times, records) |> Enum.map(fn {time, record} -> button_times(time, record) end)

    IO.inspect(ranges)
    Enum.product(ranges)
  end

  def smush(list) do
    Enum.map(list, &Integer.to_string/1) |> Enum.join("") |> String.to_integer()
  end

  def main2(filename) do
    {times, records} = parse_input(filename)

    time = smush(times)
    record = smush(records)
    IO.inspect({time, record})

    range = button_times(time, record)
    IO.inspect(range)
  end
end

IO.inspect(Day6.main2("day6.sample"))
