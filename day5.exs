require Common

defmodule Day5 do
  def parse_input(filename) do
    lines = Common.read_file(filename)
    [raw_seeds | rest] = lines

    maps =
      Enum.chunk_by(rest, fn s -> String.match?(s, ~r/^[a-z]/) end)
      |> Enum.chunk_every(2)
      |> Enum.map(fn [[raw_name], raw_rules] ->
        [_, from, to] = Regex.run(~r/([a-z]+)-to-([a-z]+)/, raw_name)

        rules =
          Enum.map(raw_rules, fn rr ->
            String.split(rr, " ", trim: true)
            |> Enum.map(&String.to_integer/1)
          end)

        {from, to, rules}
      end)
      |> Enum.reduce(%{}, fn {from, to, rules}, acc ->
        Map.put(acc, from, {to, rules})
      end)

    seeds =
      String.split(raw_seeds, " ")
      |> List.delete_at(0)
      |> Enum.map(&String.to_integer/1)

    {seeds, maps}
  end

  def apply_rules(rules, index) do
    Enum.find_value(rules, index, fn [dest, src, len] ->
      cond do
        index >= src and index < src + len ->
          dest + index - src

        true ->
          nil
      end
    end)
  end

  def iterate(maps, type, index) do
    # IO.inspect({"iterate", type, index})
    {new_type, rules} = Map.get(maps, type)
    {new_type, apply_rules(rules, index)}
  end

  def get_location(_, type, index) when type == "location" do
    index
  end

  def get_location(maps, type, index) do
    # IO.inspect({"get", type, index})
    {new_type, new_index} = iterate(maps, type, index)
    get_location(maps, new_type, new_index)
  end

  def main1(filename) do
    {seeds, maps} = parse_input(filename)
    Enum.map(seeds, fn s -> get_location(maps, "seed", s) end) |> Enum.min()
  end

  def partition_range(range, point) do
    # IO.inspect({"cutting range at", range, point})

    cond do
      range.first < point and range.last >= point ->
        # IO.inspect("cut!!!")
        [Range.new(range.first, point - 1), Range.new(point, range.last)]

      true ->
        [range]
    end
  end

  def partition_ranges_multiple(ranges, points) do
    Enum.reduce(points, ranges, fn point, acc ->
      Enum.flat_map(acc, fn a -> partition_range(a, point) end)
    end)
  end

  def apply_rules_ranged(rules, ranges) do
    # [12, 35] partitioned into [20, 25] and [30, 40] =>
    #     [[12, 19], [20, 25], [26, 29], [30, 35]
    new_source_ranges =
      Enum.reduce(rules, ranges, fn rule, acc ->
        [dest, src, len] = rule
        new_r = partition_ranges_multiple(acc, [src, src + len])
        new_r
      end)

    # IO.inspect({"new_source", new_source_ranges})

    end_ranges =
      Enum.map(new_source_ranges, fn range ->
        Enum.find_value(rules, range, fn rule ->
          [dest, src, len] = rule
          rule_as_range = Range.new(src, src + len - 1)
          # IO.inspect({"checking", range, rule_as_range, Range.disjoint?(range, rule_as_range)})

          cond do
            Range.disjoint?(rule_as_range, range) ->
              nil

            true ->
              Range.shift(range, dest - src)
          end
        end)
      end)

    # IO.inspect({"new ranges", end_ranges})
    end_ranges
  end

  def iterate_ranged(maps, type, ranges) do
    # IO.inspect({"iterate", type, ranges})
    {new_type, rules} = Map.get(maps, type)
    {new_type, apply_rules_ranged(rules, ranges)}
  end

  def get_location_ranged(_, type, ranges) when type == "location" do
    ranges
  end

  def get_location_ranged(maps, type, ranges) do
    # IO.inspect({"get", type, ranges})
    {new_type, new_ranges} = iterate_ranged(maps, type, ranges)
    get_location_ranged(maps, new_type, new_ranges)
  end

  def main2(filename) do
    {seeds, maps} = parse_input(filename)

    seed_ranges =
      Enum.chunk_every(seeds, 2)
      |> Enum.map(fn [start, len] ->
        Range.new(start, start + len - 1)
      end)

    location_ranges = get_location_ranged(maps, "seed", seed_ranges)
    Enum.min(Enum.map(location_ranges, & &1.first))
  end
end

IO.inspect(Day5.main2("day5.sample"))
