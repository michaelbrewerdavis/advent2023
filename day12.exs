defmodule Day12 do
  def parse_input(f) do
    File.read!(f)
    |> String.split("\n", trim: true)
    |> Enum.map(fn line ->
      [left, right] = String.split(line, " ")

      springs =
        left
        |> Stream.duplicate(5)
        |> Enum.join("?")
        |> String.split("", trim: true)

      counts =
        right
        |> Stream.duplicate(5)
        |> Enum.join(",")
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)

      {springs, counts}
    end)
  end

  def count_possibilities({[], counts}, _) do
    if counts == [] do
      # IO.inspect("satisfied")
      {1, %{}}
    else
      # IO.inspect("empty list with non-empty counts")
      {0, %{}}
    end
  end

  def count_possibilities({springs, []}, _) do
    if Enum.all?(springs, &(&1 != "#")) do
      # IO.inspect("done with counts, rest empty")
      {1, %{}}
    else
      # IO.inspect("done with counts, springs remain")
      {0, %{}}
    end
  end

  def count_possibilities({springs, counts}, _)
      when length(springs) < length(counts) do
    # IO.inspect("not long enough")
    {0, %{}}
  end

  def count_possibilities({springs, counts}, cache) do
    # IO.inspect({"call", springs, counts})
    cache_key = Enum.concat(springs, counts) |> Enum.join("#")
    cached = Map.get(cache, cache_key)

    if cached != nil do
      # IO.inspect("cached!!")
      {cached, cache}
    else
      [c | crest] = counts
      [s | srest] = springs

      {base, extra_cache_from_base} =
        if s != "#" do
          count_possibilities({srest, counts}, cache)
        else
          {0, %{}}
        end

      cache_with_base = Map.merge(cache, extra_cache_from_base)

      can_start_here =
        springs
        |> Enum.take(c)
        |> (fn e -> Enum.count(e) == c and Enum.all?(e, &(&1 != ".")) end).()

      can_end_here = springs |> Enum.drop(c) |> Enum.at(0) != "#"

      # IO.inspect({springs, counts, can_start_here, can_end_here})

      {result, extra_cache_from_step} =
        if can_start_here and can_end_here do
          {r, ec} = count_possibilities({Enum.drop(springs, c + 1), crest}, cache_with_base)
          {base + r, Map.merge(cache_with_base, ec)}
        else
          {base, cache_with_base}
        end

      new_cache = Map.put(extra_cache_from_step, cache_key, result)
      # IO.inspect({"result", springs, counts, base, result})
      {result, new_cache}
    end
  end

  def main(f) do
    parse_input(f)
    |> Enum.map(&count_possibilities(&1, %{}))
    |> Enum.map(&elem(&1, 0))
    |> IO.inspect()
    |> Enum.sum()
  end
end

Day12.main("day12.sample") |> IO.inspect()
