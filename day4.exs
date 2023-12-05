defmodule Day4 do
  def read_file(filename) do
    text = File.read!(filename)
    String.split(text, "\n", parts: :infinity, trim: true)
  end

  def split_numbers(s) do
    parts = String.split(s, " ", parts: :infinity, trim: true)
    Enum.map(parts, &String.to_integer/1)
  end

  def parse_line(line) do
    [left, right] = String.split(line, ":", trim: true)
    [_, idString] = String.split(left, " ", trim: true)
    id = String.to_integer(idString)
    [winnersString, numbersString] = String.split(right, "|", trim: true)

    winners = split_numbers(winnersString)
    numbers = split_numbers(numbersString)

    {id, winners, numbers}
  end

  def parse_input(filename) do
    lines = read_file(filename)
    Enum.map(lines, &parse_line/1)
  end

  def exp(a, b) do
    cond do
      b <= 0 ->
        1

      true ->
        a * exp(a, b - 1)
    end
  end

  def score_card(card) do
    # Process.sleep(500)
    {_, winners, numbers} = card
    wset = MapSet.new(winners)
    nset = MapSet.new(numbers)
    inter = MapSet.intersection(wset, nset)
    matches = MapSet.size(inter)

    cond do
      matches == 0 ->
        0

      true ->
        exp(2, matches - 1)
    end
  end

  def main(filename) do
    cards = parse_input(filename)
    tasks = Enum.map(cards, fn card -> Task.async(fn -> score_card(card) end) end)
    scores = Task.await_many(tasks)
    # scores = Enum.map(cards, &score_card/1)
    Enum.sum(scores)
  end
end

IO.inspect(Day4.main("day4.sample"))
