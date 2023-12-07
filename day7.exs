Code.require_file("common.ex")

defmodule Day7 do
  # @cards "AKQJT98765432" |> String.split("", trim: true) |> Enum.map(&String.to_atom/1)
  @cards "AKQT98765432J" |> String.split("", trim: true) |> Enum.map(&String.to_atom/1)

  @card_values @cards |> Enum.reverse() |> Enum.with_index() |> Enum.into(%{})
  @ranks [
    :five_of_a_kind,
    :four_of_a_kind,
    :full_house,
    :three_of_a_kind,
    :two_pair,
    :one_pair,
    :high_card
  ]
  @rank_values @ranks |> Enum.reverse() |> Enum.with_index() |> Enum.into(%{})

  def parse_input(filename) do
    lines = Common.read_file(filename)

    lines
    |> Enum.map(fn line ->
      [handStr, bidStr] = String.split(line, " ")

      hand =
        handStr |> String.split("", trim: true) |> Enum.map(&String.to_atom/1)

      bid = String.to_integer(bidStr)
      {hand, bid}
    end)
  end

  def rank_hand(original_hand) do
    hand = apply_jokers(original_hand)
    counts = Enum.frequencies(hand) |> Map.values() |> Enum.sort() |> Enum.reverse()

    case counts do
      [5 | _] -> :five_of_a_kind
      [4 | _] -> :four_of_a_kind
      [3, 2] -> :full_house
      [3 | _] -> :three_of_a_kind
      [2, 2 | _] -> :two_pair
      [2 | _] -> :one_pair
      _ -> :high_card
    end
  end

  def compare(line1, line2) do
    {{hand1, rank1}, _} = line1
    {{hand2, rank2}, _} = line2

    if rank1 != rank2 do
      @rank_values[rank1] < @rank_values[rank2]
    else
      Enum.zip(hand1, hand2)
      |> Enum.find_value({List.first(hand1), List.first(hand2)}, fn {card1, card2} ->
        card1 != card2 && {card1, card2}
      end)
      |> (fn {card1, card2} -> @card_values[card1] <= @card_values[card2] end).()
    end
  end

  def apply_jokers(hand) do
    other_freqs = Enum.frequencies(hand) |> Map.delete(:J)

    if Enum.empty?(other_freqs) do
      hand
    else
      max_freq = Map.values(other_freqs) |> Enum.max()

      max_card =
        Map.to_list(other_freqs)
        |> Enum.find(fn {_card, freq} -> freq == max_freq end)
        |> elem(0)

      Enum.map(hand, fn card -> if card == :J, do: max_card, else: card end)
    end
  end

  def main(filename) do
    hands = parse_input(filename)

    Enum.map(hands, fn {hand, bid} -> {{hand, rank_hand(hand)}, bid} end)
    |> Enum.sort(&compare/2)
    |> Enum.with_index(1)
    |> Enum.map(fn {{_hand, bid}, index} -> index * bid end)
    |> Enum.sum()
  end
end

IO.inspect(Day7.main("day7.sample"))
