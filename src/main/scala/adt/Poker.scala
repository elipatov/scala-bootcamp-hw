package adt

import scala.collection.SortedSet

sealed trait Suit
object Suit {
  final case object Clubs extends Suit
  final case object Hearts extends Suit
  final case object Spades extends Suit
  final case object Diamonds extends Suit
}

sealed trait Rank
object Rank {
  final case object Ace extends Rank
  final case object King extends Rank
  final case object Queen extends Rank
  final case object Jack extends Rank
  final case object `10` extends Rank
  final case object `9` extends Rank
  final case object `8` extends Rank
  final case object `7` extends Rank
  final case object `6` extends Rank
  final case object `5` extends Rank
  final case object `4` extends Rank
  final case object `3` extends Rank
  final case object `2` extends Rank
}

final case class Card(rank: Rank, suit: Suit)

sealed trait Hand
sealed abstract case class TexasHoldemHand private(cards: List[Card]) extends Hand
object TexasHoldemHand {
  def create(cards: Set[Card]): Option[Hand] =
    cards match {
      case cs if cs.size == 2 => Some(new TexasHoldemHand(cards.toList) {})
      case _                    => None
    }
}

sealed abstract case class OhamaHoldemHand private (cards: List[Card]) extends Hand
object OhamaHoldemHand {
  def create(cards: Set[Card]): Option[Hand] =
    cards match {
      case cs if cs.size == 4 => Some(new OhamaHoldemHand(cards.toList) {})
      case _                    => None
    }
}

sealed abstract case class Board(cards: List[Card])
object Board {
  def create(cards: Set[Card]): Option[Board] =
    cards match {
      case cs if cs.size == 5 => Some(new Board(cards.toList) {})
      case _                    => None
    }
}

sealed trait Combo
object Combo {
  final case object StraightFlush extends Combo
  final case object FourOfAKind extends Combo
  final case object FullHouse extends Combo
  final case object Flush extends Combo
  final case object Straight extends Combo
  final case object ThreeOfAKind extends Combo
  final case object TwoPairs extends Combo
  final case object OnePair extends Combo
  final case object HighCard extends Combo
}

final case class TestCase(board: Board, hands: List[Hand]) {
  def solve(): TestResult = ???
}

final case class TestResult(board: Board, hands: SortedSet[Hand])
