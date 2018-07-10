package example

case class PokerHand(cards: Seq[Card]){
  assert(cards.size == 5, "Poker hand should have 5 cards")

  lazy val cardsPerKind: Map[CardValue, Int] = cards.groupBy(_.value).mapValues(_.size)
  lazy val cardsPerSuit: Map[String, Int] = cards.groupBy(_.suit).mapValues(_.size)

  lazy val  allSameSuite: Boolean = cardsPerSuit.size == 1
  lazy val  containsPair: Boolean = cardsPerKind.exists(c=> c._2 == 2)
  lazy val  containsTwoPairs: Boolean = cardsPerKind.filter(c=> c._2 == 2).size == 2
  lazy val  containsThreeOfAKind: Boolean = cardsPerKind.exists(c=> c._2 == 3)
  lazy val  containsFourOfAKind: Boolean = cardsPerKind.exists(c=> c._2 == 4)
  lazy val allConsecutiveOrder: Boolean = {
    val sortedCardRanks: Seq[Int] = cards.map(_.value.rank).sorted
    val zipWithNeighbourRank: Seq[(Int, Int)] = sortedCardRanks.zip(sortedCardRanks.tail)
    zipWithNeighbourRank.forall(z=> ( z._2 - z._1 ==1))
  }

  lazy val  isRoyalFlush: Boolean = {
    val sortedCardRanks: Seq[Int] = cards.map(_.value.rank).sorted

    (sortedCardRanks.head == 10 && allConsecutiveOrder && allSameSuite)
  }

}

object PokerHand {
  def fromString(cards: Seq[String]) : PokerHand = {
    PokerHand(cards.map(Card.fromString))
  }
}
