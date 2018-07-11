package example


case class PlayerHand(cards: Seq[Card]){
  assert(cards.size == 5, "Poker hand should have 5 cards")

  lazy val cardsPerKind: Map[CardValue, Int] = cards.groupBy(_.value).mapValues(_.size)
  lazy val cardsPerSuit: Map[String, Int] = cards.groupBy(_.suit).mapValues(_.size)
  lazy val pairs = cardsPerKind.filter(c => c._2 == 2)
  lazy val threeOfAKind = cardsPerKind.filter(c=> c._2 == 3)
  lazy val fourOfAkind = cardsPerKind.filter(c=> c._2 == 4)

  lazy val  allSameSuite: Boolean = cardsPerSuit.size == 1
  lazy val  containsPair: Boolean = cardsPerKind.exists(c=> c._2 == 2)
  lazy val  containsTwoPairs: Boolean = pairs.size == 2
  lazy val  containsThreeOfAKind: Boolean = threeOfAKind.size ==1
  lazy val  containsFourOfAKind: Boolean = fourOfAkind.size == 1
  lazy val allConsecutiveOrder: Boolean = {
    val sortedCardRanks: Seq[Int] = cards.map(_.value.rank).sorted
    val zipWithNeighbourRank: Seq[(Int, Int)] = sortedCardRanks.zip(sortedCardRanks.tail)
    zipWithNeighbourRank.forall(z=> ( z._2 - z._1 ==1))
  }


  lazy val  isRoyalFlush: Boolean = {
    val sortedCardRanks: Seq[Int] = cards.map(_.value.rank).sorted

    (sortedCardRanks.head == 10 && allConsecutiveOrder && allSameSuite)
  }

  def compareTo(otherPlayerHand: PlayerHand): Int ={
    val myCombinationResult = PokerCombinationMatcher.evaluate(this)
    val  otherPlayerCombinationResult = PokerCombinationMatcher.evaluate(otherPlayerHand)
    myCombinationResult.compareTo(otherPlayerCombinationResult)
  }

}

object PlayerHand {
  def fromString(cards: Seq[String]) : PlayerHand = {
    PlayerHand(cards.map(Card.fromString))
  }


}

