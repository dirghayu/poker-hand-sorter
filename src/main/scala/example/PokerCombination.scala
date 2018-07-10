package example

object PokerCombinationMatcher {
  def evaluate(pokerHand: PokerHand): Result = {
    val sequenceOfCombination: PartialFunction[PokerHand, Result] = royalFlush orElse straightFlush orElse fourOfAKind orElse fullHouse orElse
    flush orElse straight orElse threeOfAKind orElse twoPairs orElse pair orElse highCard

    sequenceOfCombination(pokerHand)
  }

  def highCard: PartialFunction[PokerHand, Result] = {
    case pokerHand  => HighCardResult
  }

  def pair: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.containsPair) => PairResult
  }

  def twoPairs: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.containsTwoPairs) => TwoPairsResult
  }

  def threeOfAKind: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.containsThreeOfAKind) => ThreeOfAKindResult
  }

  def straight: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.allConsecutiveOrder) => StraightResult
  }

  def flush: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.allSameSuite) => FlushResult
  }
  def fullHouse: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.containsThreeOfAKind && pokerHand.containsPair) => FullHouseResult
  }

  def fourOfAKind: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.containsFourOfAKind) => FourOfAKindResult
  }
  def straightFlush: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.allSameSuite && pokerHand.allConsecutiveOrder) => StraightFlushResult
  }
  def royalFlush: PartialFunction[PokerHand, Result] = {
    case pokerHand if (pokerHand.isRoyalFlush) => RoyalFlushResult
  }

}



trait Result {
  def rank: Int
}


case object HighCardResult extends Result {
  override def rank: Int = 1
}

case object PairResult extends Result {
  override def rank: Int = 2
}
case object TwoPairsResult extends Result {
  override def rank: Int = 3
}
case object ThreeOfAKindResult extends Result {
  override def rank: Int = 4
}
case object StraightResult extends Result {
  override def rank: Int = 5
}
case object FlushResult extends Result {
  override def rank: Int = 6
}
case object FullHouseResult extends Result {
  override def rank: Int =7
}
case object FourOfAKindResult extends Result {
  override def rank: Int = 8
}

case object StraightFlushResult extends Result {
  override def rank: Int = 9
}
case object RoyalFlushResult extends Result {
  override def rank: Int = 10
}