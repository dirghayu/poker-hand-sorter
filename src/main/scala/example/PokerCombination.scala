package example

object PokerCombinationMatcher {
  def evaluate(pokerHand: PlayerHand): Result = {
    val sequenceOfCombination: PartialFunction[PlayerHand, Result] = royalFlush orElse straightFlush orElse fourOfAKind orElse fullHouse orElse
    flush orElse straight orElse threeOfAKind orElse twoPairs orElse pair orElse highCard

    sequenceOfCombination(pokerHand)
  }

  def highCard: PartialFunction[PlayerHand, Result] = {
    case pokerHand  => HighCardResult(pokerHand)
  }

  def pair: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.containsPair) => PairResult(pokerHand)
  }

  def twoPairs: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.containsTwoPairs) => TwoPairsResult(pokerHand)
  }

  def threeOfAKind: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.containsThreeOfAKind) => ThreeOfAKindResult(pokerHand)
  }

  def straight: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.allConsecutiveOrder) => StraightResult(pokerHand)
  }

  def flush: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.allSameSuite) => FlushResult(pokerHand)
  }
  def fullHouse: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.containsThreeOfAKind && pokerHand.containsPair) =>
      FullHouseResult(pokerHand)
  }

  def fourOfAKind: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.containsFourOfAKind) =>
      FourOfAKindResult(pokerHand)
  }
  def straightFlush: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.allSameSuite && pokerHand.allConsecutiveOrder) =>
      StraightFlushResult(pokerHand)
  }
  def royalFlush: PartialFunction[PlayerHand, Result] = {
    case pokerHand if (pokerHand.isRoyalFlush) => RoyalFlushResult
  }

}



trait Result extends Comparable[Result]{
  def rank: Int
  def compareResultRank(result: Result): Int ={
    rank - result.rank
  }
}


case class HighCardResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int = 1
  val allCards: Seq[CardValue] = pokerHand.cards.map(_.value)

  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherHighCardResult = other.asInstanceOf[HighCardResult]
      val  orderedRanks: Seq[(Int, Int)] = allCards.map(_.rank).sorted.reverse zip
        otherHighCardResult.allCards.map(_.rank).sorted.reverse
      val firstNonZeroDiff = orderedRanks.find(a => a._1 - a._2 != 0)
      firstNonZeroDiff.map(p => p._1 - p._2).getOrElse(0)
    } else {
      compareResultRank(other)
    }
  }
}

case class PairResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int = 2
  val pair: CardValue = pokerHand.pairs.keySet.head
  val remainingCards: Seq[CardValue] = (pokerHand.cardsPerKind.keySet - pair).toSeq

  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherPairResult = other.asInstanceOf[PairResult]
      val rankDiff = pair.rank - otherPairResult.pair.rank
      if(rankDiff == 0) {
        val  orderedRanks: Seq[(Int, Int)] = remainingCards.map(_.rank).sorted.reverse zip
          otherPairResult.remainingCards.map(_.rank).sorted.reverse
        val firstNonZeroDiff = orderedRanks.find(a => a._1 - a._2 != 0)
        firstNonZeroDiff.map(p => p._1 - p._2).getOrElse(0)
      } else {
        rankDiff
      }
    } else {
      compareResultRank(other)
    }
  }}
case class TwoPairsResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int = 3
  val pairs: Seq[CardValue] = pokerHand.pairs.keys.toSeq
  val remainingCard: CardValue = (pokerHand.cardsPerKind.keySet diff pairs.toSet).head
  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherTwoOfKindResult = other.asInstanceOf[TwoPairsResult]
      val  orderedRankPairs: Seq[(Int, Int)] = pairs.map(_.rank).sorted.reverse zip
        otherTwoOfKindResult.pairs.map(_.rank).sorted.reverse
      val firstNonZeroDiff = orderedRankPairs.find(a => a._1 - a._2 != 0)
      val pairComparisionResult = firstNonZeroDiff.map(p => p._1 - p._2).getOrElse(0)

      if(pairComparisionResult==0){
        remainingCard.rank - otherTwoOfKindResult.remainingCard.rank
      }
      else {
        pairComparisionResult
      }

    } else {
      compareResultRank(other)
    }
  }
}
case class ThreeOfAKindResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int = 4
  val repeatedCard: CardValue = pokerHand.threeOfAKind.keySet.head
  val remainingCards: Seq[CardValue] = (pokerHand.cardsPerKind.keySet - repeatedCard).toSeq

  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherThreeOfKindResult = other.asInstanceOf[ThreeOfAKindResult]
      val rankDiff = repeatedCard.rank - otherThreeOfKindResult.repeatedCard.rank
      if(rankDiff == 0) {
        val  orderedRanks: Seq[(Int, Int)] = remainingCards.map(_.rank).sorted.reverse zip
          otherThreeOfKindResult.remainingCards.map(_.rank).sorted.reverse
        val firstNonZeroDiff = orderedRanks.find(a => a._1 - a._2 != 0)
        firstNonZeroDiff.map(p => p._1 - p._2).getOrElse(0)
      } else {
        rankDiff
      }

    } else {
      compareResultRank(other)
    }
  }
}
case class StraightResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int = 5

  val cards: Seq[CardValue] = pokerHand.cards.map(_.value)

  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherStraightResult = other.asInstanceOf[StraightResult]

      cards.maxBy(_.rank).rank - otherStraightResult.cards.maxBy(_.rank).rank
    } else {
      compareResultRank(other)
    }
  }
}
case class FlushResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int = 6
  val cards: Seq[CardValue] = pokerHand.cards.map(_.value)

  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherFlushResult = other.asInstanceOf[FlushResult]
      val  orderedRankPairs: Seq[(Int, Int)] = ( cards.map(_.rank).sorted.reverse zip
      otherFlushResult.cards.map(_.rank).sorted.reverse)
      val firstNonZeroDiff = orderedRankPairs.find(a => a._1 - a._2 != 0)
       firstNonZeroDiff.map(p => p._1 - p._2).getOrElse(0)
    } else {
      compareResultRank(other)
    }
  }
}
case class FullHouseResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int =7

  val repeatedThrice: CardValue = pokerHand.threeOfAKind.head._1
  val pair: CardValue = pokerHand.pairs.head._1


  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherFullHouseResult = other.asInstanceOf[FullHouseResult]
      val repeatedThriceDiff = repeatedThrice.rank - otherFullHouseResult.repeatedThrice.rank
      if(repeatedThriceDiff == 0) {
        pair.rank - otherFullHouseResult.pair.rank
      } else {
        repeatedThriceDiff
      }
    } else {
      compareResultRank(other)
    }
  }}
case class FourOfAKindResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int = 8
  val repeatedCard: CardValue = pokerHand.fourOfAkind.head._1
  val remainingCard = (pokerHand.cardsPerKind.keySet - repeatedCard).head

  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherHighestResult = other.asInstanceOf[FourOfAKindResult]
      if(repeatedCard.rank != otherHighestResult.repeatedCard.rank){
        repeatedCard.rank - otherHighestResult.repeatedCard.rank
      } else {
        remainingCard.rank - otherHighestResult.remainingCard.rank
      }

    } else {
      compareResultRank(other)
    }
  }
}

case class StraightFlushResult(pokerHand: PlayerHand) extends Result {
  override def rank: Int = 9
  val cards = pokerHand.cards.map(_.value)

  override def compareTo(other: Result): Int = {
    if(compareResultRank(other)== 0) {
      val otherHighestResult = other.asInstanceOf[StraightFlushResult]
      cards.map(_.rank).max - otherHighestResult.cards.map(_.rank).max
    } else {
      compareResultRank(other)
    }

  }
}
case object RoyalFlushResult extends Result {
  override def rank: Int = 10

  override def compareTo(other: Result): Int =  compareResultRank(other)

}