package example

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest._

class PokerCombinationMatcherTest extends FlatSpec with Matchers {
  "evaluate" should "evaluate Royal Flush" in {
    val pokerHand = PlayerHand.fromString(Seq("10H", "JH", "QH", "AH", "KH"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual RoyalFlushResult
  }

  it should "evaluate straight Flush" in {
    val pokerHand = PlayerHand.fromString(Seq("10H", "JH", "QH", "9H", "8H"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual StraightFlushResult(pokerHand)
  }

  it should "evaluate four Of its kind" in {
    val pokerHand = PlayerHand.fromString(Seq("10H", "JH", "10C", "10S", "10D"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual FourOfAKindResult(pokerHand)
  }

  it should "evaluate full house" in {
    val pokerHand = PlayerHand.fromString(Seq("10H", "JH", "10C", "JS", "10D"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual FullHouseResult(pokerHand)
  }

  it should "evaluate flush" in {
      val pokerHand = PlayerHand.fromString(Seq("10D", "2D", "5D", "JD", "KD"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual FlushResult(pokerHand)
  }

  it should "evaluate straight" in {
    val pokerHand = PlayerHand.fromString(Seq("6D", "4D", "5H", "8S", "7D"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual StraightResult(pokerHand)
  }
  it should "evaluate three of a kind" in {
    val pokerHand = PlayerHand.fromString(Seq("6D", "4D", "5H", "5C", "5S"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual ThreeOfAKindResult(pokerHand)
  }
  it should "evaluate two pairs" in {
    val pokerHand = PlayerHand.fromString(Seq("6D", "4D", "4H", "5C", "5S"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual TwoPairsResult(pokerHand)
  }

  it should "evaluate pairs" in {
    val pokerHand = PlayerHand.fromString(Seq("6D", "4D", "4H", "9C", "5S"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual PairResult(pokerHand)
  }
  it should "evaluate high card" in {
    val pokerHand = PlayerHand.fromString(Seq("6D", "3S", "4H", "9C", "2S"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual HighCardResult(pokerHand)
  }

}
