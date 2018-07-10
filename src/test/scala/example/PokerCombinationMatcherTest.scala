package example

import example.PokerHand.fromString
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest._

class PokerCombinationMatcherTest extends FlatSpec with Matchers {
  "evaluate" should "evaluate Royal Flush" in {
    val pokerHand = fromString(Seq("10H", "JH", "QH", "AH", "KH"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual RoyalFlushResult
  }

  it should "evaluate straight Flush" in {
    val pokerHand = fromString(Seq("10H", "JH", "QH", "9H", "8H"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual StraightFlushResult
  }

  it should "evaluate four Of its kind" in {
    val pokerHand = fromString(Seq("10H", "JH", "10C", "10S", "10D"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual FourOfAKindResult
  }

  it should "evaluate full house" in {
    val pokerHand = fromString(Seq("10H", "JH", "10C", "JS", "10D"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual FullHouseResult
  }

  it should "evaluate flush" in {
      val pokerHand = fromString(Seq("10D", "2D", "5D", "JD", "KD"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual FlushResult
  }

  it should "evaluate straight" in {
    val pokerHand = fromString(Seq("6D", "4D", "5H", "8S", "7D"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual StraightResult
  }
  it should "evaluate three of a kind" in {
    val pokerHand = fromString(Seq("6D", "4D", "5H", "5C", "5S"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual ThreeOfAKindResult
  }
  it should "evaluate two pairs" in {
    val pokerHand = fromString(Seq("6D", "4D", "4H", "5C", "5S"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual TwoPairsResult
  }

  it should "evaluate pairs" in {
    val pokerHand = fromString(Seq("6D", "4D", "4H", "9C", "5S"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual PairResult
  }
  it should "evaluate high card" in {
    val pokerHand = fromString(Seq("6D", "4S", "4H", "9C", "2S"))
    PokerCombinationMatcher.evaluate(pokerHand) shouldEqual PairResult
  }

}
