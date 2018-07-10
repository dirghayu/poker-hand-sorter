package example

import example.PokerHand.fromString
import org.scalatest.{FlatSpec, Matchers}

class PokerHandTest extends FlatSpec with Matchers {
  "PokerHand" should "have 5 elemnts" in {
    val pokerHand = fromString(Seq("AH", "2H", "8H", "9H", "3H"))

    pokerHand shouldNot be(null)

    assertThrows[AssertionError]{
      fromString(Seq("AH", "2H", "8H", "9H"))
    }
  }

  "allSameSuite" should "work successfully" in {
    fromString(Seq("AH", "2H", "8H", "9H", "3H")).allSameSuite  shouldBe true
    fromString(Seq("AH", "2H", "8H", "9H", "3S")).allSameSuite  shouldBe false
  }

  "containsPair" should "work successfully" in {
    fromString(Seq("AH", "2H", "8H", "9H", "3H")).containsPair  shouldBe false
    fromString(Seq("AH", "2H", "8H", "9H", "2S")).containsPair  shouldBe true
    fromString(Seq("AH", "2H", "8H", "2C", "2S")).containsPair  shouldBe false
  }

  "containsThreeOfAKind" should "work successfully" in {
    fromString(Seq("AH", "2H", "8H", "9H", "3H")).containsThreeOfAKind  shouldBe false
    fromString(Seq("AH", "2H", "8H", "9H", "2S")).containsThreeOfAKind  shouldBe false
    fromString(Seq("AH", "2H", "8H", "2C", "2S")).containsThreeOfAKind  shouldBe true
  }
  "allConsecutiveOrder" should "work successfully" in {
    fromString(Seq("AH", "2H", "4H", "5H", "3S")).allConsecutiveOrder  shouldBe false
    fromString(Seq("4H", "6H", "5H", "3H", "2S")).allConsecutiveOrder  shouldBe true
    fromString(Seq("AH", "2H", "8H", "2C", "2S")).allConsecutiveOrder  shouldBe false
    fromString(Seq("9H", "10H", "KH", "QC", "JS")).allConsecutiveOrder  shouldBe true
  }

}
