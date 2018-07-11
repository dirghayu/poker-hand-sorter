package example

import org.scalatest.{FlatSpec, Matchers}

class PlayerTest extends FlatSpec with Matchers {
  "Player" should "have 5 elemnts" in {
    val player = PlayerHand.fromString(Seq("AH", "2H", "8H", "9H", "3H"))

    player shouldNot be(null)

    assertThrows[AssertionError]{
      PlayerHand.fromString(Seq("AH", "2H", "8H", "9H"))
    }
  }

  "allSameSuite" should "work successfully" in {
    PlayerHand.fromString(Seq("AH", "2H", "8H", "9H", "3H")).allSameSuite  shouldBe true
    PlayerHand.fromString(Seq("AH", "2H", "8H", "9H", "3S")).allSameSuite  shouldBe false
  }

  "containsPair" should "work successfully" in {
    PlayerHand.fromString(Seq("AH", "2H", "8H", "9H", "3H")).containsPair  shouldBe false
    PlayerHand.fromString(Seq("AH", "2H", "8H", "9H", "2S")).containsPair  shouldBe true
    PlayerHand.fromString(Seq("AH", "2H", "8H", "2C", "2S")).containsPair  shouldBe false
  }

  "containsThreeOfAKind" should "work successfully" in {
    PlayerHand.fromString(Seq("AH", "2H", "8H", "9H", "3H")).containsThreeOfAKind  shouldBe false
    PlayerHand.fromString(Seq("AH", "2H", "8H", "9H", "2S")).containsThreeOfAKind  shouldBe false
    PlayerHand.fromString(Seq("AH", "2H", "8H", "2C", "2S")).containsThreeOfAKind  shouldBe true
  }
  "allConsecutiveOrder" should "work successfully" in {
    PlayerHand.fromString(Seq("AH", "2H", "4H", "5H", "3S")).allConsecutiveOrder  shouldBe false
    PlayerHand.fromString(Seq("4H", "6H", "5H", "3H", "2S")).allConsecutiveOrder  shouldBe true
    PlayerHand.fromString(Seq("AH", "2H", "8H", "2C", "2S")).allConsecutiveOrder  shouldBe false
    PlayerHand.fromString(Seq("9H", "10H", "KH", "QC", "JS")).allConsecutiveOrder  shouldBe true
  }

  "compareTo" should "compare pairs successfully" in {
    val player1= PlayerHand.fromString(Seq("4H", "4C", "6S", "7D", "KD"))
    val player2= PlayerHand.fromString(Seq("2C", "3S", "9S", "9D", "KD"))
    player1 compareTo player2  shouldBe (4 - 9)

  }
  it should "compare Highest Card successfully" in {
    val player1= PlayerHand.fromString(Seq("5D", "8C", "9S", "JS", "AC"))
    val player2= PlayerHand.fromString(Seq("2C", "5C", "7D", "8S", "QH"))
    player1 compareTo player2  shouldBe (13 - 11)

  }

  it should "compare Three Aces With Flush of Dimonds" in {
    val player1= PlayerHand.fromString(Seq("2D", "9C", "AS", "AH", "AC"))
    val player2= PlayerHand.fromString(Seq("3D", "6D", "7D", "JD", "QD"))

    val flushResultRank = 6
    val threeOfKindRank = 4
    player1 compareTo player2  shouldBe (threeOfKindRank - flushResultRank)
  }

  it should "compare three queens and three queens and fallback to highest card" in {
    val player1= PlayerHand.fromString(Seq("2D", "9C", "QS", "QH", "QC"))
    val player2= PlayerHand.fromString(Seq("3D", "6D", "QH", "QS", "QD"))
    player1 compareTo player2  shouldBe 9 - 6
  }
  it should "compare three fours with three threes" in {
    val player1= PlayerHand.fromString(Seq("2H", "2D", "4C", "4D", "4S"))
    val player2= PlayerHand.fromString(Seq("3C", "3D", "3S", "9S", "9D"))
    player1 compareTo player2  shouldBe 4 - 3
  }

}
