package example

import org.scalatest._

class CardSpec extends FlatSpec with Matchers {
  "Card" should "parse string properly" in {
    Card.fromString("2A") shouldEqual Card(CardValue("2",2), "A")
    Card.fromString("9A") shouldEqual Card(CardValue("9",9), "A")
    Card.fromString("10A") shouldEqual Card(CardValue("10",10), "A")
    Card.fromString("AA") shouldEqual Card(CardValue("A",14), "A")
    Card.fromString("JA") shouldEqual Card(CardValue("J",11), "A")
    Card.fromString("QA") shouldEqual Card(CardValue("Q",12), "A")
  }

  "Card" should "throw" in {
    assertThrows[IllegalArgumentException]{
      Card.fromString("1A")
    }

    assertThrows[IllegalArgumentException]{
      Card.fromString("11A")
    }
  }
}
