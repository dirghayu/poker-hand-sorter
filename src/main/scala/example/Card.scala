package example


case class Card(value: CardValue , suit: String )

object Card{
  def fromString(card: String) = {
    val (value, suit) = card.splitAt(card.size -1)
    Card(CardValue.fromString(value), suit)
  }
}

case class CardValue(value: String, rank: Int)

object CardValue{

  val NumberedCards: Seq[CardValue] = for(n<- 2 to 10 ) yield (CardValue(""+ n, n))

  val JACK = CardValue("J", 11)
  val QUEEN = CardValue("Q", 12)
  val KING = CardValue("K", 13)
  val ACE = CardValue("A", 14)

  val FaceCards = Seq(JACK, QUEEN, KING, ACE)
  val allCards: Seq[CardValue] =  FaceCards ++ NumberedCards
  val stringToCharValueMap: Map[String, CardValue] =  (allCards.map(x=> (x.value, x))).toMap

  def fromString(str: String)  =
    stringToCharValueMap.get(str)
    .getOrElse(throw new IllegalArgumentException(s"Card value not valid: $str ."));

}


