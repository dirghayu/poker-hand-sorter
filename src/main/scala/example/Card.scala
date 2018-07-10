package example

case class Card(face: Char , suit: Char )

object Card{
  def fromString(card: String) = Card(card.charAt(0), card.charAt(1))
}
