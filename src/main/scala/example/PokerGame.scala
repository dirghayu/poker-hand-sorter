package example

class PokerGame(games: Seq[(PlayerHand, PlayerHand)]) {

  def evaluateGames(): (Int,Int) ={
    val results: (Seq[Int], Seq[Int]) = games.map(g => g._1.compareTo(g._2)).partition(n=>n>0)
    val player1Wins = results._1.size
    val player2Wins = results._1.size

    (player1Wins, player2Wins)
  }
}
