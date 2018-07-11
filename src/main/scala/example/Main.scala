package example

import scala.io.Source
import collection.JavaConverters._
object Main  {

  def main(args: Array[String]) = {
    val bufferedSource = Source.fromFile(args(0))

    val lines = readFile(args(0))
    val wordsPerLine = lines.map(str=> str.split(" ").map(_.trim).toSeq)

    println(s"args::"+ args.size)
    val games = wordsPerLine.map(
      s=>
        (PlayerHand.fromString(s.take(5)),
          PlayerHand.fromString(s.takeRight(5))))

    val game = new PokerGame(games);
    val results: (Int, Int) = game.evaluateGames()

    println(s"Player 1: ${results._1} hands")
    println(s"Player 2: ${results._2} hands")

  }

  def readFile(filename: String): Seq[String] = {
    val bufferedSource = io.Source.fromFile(filename)
    val lines = (for (line <- bufferedSource.getLines()) yield line).toList
    bufferedSource.close
    lines
  }
}