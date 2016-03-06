import Generator.{monoImageToPuzzle, textToImage}
import Deducer.solve
import Show.allToGrids

object HanjieWord extends App {
  val testText = "SIMON MILES"
  val puzzle = monoImageToPuzzle (textToImage (testText))

  println (puzzle._1)
  println (puzzle._2)
  println ()

  println ("Solutions: ")
  val solutions = solve (puzzle._1, puzzle._2)
  println ()
  println ()
  println (allToGrids (solutions))
  println (solutions.size + " solution(s)")
}
