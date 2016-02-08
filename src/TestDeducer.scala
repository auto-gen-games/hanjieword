object TestDeducer extends App {
  // ***.**.**...**
  // .***..**.**.**
  val rowLengths = Seq (Seq (3, 2, 2, 2), Seq (3, 2, 2, 2))
  val colLengths = Seq (Seq (1), Seq (2), Seq (2), Seq (1), Seq (1), Seq (1), Seq (1), Seq (2), Seq (1), Seq (1), Seq (1),
    Seq (0), Seq (2), Seq (2))

//  val rowLengths = Seq (Seq (1), Seq (1))
//  val colLengths = Seq (Seq (1), Seq (1))

  println (Deducer.certainties (14, Seq (3, 2, 2, 2), Set ()))
  println ()
  println ("\n\n" + Show.allToGrids (Deducer.solve (rowLengths, colLengths)))
}
