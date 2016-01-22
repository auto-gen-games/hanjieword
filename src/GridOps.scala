object GridOps {
  implicit class BoolArrayAsGrid (grid: Seq[Seq[Boolean]]) {
    def height: Int =
      grid.size

    def surround (filled: Boolean): Seq[Seq[Boolean]] =
      Seq.fill (width + 2)(filled) +: grid.map (filled +: _ :+ filled) :+ Seq.fill (width + 2)(filled)

    def removeGaps: Seq[Seq[Boolean]] =
      grid.filter (_.contains (true)).transpose.filter (_.contains (true)).transpose

    def trimEdges: Seq[Seq[Boolean]] =
      grid.trimTopAndBottom.trimLeftAndRight

    def trimLeftAndRight: Seq[Seq[Boolean]] =
      grid.transpose.trimTopAndBottom.transpose

    def trimTopAndBottom: Seq[Seq[Boolean]] =
      grid.dropWhile (!_.contains (true)).reverse.dropWhile (!_.contains (true)).reverse

    def width: Int =
      grid.head.size
  }
}