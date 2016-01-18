object Solver {
  /** A possibility is a sequence that is a possible pattern on a row/column. */
  type Possibility = Seq[Entry]

  /** Return all possible patterns for a row/column given its lengths and the row/column width */
  def findRowPossibilities (index: Int, isRow: Boolean, allLengths: Seq[Int], width: Int): Seq[Possibility] = {
    /** Find all possibilities with the given lengths remaining to be allocated to the right of an allocated section (prefix). */
    def findRowPossibilitiesFollowing (lengths: Seq[Int], prefix: Possibility): Seq[Possibility] = {
      /** Create a series of entries of a given length from a given offset on the row, either filled or empty. */
      def cells (length: Int, filled: Boolean, offset: Int) =
        Seq.tabulate (length)(pos => if (isRow) Entry (index, offset + pos, filled) else Entry (offset + pos, index, filled))

      /** Create a series of entries for this row from a given offset, which is a sequence of empty cells followed by a
        * sequence of filled cells and then another sequence of empty. */
      def delimitedLength (left: Int, filled: Int, right: Boolean, offset: Int): Possibility =
          cells (left, false, offset) ++ cells (filled, true, offset + left) ++
            (if (right) cells (1, false, offset + left + filled) else Nil)

      /* The possibility for a row remainder with no lengths is just empty cells to the end of the row.
       * Otherwise, generate the possibilities by choosing different positions for the first length, then recursively
       * applying this function for the rest of the lengths. */
      if (lengths.isEmpty)
        Seq (prefix ++ cells (width - prefix.length, false, prefix.length))
      else
        (for (gap <- 0 to (width - prefix.length - lengths.sum - lengths.size + 1)) yield
          findRowPossibilitiesFollowing (lengths.tail, prefix ++
            delimitedLength (gap, lengths.head, lengths.tail.nonEmpty, prefix.length))
          ).flatten
    }

    // Find all possibilities for this row containing all lengths with no cells initially assigned.
    findRowPossibilitiesFollowing (allLengths, Nil)
  }

  /** Generate all the possibilities from each row and each column in the grid, given the grid's dimensions and the lengths. */
  def findGridPossibilities (width: Int, allLengths: Seq[Seq[Int]], isRow: Boolean): Seq[Possibility] =
    allLengths.zipWithIndex.flatMap {
      case (lengths, index) => findRowPossibilities (index, isRow, lengths, width)
    }

  /** Find each position with only one possible entry (empty or filled) in all the given possibilities, return those entries. */
  def certainties (possibilities: Seq[Possibility]): Set[Entry] =
    possibilities.flatten.groupBy {
      e => (e.row, e.column)
    }.mapValues (_.map (_.filled).toSet).filter (_._2.size == 1).map {
      p => Entry (p._1._1, p._1._2, p._2.head)
    }.toSet

  def filter (possibilities: Seq[Possibility], known: Set[Entry]): Seq[Possibility] =
    possibilities.filter (!_.exists (entry => known.contains (entry.opposite)))

  def entriesToGrid (width: Int, height: Int, entries: Set[Entry], rowOffset: Int, columnOffset: Int): Seq[Seq[Cell]] =
    for (gRow <- 0 until height) yield
      for (gCol <- 0 until width) yield
        entries.find (e => e.row == gRow + rowOffset && e.column == gCol + columnOffset).map { e =>
          if (e.filled) Filled else Empty
        }.getOrElse (Unknown)

  def gridToString (grid: Seq[Seq[Cell]]) =
    grid.map { _.map {
      case Filled => "*"
      case Empty => "."
      case Unknown => "?"
    }.mkString }.mkString ("\n")

  def solve (width: Int, height: Int, rowLengths: Seq[Seq[Int]], columnLengths: Seq[Seq[Int]]): Option[Set[Entry]] = {
    def deduce (rowPossibilities: Seq[Possibility], colPossibilities: Seq[Possibility], previouslyKnown: Set[Entry]): Set[Entry] = {
      val known = certainties (rowPossibilities) ++ certainties (colPossibilities)
      println (gridToString (entriesToGrid (width, height, known, 0, 0))); println ()
      if (known.size > previouslyKnown.size)
        deduce (filter (rowPossibilities, known), filter (colPossibilities, known), known)
      else
        known
    }

    val rowPossibilities = findGridPossibilities (width, rowLengths, true)
    val colPossibilities = findGridPossibilities (height, columnLengths, false)

    val solution = deduce (rowPossibilities, colPossibilities, Set[Entry] ())
    println (width + " " + height)
    println (solution.size)
    if (solution.size == width * height) Some (solution) else None
  }
}
