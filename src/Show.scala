object Show {
  def entriesToGrid (width: Int, height: Int, entries: Set[Entry], rowOffset: Int, columnOffset: Int): String =
    (for (gRow <- 0 until height) yield
      (for (gCol <- 0 until width) yield
        entries.find (e => e.row == gRow + rowOffset && e.column == gCol + columnOffset).map { e =>
          if (e.filled) "*" else "."
        }.getOrElse ("?")).mkString).mkString ("\n")

  def allToGrids (width: Int, height: Int, entries: Seq[Set[Entry]]): String =
    entries.map (solution => entriesToGrid (width, height, solution, 0, 0)).mkString ("\n\n")
}
