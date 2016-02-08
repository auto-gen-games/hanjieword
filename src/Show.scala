object Show {
  def entriesToGrid (width: Int, height: Int, entries: Set[Entry]): String =
    (for (gRow <- 0 until height) yield
      (for (gCol <- 0 until width) yield
        entries.find (e => e.row == gRow && e.column == gCol).map { e =>
          if (e.filled) "*" else "."
        }.getOrElse ("?")).mkString).mkString ("\n")

  def entriesToGrid (entries: Set[Entry]): String =
    entriesToGrid (entries.map (_.column).max + 1, entries.map (_.row).max + 1, entries)

  def allToGrids (entries: Seq[Set[Entry]]): String =
    entries.map (solution => entriesToGrid (solution)).mkString ("\n\n")
}
