case class Entry (row: Int, column: Int, filled: Boolean) {
  def opposite = Entry (row, column, !filled)
}
