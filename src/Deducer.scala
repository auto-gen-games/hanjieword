import scala.annotation.tailrec
import scala.collection.immutable.IndexedSeq

object Deducer {
  /** Return all entries in a row/column, with given width and lengths, that are certain to be true given a set of
    * entries already known/assumed to be true on that row/column, or None if the assumed values mean no possibility is
    * valid. */
  def certainties (width: Int, allLengths: Seq[Int], known: Set[(Int, Boolean)]): Option[Set[(Int, Boolean)]] = {
    /** Returns the minimum number of cells before the length at the given index, not accounting for the known entries. */
    def minimumPreceding (lengthIndex: Int): Int =
      if (lengthIndex == 0) 0 else allLengths.take (lengthIndex).sum + lengthIndex

    /** Returns the minimum number of cells after the length at the given index, not accounting for the known entries. */
    def minimumSucceeding (lengthIndex: Int): Int =
      if (lengthIndex == allLengths.size - 1) 0
      else
        allLengths.takeRight (allLengths.size - 1 - lengthIndex).sum + allLengths.size - 1 - lengthIndex

    def minimumEnd (lengthIndex: Int): Int =
      minimumPreceding (lengthIndex) + allLengths (lengthIndex) - 1

    def maximumPreceding (lengthIndex: Int): Int =
      width - minimumSucceeding (lengthIndex) - allLengths (lengthIndex)

    def maximumSucceeding (lengthIndex: Int): Int =
      width - minimumPreceding (lengthIndex) - allLengths (lengthIndex)

    /** Return the earliest/latest position, if any, from the given start position for which there are no gaps preventing the
      * given length starting/ending in that position and no filled cell immediately adjacent.
      *
      * @param direction 1 to look ahead from the start position to find the earliest valid position, or -1 to look back
      *                  from the start position to find the latest valid position
      * @param limit     The final position that the stretch can occupy.
      */
    def limitOfStretch (length: Int, start: Int, direction: Int, limit: Int): Option[Int] = {
      /** Same as overall function (with remainingLength replacing length, from replacing start) except that if
        * a stretch of remainingLength cannot be fit from here, try from the next position with the full length.  */
      @tailrec
      def limitOfRemainingStretch (remainingLength: Int, from: Int): Option[Int] = {
        /* The position of the last cell of the stretch if starting from position from */
        val endPoint = from + direction * (remainingLength - 1)
        /* If endPoint would be beyond the final position the length could occupy, no valid position has been found */
        if ((direction == -1 && endPoint < limit) || (direction == 1 && endPoint > limit)) None
        else remainingLength match {
          /* If remainingLength is zero, we've allocated the full length, so from is the cell beyond the end point:
           * return the start position */
          case 0 => Some (from - direction * length)
          case _ =>
            /* If the cell at from is known to be empty or the cell immediately following endPoint must be filled,
             * we cannot allocate remainingLength here, so start trying to allocate the full length from the next position.
             * Otherwise, try to allocate remainingLength-1 from the next position. */
            limitOfRemainingStretch (
              if (known.contains ((from, false)) || known.contains ((endPoint + direction, true))) length else remainingLength - 1,
              from + direction)
        }
      }

      limitOfRemainingStretch (length, start)
    }

    /** Return the earliest position, if any, that the length at the given index can start, accounting for existing
      * cells in judging possible space and given the already known bounds per length index. */
    def earliestStart (lengthIndex: Int, within: Seq[(Int, Int)]): Option[Int] =
      if (lengthIndex == 0)
        limitOfStretch (allLengths (0), within (0)._1, 1, within (0)._2)
      else
        earliestStart (lengthIndex - 1, within).flatMap { prior =>
          val from = within (lengthIndex)._1.min (prior + allLengths (lengthIndex - 1) + 1)
          limitOfStretch (allLengths (lengthIndex), from, 1, within (lengthIndex)._2)
        }

    /** Return the latest position, if any, that the length at the given index can end, accounting for existing
      * cells in judging possible space and given the already known bounds per length index. */
    def latestEnd (lengthIndex: Int, within: Seq[(Int, Int)]): Option[Int] =
      if (lengthIndex == allLengths.size - 1)
        limitOfStretch (allLengths (lengthIndex), within (lengthIndex)._2, -1, within (lengthIndex)._1)
      else
        latestEnd (lengthIndex + 1, within).flatMap { next =>
          val from = within (lengthIndex)._2.min (next - allLengths (lengthIndex + 1) - 1)
          limitOfStretch (allLengths (lengthIndex), from, -1, within (lengthIndex)._1)
        }

    def earliestEnd (lengthIndex: Int, within: Seq[(Int, Int)]): Option[Int] =
      earliestStart (lengthIndex, within).map (_ + allLengths (lengthIndex) - 1)

    def latestStart (lengthIndex: Int, within: Seq[(Int, Int)]): Option[Int] =
      latestEnd (lengthIndex, within).map (_ - allLengths (lengthIndex) + 1)

    /** Returns the number of filled cells possible in a given direction from a given cell, excluding the cell itself. */
    def possibleLengthFrom (start: Int, direction: Int): Int = {
      @tailrec
      def possibleContinuationFrom (from: Int, alreadyFound: Int): Int =
        if (from + direction < 0 || from + direction >= width || known.contains ((from + direction, false))) alreadyFound
        else possibleContinuationFrom (from + direction, alreadyFound + 1)

      possibleContinuationFrom (start, 0)
    }

    /** Determines whether it is possible for the length at the given index can possibly overlap the given cell position. */
    def possibleCovered (lengthIndex: Int, position: Int, within: Seq[(Int, Int)]): IsPossible =
      if (lengthIndex >= allLengths.size) NoTooEarly else
      if (minimumPreceding (lengthIndex) > position) NoTooEarly
      else if (minimumSucceeding (lengthIndex) > width - position) NoTooLate
      else (earliestStart (lengthIndex, within), latestEnd (lengthIndex, within)) match {
        case (Some (start), Some (end)) =>
          if (position < start) NoTooEarly
          else if (position > end) NoTooLate
          else if (end - start + 1 < allLengths (lengthIndex)) NoInvalid
          else if (possibleLengthFrom (position, 1) + possibleLengthFrom (position, -1) + 1 >= allLengths (lengthIndex))
            YesPossible
          else NoTooLittleSpace
        case _ => NoInvalid
      }

    /** Determines whether it is possible for the given cell position to be in the gap immediately preceding the length
      * at the given index. If the index is beyond the size of the lengths list, determine whether it is possible for
      * the given cell to be after all lengths. */
    def possibleBefore (lengthIndex: Int, position: Int, within: Seq[(Int, Int)]): IsPossible =
      if (lengthIndex >= allLengths.size)
        if (position < width - maximumSucceeding (allLengths.size - 1)) NoTooEarly
        else earliestEnd (allLengths.size - 1, within) match {
          case Some (end) => if (position <= end) NoTooEarly else YesPossible
          case _ => NoInvalid
        }
      else if (maximumPreceding (lengthIndex) <= position) NoTooLate
      else if (lengthIndex > 0 && minimumEnd (lengthIndex - 1) >= position) NoTooEarly
      else (latestStart (lengthIndex, within), if (lengthIndex == 0) Some (-1) else earliestEnd (lengthIndex - 1, within)) match {
        case (Some (start), Some (priorEnd)) =>
          if (position >= start) NoTooLate
          else if (position <= priorEnd) NoTooEarly
          else YesPossible
        case _ => NoInvalid
      }

    def possibleWithin (position: Int, within: (Int, Int)): IsPossible =
      if (position >= within._1 && position <= within._2) YesPossible else NoNotForThis

    /** Is the given position possibly the given value? Returns YesPossible (can be the value), YesCertain (must be the value),
      * NoNotForThis (cannot be the value) or NoInvalid (the assumed values make fitting the lengths impossible). */
    def possibleValue (position: Int, value: Boolean, within: Seq[(Int, Int)]): IsPossible = {
      def possibleFrom (lengthIndex: Int): IsPossible =
        (if (value) possibleWithin (position, within (lengthIndex)) else possibleBefore (lengthIndex, position, within)) match {
        case YesPossible => YesPossible
        case YesCertain => YesCertain
        case NoTooEarly => NoNotForThis
        case NoInvalid => NoInvalid
        case _ =>
          if ((value && lengthIndex < allLengths.size - 1) || (!value && lengthIndex < allLengths.size))
            possibleFrom (lengthIndex + 1)
          else
            NoNotForThis
      }

      if (allLengths.isEmpty) if (value) NoNotForThis else YesCertain
      else if (known.contains (position, value)) YesCertain
      else if (known.contains (position, !value)) NoNotForThis
      else possibleFrom (0)
    }

    def certaintiesFrom (position: Int, within: Seq[(Int, Int)]): Option[Set[(Int, Boolean)]] =
      if (position >= width) Some (Set[(Int, Boolean)] ())
      else {
        val addition = possibleValue (position, true, within) match {
          case YesCertain => Some (Set ((position, true)))
          case YesPossible => possibleValue (position, false, within) match {
            case YesCertain => Some (Set ((position, false)))
            case YesPossible => Some (Set[(Int, Boolean)] ())
            case NoNotForThis => Some (Set ((position, true)))
            case _ => None
          }
          case NoNotForThis => possibleValue (position, false, within) match {
            case YesCertain => Some (Set ((position, false)))
            case YesPossible => Some (Set ((position, false)))
            case a @ _ => None
          }
          case _ => None
        }
        if (!addition.isDefined) None
        else certaintiesFrom (position + 1, within) match {
          case None => None
          case Some (more) => addition.map (_ ++ more)
        }
      }

    val knownFilled: Set[Int] = known.filter (_._2).map (_._1)

    def lengthLimits (initialLimits: Seq[(Int, Int)]): Seq[(Int, Int)] = {
      // For each position known to be filled, the length indices which could cover that position
      val possibleLengths: Set[(Int, Seq[Int])] =
        knownFilled.map (position => (position, allLengths.indices.filter { lengthIndex =>
          possibleCovered (lengthIndex, position, initialLimits) == YesPossible
        }))
      // For each position known to be filled for which only one length could cover, map of position to length index
      val certainLengths: Set[(Int, Int)] = possibleLengths.filter (_._2.size == 1).map (c => (c._1, c._2.head))
      // For each length, the known filled positions for which this length must be the covering one
      val certainPositions: Seq[Set[Int]] =
        allLengths.indices.map { lengthIndex => certainLengths.filter (_._2 == lengthIndex).map (_._1) }
      // The new starting and ending limits on each length given the constraints of known covered positions
      val newLimits = allLengths.indices.map { lengthIndex =>
        if (certainPositions (lengthIndex).isEmpty)
          initialLimits (lengthIndex)
        else
          (initialLimits (lengthIndex)._1.max (certainPositions (lengthIndex).max - allLengths (lengthIndex) + 1),
            initialLimits (lengthIndex)._2.min (certainPositions (lengthIndex).min + allLengths (lengthIndex) - 1))
      }
      if (newLimits == initialLimits) newLimits else lengthLimits (newLimits)
    }

    val limits = lengthLimits (allLengths.indices.map (index => (minimumPreceding (index), width - 1 - minimumSucceeding (index))))

    if (limits.exists (limit => limit._2 < limit._1))
      None
    else
      certaintiesFrom (0, limits)
  }

  type Possibility = Seq[Entry]

  def entriesToRow (entries: Set[Entry], isRow: Boolean, index: Int): Set[(Int, Boolean)] =
    entries.flatMap { entry =>
      if (isRow) if (entry.row == index) Some ((entry.column, entry.filled)) else None
      else if (entry.column == index) Some ((entry.row, entry.filled)) else None
    }

  def validLine (cells: Set[(Int, Boolean)], lengths: Seq[Int], width: Int): Boolean = {
    def validLineFrom (remainingLengths: Seq[Int], start: Int, continuing: Boolean): Boolean =
      if (start >= width)
        remainingLengths.isEmpty || (remainingLengths.size == 1 && remainingLengths.head == 0)
      else if (remainingLengths.isEmpty)
        cells.contains ((start, false)) && validLineFrom (remainingLengths, start + 1, false)
      else if (continuing && remainingLengths.head == 0)
        cells.contains ((start, false)) && validLineFrom (remainingLengths.tail, start + 1, false)
      else if (continuing && !cells.contains ((start, true)))
        false
      else if (cells.contains ((start, false)))
        validLineFrom (remainingLengths, start + 1, false)
      else
        validLineFrom ((remainingLengths.head - 1) +: remainingLengths.tail, start + 1, true)

    validLineFrom (lengths, 0, false)
  }

  def solve (rowLengths: Seq[Seq[Int]], columnLengths: Seq[Seq[Int]]): Seq[Set[Entry]] = {
    val width = columnLengths.size
    val height = rowLengths.size
    /** All coordinates in the grid */
    val points = Seq.tabulate (height, width)((_, _)).flatten
    /** All row indices coupled with isRow=true then all column indices coupled with isRow=false */
    val lines = (0 until height).map ((_ , true)) ++ (0 until width).map ((_, false))

    def size (isRow: Boolean) = if (isRow) width else height
    def lengths (isRow: Boolean, index: Int) = if (isRow) rowLengths (index) else columnLengths (index)
    def toEntry (isRow: Boolean, index: Int, posFilled: (Int, Boolean)) =
      if (isRow) Entry (index, posFilled._1, posFilled._2) else Entry (posFilled._1, index, posFilled._2)

    def validSolution (known: Set[Entry]): Boolean =
      (0 until height).forall (row => validLine (entriesToRow (known, true, row), rowLengths (row), width)) &&
        (0 until width).forall (col => validLine (entriesToRow (known, false, col), columnLengths (col), height))

    /** Find all solutions given a set of assumed
      * grid entries, and include a set of pre-found solutions in the results. */
    @tailrec def search (assumptions: Seq[Set[Entry]], found: Seq[Set[Entry]]): Seq[Set[Entry]] = {
      @tailrec def deduce (previouslyKnown: Set[Entry]): Option[Set[Entry]] = {
        // For each row then column, the entries certain from current assumptions, or None where no combination possible
        val knownIfValid: Seq[Option[Set[Entry]]] = lines.par.map { case (line, isRow) =>
          certainties (size (isRow), lengths (isRow, line), entriesToRow (previouslyKnown, isRow, line)).
            map (_.map (toEntry (isRow, line, _)))
        }.seq
        if (knownIfValid.forall (_.isDefined)) {
          val known = knownIfValid.flatten.flatten.toSet
          if (known.size == width * height || known.size <= previouslyKnown.size)
            Some (known)
          else
            deduce (known)
        } else None
      }

      val solutions = assumptions.flatMap { assumption =>
        deduce (assumption).map { solution =>
          (assumption, solution)
        }
      }
      val isComplete = solutions.partition (_._2.size == width * height)
      if (isComplete._2.isEmpty)
        found ++ isComplete._1.map (_._2)
      else {
        val nextTries = isComplete._2.flatMap { partial =>
          points.find (point => !partial._2.exists (entry => entry.row == point._1 && entry.column == point._2)).map { unknown =>
            Seq (partial._1 ++ partial._2 + Entry (unknown._1, unknown._2, true),
              partial._1 ++ partial._2 + Entry (unknown._1, unknown._2, false))
          }
        }.flatten
        search (nextTries, found ++ isComplete._1.map (_._2))
      }
    }

    search (Seq (Set[Entry] ()), Nil).filter (validSolution)
  }
}
