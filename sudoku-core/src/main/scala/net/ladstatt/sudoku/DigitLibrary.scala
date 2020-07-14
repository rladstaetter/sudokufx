package net.ladstatt.sudoku

import net.ladstatt.sudoku.JavaCV.copyMat
import org.bytedeco.opencv.opencv_core.Mat

case class DigitLibrary(digits: Map[Int, DigitEntry]) {

  /**
   * if digit libary contains a mapping of detected value, return true if cell has a lower value for quality
   * than given mapping, otherwise return true.
   *
   * @param c
   * @return
   */
  def hasCellBetterQuality(c: SCell): Boolean = {
    if (contains(c.detectedValue)) {
      (c.quality < digits(c.detectedValue).quality) // lower means "better"
    } else true
  }

  def contains(value: Int): Boolean = {
    digits.contains(value)
  }

  def add(normalized: Mat,
          detectedCells: Seq[SCell]): DigitLibrary = {

    /**
     * The filter returns only cells which contain 'better match' cells.
     */
    val qualityFilter: PartialFunction[SCell, Boolean] = {
      case c => hasCellBetterQuality(c)
    }

    val hits: Seq[SCell] = detectedCells.filter(qualityFilter)
    val grouped: Map[Int, Seq[SCell]] = hits.groupBy(c => c.detectedValue)
    val optimal: Map[Int, SCell] = grouped.map { case (i, cells) => i -> cells.maxBy(c => c.quality)(Ordering.Double.TotalOrdering) }

    val updatedLibraryEntries =
      (for (c <- optimal.values if hasCellBetterQuality(c)) yield {
        assert(c.detectedValue != 0)
        val newData = Some(copyMat(normalized.apply(c.roi)))
        c.detectedValue -> DigitEntry(c.detectedValue, System.currentTimeMillis(), c.quality, newData)
      }).toMap
    println("size:" + updatedLibraryEntries.size)

    DigitLibrary(digits ++ updatedLibraryEntries)
  }
}
