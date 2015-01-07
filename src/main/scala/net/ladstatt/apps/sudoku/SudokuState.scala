package net.ladstatt.apps.sudoku

import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.core.Utils
import org.opencv.core._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object SudokuState {

  def duplicate(origHCounts: HitCounts): HitCounts = {
    for (cnts <- origHCounts) yield {
      for (x <- cnts) yield x
    }
  }

  def duplicate(origDigitQuality: Array[Double]): Array[Double] = origDigitQuality.clone()

  def duplicate(origDigitData: Array[Option[Mat]]): Array[Option[Mat]] = origDigitData.clone()

  def apply(orig: SudokuState): SudokuState = {
    SudokuState(hCounts = duplicate(orig.hCounts),
      digitQuality = duplicate(orig.digitQuality),
      digitData = duplicate(orig.digitData)
    )
  }
}

/**
 * @param hCounts the individual detection numbers
 *                for each of the 81 sudoku cells, there exists a list which depicts how often a certain number
 *                was found in the sudoku, where the index in the list is the number (from 0 to 9, with 0 being
 *                the "empty" cell)
 * @param digitQuality indicates the best hit for each number
 * @param digitData saves the picture information for the best hit
 */
case class SudokuState(hCounts: HitCounts = Array.fill(cellRange.size)(Array.fill[SCount](digitRange.size)(0)),
                       digitQuality: Array[Double] = Array.fill(digitRange.size)(Double.MaxValue),
                       digitData: Array[Option[Mat]] = Array.fill(digitRange.size)(None)) extends Utils {

  def statsAsString(): String =
    s"""$digitQualityAsString
       |${hitCountsAsString(hCounts)}
       |""".stripMargin

  def digitQualityAsString: String =
    s"""Quality:
      |--------
      |${digitQuality.map(q => q).mkString("\n")}
      |""".stripMargin

  def hitCountsAsString(hitCounts: HitCounts): String = {
    s"""Hitcounts:
      |----------
      |
      |${hitCounts.map(_.mkString(",")).mkString("\n")}
      |""".stripMargin
  }

  val qualityFilter: PartialFunction[SCell, Boolean] = {
    case c => (c.value != 0) && (c.quality < digitQuality(c.value)) // lower means "better"
  }


  def merge(detectedCells: Traversable[SCell]): SudokuState =  {
    val hits: Traversable[SCell] = detectedCells.filter(qualityFilter)
    val grouped: Map[Int, Traversable[SCell]] = hits.groupBy(f => f.value)
    val optimal: Map[Int, SCell] = grouped.map { case (i, cells) => i -> cells.maxBy(c => c.quality)}
    optimal.values.foreach(c => {
      digitData(c.value) = Some(c.data)
      digitQuality(c.value) = c.quality
    })
    this
  }

}






