package net.ladstatt.sudoku

import org.bytedeco.opencv.opencv_core.{Mat, Size}

object Parameters {

  val defaultDigitLibrary: DigitLibrary = DigitLibrary(Map())


  // least number of matches necessary to identify one number
  // if you have a good camera, take 1 to get fast response
  val cap = 3

  // number of different values a cell can have before the cell is label 'ambiguous'
  val ambiguitiesCount = 5

  // how many cells are allowed to have ambiguous information before number detection process is restarted
  val ambiCount = 5

  // numbers won't get any larger in the status matrix than this number
  val topCap = 15


  assert(topCap - cap > 0)


  val ssize = 9
  val cellCount: Int = ssize * ssize

  val range: Seq[Int] = 0 until ssize
  val digitRange: Seq[Int] = 0 to ssize

  val cellRange: Range = 0 until cellCount

  val colorRange: Seq[Int] = 0 to 256 by 16

  private val leftRange: Seq[Int] = Seq(0, 1, 2)
  private val middleRange: Seq[Int] = Seq(3, 4, 5)
  private val rightRange: Seq[Int] = Seq(6, 7, 8)
  val sectors: Seq[Seq[Int]] = Seq(leftRange, middleRange, rightRange)

  /** size of internal representation of sudoku */
  val size1280x720 = new Size(1280, 720)

  /** has to be a def since we get trouble with memory allocation / corruption with JavaCV */
  def normalizedCorners: Mat = JavaCV.mkCorners(size1280x720.width, size1280x720.height)

  def row(i: Int): Int = i / 9

  def col(i: Int): Int = i % 9

}
