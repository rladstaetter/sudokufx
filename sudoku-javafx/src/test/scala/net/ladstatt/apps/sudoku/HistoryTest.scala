package net.ladstatt.apps.sudoku

import net.ladstatt.core.Utils
import net.ladstatt.opencv.OpenCV
import org.junit.Assert._
import org.junit.Test

import scala.util.{Failure, Try}

/**
  * Created by lad on 27.04.14.
  */
class HistoryTest   {

  //OpenCV.loadNativeLib("../lib/libopencv_java310.so")

  @Test
  def invalidSCell() = {
    Try {
      SCell(-10, 0, null)
    } match {
      case Failure(e: AssertionError) =>
      case _ => fail("should throw an AssertionError")
    }
  }

  @Test
  def invalidSCell2() = {
    Try {
      SCell(0, -1, null)
    } match {
      case Failure(e: AssertionError) =>
      case _ => fail("should throw an AssertionError")
    }
  }


}
