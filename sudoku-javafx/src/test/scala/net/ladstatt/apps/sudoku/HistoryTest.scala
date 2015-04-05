package net.ladstatt.apps.sudoku

import net.ladstatt.apps.sudoku.Parameters._
import net.ladstatt.core.Utils
import org.junit.Assert._
import org.junit.Test

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}

/**
 * Created by lad on 27.04.14.
 */
class HistoryTest extends OpenCvUnitTest with Utils {





  @Test
  def invalidSCell() = {
    Try {
      SCell(-10, 0,null)
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
