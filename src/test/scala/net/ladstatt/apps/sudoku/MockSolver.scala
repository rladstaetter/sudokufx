package net.ladstatt.apps.sudoku

import net.ladstatt.core.HasDescription

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by lad on 26.10.14.
 */

object MockSolver extends HasDescription {
  val description = "mock test solver"

  def solve(s: String)(log: String => Unit): String = {
    """245981376
      |169273584
      |837564219
      |976125438
      |513498627
      |482736951
      |391657842
      |728349165
      |654812793""".stripMargin
  }

  def solve2(s: String)(log: String => Unit): Future[String] = Future {
    """000000000
      |040070300
      |059060001
      |006240000
      |000050400
      |008790000
      |034000008
      |000080906
      |000400000""".stripMargin
  }
}


