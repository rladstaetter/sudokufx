package net.ladstatt.sudoku

import org.junit.Assert._
import org.junit.Test

/**
 * Created by lad on 01.11.14.
 */
class TemplatesTest {

  @Test def doWeLoadTemplatesCorrectly(): Unit = {
    assertEquals(9L, TemplateLibrary.asSeq.size.toLong)
  }

}

