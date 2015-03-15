package net.ladstatt.apps.sudoku

import org.junit.Test
import org.junit.Assert._

/**
 * Created by lad on 01.11.14.
 */
class TemplatesTest {

  @Test def doWeLoadTemplatesCorrectly(): Unit = {
    assertEquals(9L, TemplateLibrary.asSeq.size.toLong)
  }

}

