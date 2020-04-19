package net.ladstatt.sudoku

import org.scalatest.WordSpecLike


/**
 * Created by lad on 01.11.14.
 */
class TemplatesSpec extends WordSpecLike {

  "doWeLoadTemplatesCorrectly" in {
    assert(9L == TemplateLibrary.asSeq.size.toLong)
  }

}

