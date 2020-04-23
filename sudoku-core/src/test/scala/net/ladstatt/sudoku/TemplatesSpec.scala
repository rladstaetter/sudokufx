package net.ladstatt.sudoku

import org.scalatest.wordspec.AnyWordSpecLike


/**
 * Created by lad on 01.11.14.
 */
class TemplatesSpec extends AnyWordSpecLike {

  "doWeLoadTemplatesCorrectly" in {
    assert(9L == TemplateLibrary.asSeq.size.toLong)
  }

}

