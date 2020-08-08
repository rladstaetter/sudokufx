package net.ladstatt.sudoku

import javafx.util.StringConverter

class ReplaySessionStringConverter extends StringConverter[ReplaySession] {
  override def toString(input: ReplaySession): String = input.asString

  override def fromString(asString: String): ReplaySession = ReplaySession(asString)
}
