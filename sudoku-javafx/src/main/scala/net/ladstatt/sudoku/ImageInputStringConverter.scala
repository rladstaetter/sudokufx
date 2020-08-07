package net.ladstatt.sudoku

import javafx.util.StringConverter

class ImageInputStringConverter extends StringConverter[ImageInput] {
  override def toString(input: ImageInput): String = input.asString

  override def fromString(asString: String): ImageInput = ImageInput(asString)
}
