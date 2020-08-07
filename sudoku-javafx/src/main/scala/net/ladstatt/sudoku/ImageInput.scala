package net.ladstatt.sudoku

abstract class ImageInput(val asString: String)

case object FromVideo extends ImageInput("Webcam")

case object FromFile extends ImageInput("File system")

object ImageInput {

  val seq = Seq[ImageInput](FromVideo, FromFile)

  def apply(asString: String): ImageInput = {
    seq.find(_.asString == asString).getOrElse(FromVideo)
  }

}