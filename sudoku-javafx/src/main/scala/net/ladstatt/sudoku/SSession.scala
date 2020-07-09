package net.ladstatt.sudoku

import java.nio.file.{Files, Path}
import java.util.stream

import org.bytedeco.opencv.opencv_core.Mat
import rx.lang.scala.Subscriber

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps


object SSession {
  def apply(id: String, path: Path): SSession = {
    val postFixLength = ".png".length
    val files: stream.Stream[Path] =
      Files.list(path).sorted((p1: Path, p2: Path) => {
        p1.getFileName.toString.dropRight(postFixLength).toInt.compareTo(
          p2.getFileName.toString.dropRight(postFixLength).toInt)
      })// .limit(50)
    SSession(id, files, 0 millis)
  }
}

/**
 * reads images from a given directory, in the form <nr>.png
 *
 **/
case class SSession(id: String, files: stream.Stream[Path], sleep: FiniteDuration) {

  def subscribe(subscriber: Subscriber[Mat]): Unit = {
    files.forEach((p: Path) => {
      Thread.sleep(sleep.toMillis)
      subscriber.onNext(JavaCV.loadMat(p))
    })
    subscriber.onCompleted()
  }
}