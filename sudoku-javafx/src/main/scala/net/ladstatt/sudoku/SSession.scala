package net.ladstatt.sudoku

import java.nio.file.{Files, Path}
import java.util.stream

import org.bytedeco.opencv.opencv_core.Mat
import rx.lang.scala.Subscriber

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps


object SSession {
  def apply(id: String, path: Path): SSession = {
    val framePrefix = "frame-"
    val prefixLength = framePrefix.length
    val postFixLength = ".png".length
    val files: stream.Stream[Path] =
      Files.list(path).filter((t: Path) => {
        t.getName(t.getNameCount - 1).toString.startsWith(framePrefix)
      }).sorted((p1: Path, p2: Path) => {
        p1.getFileName.toString.dropRight(postFixLength).substring(prefixLength).toInt.compareTo(
          p2.getFileName.toString.dropRight(postFixLength).substring(prefixLength).toInt)
      }) // .limit(50)
    SSession(id, files, 100 millis)
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
    subscriber.onCompleted() // after looping through all images, exit
  }
}