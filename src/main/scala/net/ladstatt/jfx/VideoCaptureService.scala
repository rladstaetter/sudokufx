package net.ladstatt.jfx

import javafx.concurrent.Service
import org.opencv.core.Mat
import org.opencv.highgui.VideoCapture
import scala.util.Try
import net.ladstatt.core.Utils


/**
 * a simple service implementation which reads images from a webcam for example.
 *
 * Restart if you want more than one image.
 */
class VideoCaptureService(logfn : String => Unit) extends Service[Try[Mat]] with JfxUtils with Utils {

  private val videoCapture: VideoCapture = time(new VideoCapture(0), t => logfn(s"Camera initialisation: $t ms"))

  def createTask = mkTask(
    Try {
      val image = new Mat()
      videoCapture.read(image)
      nullable(image)(throw new RuntimeException("videoCapture.read(...) returned null"), image => image)
    })

}

