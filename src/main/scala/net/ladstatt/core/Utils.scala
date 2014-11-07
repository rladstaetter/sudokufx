package net.ladstatt.core

import java.io.{File, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.{Channels, ReadableByteChannel, WritableByteChannel}
import java.util.UUID

import scala.concurrent.{ExecutionContext, Future, Promise}

trait HasDescription {
  def description: String
}

trait CanLog extends Utils {

  def logInfo(msg: String) = println("INFO: " + msg)

  def logWarn(msg: String) = println("WARN: " + msg)

  def logTrace(msg: String) = ()

  def logError(msg: String) = System.err.println(msg)

  def logWithTimer[A](msg: String, f: => A): A = {
    time(f, t => logInfo(s"$msg (duration: $t ms)"))
  }


}

object SystemEnv {

  val isX64 = System.getProperty("os.arch").contains("64")

  val runOnMac = {
    System.getProperty("os.name").toLowerCase match {
      case "mac os x" => true
      case _ => false
    }
  }

}

trait Utils {

  def traverseWithIndex[A](cs: Seq[A])(up: (A, Int) => Unit): Unit = {
    var i = 0
    for (c <- cs) {
      up(c, i)
      i = i + 1
    }
  }

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(f)
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  def execFuture[A](f: => A)(implicit ec: ExecutionContext): Future[A] = {
    val p = Promise[A]()
    p.completeWith(Future(f))
    p.future
  }

  def option[A, X](o: Option[A])(none: => X, some: => A => X): X = {
    o match {
      case None => none
      case Some(a) => some(a)
    }
  }

  def isNull[A, X](o: X)(isnull: => A, notnull: => X => A): A = {
    if (o == null) {
      isnull
    } else {
      notnull(o)
    }
  }

  /**
   * function to measure execution time of first function, optionally executing a display function,
   * returning the time in ms
   */
  def time[A](a: => A, display: Long => Unit = s => ()): A = {
    val before = System.nanoTime
    val result = a
    val millis = (System.nanoTime - before) / 1000000
    display(millis)
    result
  }


  /**
   * see
   * http://code.google.com/p/time-shift/source/browse/algorithm/trunk/src/main/java/net/brams/timeshift/algorithm/algos/FastChannelCopy.java?spec=svn50&r=50
   */
  def fastChannelCopy(src: ReadableByteChannel, dest: WritableByteChannel) = {
    val buffer = ByteBuffer.allocateDirect(16 * 1024)
    while (src.read(buffer) != -1) {
      // prepare the buffer to be drained
      buffer.flip()
      // write to the channel, may block
      dest.write(buffer)
      // If partial transfer, shift remainder down
      // If buffer is empty, same as doing clear()
      buffer.compact()
    }
    // EOF will leave buffer in fill state
    buffer.flip()
    // make sure the buffer is fully drained.
    while (buffer.hasRemaining()) {
      dest.write(buffer)
    }
  }

  // allocate the stream ... only for example
  def copyStream(is: InputStream, os: OutputStream) = {
    // get an channel from the stream
    val inputChannel = Channels.newChannel(is)
    val outputChannel = Channels.newChannel(os)
    // copy the channels
    fastChannelCopy(inputChannel, outputChannel)
    // closing the channels
    outputChannel.close()
    os.close()
    inputChannel.close()
    is.close()
  }

  /**
   * creates a temporary file handle which will be deleted after program termination
   *
   * @return
   */
  def mkTempFile(): File = {
    val f = File.createTempFile(UUID.randomUUID.toString, null)
    f.deleteOnExit
    f
  }

  def mkTempDirectory(): File = {
    val temp = mkTempFile // File.createTempFile("temp", System.nanoTime().toString)

    if (!temp.delete()) {
      sys.error("Could not delete temp file: " + temp.getAbsolutePath())
    }

    if (!temp.mkdir()) {
      sys.error("Could not create temp directory: " + temp.getAbsolutePath())
    }
    temp
  }

}

