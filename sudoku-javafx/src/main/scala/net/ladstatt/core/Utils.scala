package net.ladstatt.core

import java.io.{File, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.{Channels, ReadableByteChannel, WritableByteChannel}
import java.util.UUID

import scala.concurrent.{ExecutionContext, Future, Promise}

trait Utils extends Timed {


  def isNull[A, X](o: X)(isnull: => A, notnull: => X => A): A = {
    if (o == null) {
      isnull
    } else {
      notnull(o)
    }
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


}

