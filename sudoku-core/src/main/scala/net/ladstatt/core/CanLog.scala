package net.ladstatt.core

/**
 * Created by lad on 02.02.15.
 */
trait CanLog extends Timed {

  def logInfo(msg: String) = println("INFO: " + msg)

  def logWarn(msg: String) = System.err.println("WARN: " + msg)

  def logTrace(msg: String) = ()

  def logError(msg: String) = System.err.println("ERROR: " + msg)

  def logWithTimer[A](msg: String, f: => A): A = {
    time(f, t => logInfo(s"$msg (duration: $t ms)"))
  }


}
