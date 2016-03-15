package net.ladstatt.core

/**
 * Created by lad on 02.02.15.
 */
trait CanLog  {

  def logInfo(msg: String) = println("INFO: " + msg)

  def logWarn(msg: String) = System.err.println("WARN: " + msg)

  def logError(msg: String) = System.err.println("ERROR: " + msg)

  def logTrace(msg: String) = println("TRACE:" + msg)

  def logWithTimer[A](msg: String, f: => A): A = {
    Utils.time(f, t => logInfo(s"$msg (duration: $t ms)"))
  }


}
