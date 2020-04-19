package net.ladstatt.core

import scala.concurrent.duration._
import scala.language.postfixOps

/**
 * Created by lad on 02.02.15.
 */
trait CanLog  {

  def logInfo(msg: String): Unit = println("INFO: " + msg)

  def logWarn(msg: String): Unit = System.err.println("WARN: " + msg)

  def logError(msg: String): Unit = System.err.println("ERROR: " + msg)

  def logTrace(msg: String): Unit = println("TRACE:" + msg)

  def logException(e : Throwable) : Unit = e.printStackTrace()

  def logWithTimer[A](msg: String, f: => A): A = {
    Utils.time(f, t => logInfo(s"$msg (duration: $t ms)"))
  }

  def timeR[A](a: => A, msg: String, errorThreshold: FiniteDuration = 200 millis): A = {
    Utils.time(a, t => {
      if (t <= errorThreshold.toMillis) {
        logTrace(s"$msg (duration: $t ms)")
      } else {
        logTrace(s"$msg (duration: $t ms [exceeded threshold of ${errorThreshold.toMillis}])")
      }
    })
  }


}
