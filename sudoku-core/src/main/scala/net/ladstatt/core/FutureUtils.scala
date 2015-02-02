package net.ladstatt.core

import scala.concurrent.{Promise, Future, ExecutionContext}

/**
 * Created by lad on 02.02.15.
 */
object FutureUtils {
// TODO remove
  def execFuture[A](f: => A)(implicit ec: ExecutionContext): Future[A] = {
    val p = Promise[A]()
    p.completeWith(Future(f))
    p.future
  }
}
