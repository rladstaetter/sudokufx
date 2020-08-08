package net.ladstatt.sudoku

import java.nio.file.{Files, Path, Paths}

import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps

object ReplaySession {

  def listAll(path: Path): java.util.stream.Stream[ReplaySession] = {
    Files.list(path).map(p => ReplaySession(p, 0 millis)).sorted((o1: ReplaySession, o2: ReplaySession) => {
      o1.p.toAbsolutePath.toString.compareTo(o2.p.toAbsolutePath.toString)
    })
  }

  def apply(asString: String): ReplaySession = {
    ReplaySession(Paths.get(asString), 0 millis)
  }
}

case class ReplaySession(p: Path, sleep: FiniteDuration) {
  def session: SSession = {
    SSession(p.getFileName.toString, p, sleep)
  }

  def asString: String = p.toAbsolutePath.toString
}