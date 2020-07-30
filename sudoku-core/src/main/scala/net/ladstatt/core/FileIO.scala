package net.ladstatt.core

import java.io.FileOutputStream
import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import scala.util.Try


object FileIO extends CanLog {

  def createDirectories(path: Path): Path = {
    if (!Files.exists(path)) {
      Files.createDirectories(path)
      logTrace(s"Created directory " + path.toAbsolutePath)
    } else
      logTrace("Using directory " + path.toAbsolutePath)
    path
  }

  private def createParentOnDemand(p: Path): Path = {
    val path = p.toAbsolutePath

    def doCreateParentOnDemand(path: Path): Unit = {
      Option(path.getParent) match {
        case Some(parentPath) =>
          if (parentPath != path && !Files.exists(parentPath)) {
            Option(parentPath.getParent) match {
              case Some(subParentPath) =>
                if (subParentPath != path && !Files.exists(subParentPath)) {
                  doCreateParentOnDemand(subParentPath)
                }
              case None => // all done
            }
            createDirectories(parentPath)
            ()
          }
        case None => // all done
      }
    }

    doCreateParentOnDemand(path)
    path
  }

  def toPath(path: Path, content: ByteBuffer): Try[Unit] = {
    val logPrefix = if (Files.exists(path)) "Updated" else "Created"
    timeR(Try {

      createParentOnDemand(path)
      val fous = new FileOutputStream(path.toFile)
      try {
        val c = fous.getChannel
        c.write(content)
        c.force(true)
        fous.flush()
        fous.getFD.sync()
      } finally {
        fous.close()
      }
    }, s"$logPrefix ${path.toAbsolutePath.toString}")
  }
}
