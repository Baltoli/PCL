package uk.ac.cam.bsc28.diss.App

import java.io._
import java.nio.file.{Paths, Files}

object Filesystem {

  trait Error extends Exception
  case class FileReadError(path: String) extends Filesystem.Error {
    override def toString = path
  }

  type Result[T] = Either[T, Filesystem.Error]

  def textFromTargetFileName(name: String): Result[String] = {
    val file = new File(name)

    val path = if (file.isAbsolute) {
      new File(name).getCanonicalPath
    } else {
      new File(System.getProperty("user.dir") + "/" + name)
    }

    try {
      val text = Files.readAllBytes(Paths.get(file.getCanonicalPath))
      Left(new String(text))
    } catch {
      case e: Exception => Right(FileReadError(e.getMessage)) // TODO: better errors - more type
    }
  }

}
