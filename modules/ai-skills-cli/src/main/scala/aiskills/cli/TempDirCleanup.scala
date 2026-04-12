package aiskills.cli

import aiskills.core.given
import cats.syntax.all.*

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*

object TempDirCleanup {

  val TempDirPrefix: String = ".aiskills-temp-"

  def isTempDir(dir: os.Path): Boolean =
    (dir / os.up) === os.home && dir.last.startsWith(TempDirPrefix)

  def safeRemoveAll(dir: os.Path): Unit =
    if isTempDir(dir) then os.remove.all(dir) else ()

  private val dirs: scala.collection.mutable.Set[os.Path] = scala.collection.mutable.Set.empty

  def register(dir: os.Path): Unit = {
    val _ = dirs.add(dir)
  }

  def unregister(dir: os.Path): Unit = {
    val _ = dirs.remove(dir)
  }

  def clearAll(): Unit = {
    dirs.toList.foreach { d =>
      try safeRemoveAll(d)
      catch { case _: Exception => () }
    }
    dirs.clear()
  }

  private val atexitRegistered: Boolean = {
    val cleanup: CFuncPtr0[Unit] = CFuncPtr0.fromScalaFunction[Unit] { () =>
      dirs.toList.foreach { d =>
        try safeRemoveAll(d)
        catch { case _: Exception => () }
      }
      dirs.clear()
    }

    val _ = stdlib.atexit(cleanup)
    true
  }

  def ensureAtexitRegistered(): Unit = {
    val _ = atexitRegistered
  }
}
