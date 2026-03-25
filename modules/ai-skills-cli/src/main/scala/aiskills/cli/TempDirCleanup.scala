package aiskills.cli

import scala.scalanative.libc.stdlib
import scala.scalanative.unsafe.*

object TempDirCleanup {

  val TempDirPrefix: String = ".aiskills-temp-"

  def isTempDir(dir: os.Path): Boolean =
    (dir / os.up) == os.home && dir.last.startsWith(TempDirPrefix)

  def safeRemoveAll(dir: os.Path): Unit =
    if isTempDir(dir) then os.remove.all(dir) else ()

  @annotation.nowarn("msg=null")
  private var currentTempDir: os.Path | Null = null // scalafix:ok DisableSyntax.var,DisableSyntax.null

  @annotation.nowarn("msg=null")
  def register(dir: os.Path): Unit =
    currentTempDir = dir

  @annotation.nowarn("msg=null")
  def unregister(): Unit =
    currentTempDir = null // scalafix:ok DisableSyntax.null

  @annotation.nowarn("msg=null")
  private val atexitRegistered: Boolean = {
    val cleanup: CFuncPtr0[Unit] = CFuncPtr0.fromScalaFunction[Unit] { () =>
      val dir = currentTempDir
      if dir != null then { // scalafix:ok DisableSyntax.null
        try safeRemoveAll(dir)
        catch { case _: Exception => () }
        currentTempDir = null // scalafix:ok DisableSyntax.null
      }
    }

    val _ = stdlib.atexit(cleanup)
    true
  }

  def ensureAtexitRegistered(): Unit = {
    val _ = atexitRegistered
  }
}
