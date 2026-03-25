package aiskills.cli

import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*

object SigintHandler {

  /** Install a SIGINT handler that restores the terminal cursor before exiting.
    *
    * This must be called before any interactive prompt that hides the cursor (e.g. cue4s Prompts).
    * It should also be re-installed after any subprocess invocation (e.g. os.proc.call) which may
    * reset the signal disposition.
    */
  def install(): Unit = {
    import scala.scalanative.posix.signal
    import scala.scalanative.posix.unistd
    import scala.scalanative.libc.stdlib

    val handler: CFuncPtr1[CInt, Unit] =
      CFuncPtr1.fromScalaFunction[CInt, Unit] { (sig: CInt) =>
        // ESC[?25h = show cursor (6), \r = go to col 0 (1), ESC[J = erase from cursor to end of screen (3) = 10 bytes
        val cur = stackalloc[Byte](10)
        cur(0) = 0x1b.toByte // ESC
        cur(1) = '['.toByte
        cur(2) = '?'.toByte
        cur(3) = '2'.toByte
        cur(4) = '5'.toByte
        cur(5) = 'h'.toByte
        cur(6) = '\r'.toByte
        cur(7) = 0x1b.toByte // ESC
        cur(8) = '['.toByte
        cur(9) = 'J'.toByte
        val _   = unistd.write(1, cur, 10.toUSize)

        if sig == 2 then { // SIGINT
          // "\nCancelled by Ctrl+C\n" = 21 bytes
          val msg = stackalloc[Byte](21)
          msg(0) = '\n'.toByte
          msg(1) = 'C'.toByte
          msg(2) = 'a'.toByte
          msg(3) = 'n'.toByte
          msg(4) = 'c'.toByte
          msg(5) = 'e'.toByte
          msg(6) = 'l'.toByte
          msg(7) = 'l'.toByte
          msg(8) = 'e'.toByte
          msg(9) = 'd'.toByte
          msg(10) = ' '.toByte
          msg(11) = 'b'.toByte
          msg(12) = 'y'.toByte
          msg(13) = ' '.toByte
          msg(14) = 'C'.toByte
          msg(15) = 't'.toByte
          msg(16) = 'r'.toByte
          msg(17) = 'l'.toByte
          msg(18) = '+'.toByte
          msg(19) = 'C'.toByte
          msg(20) = '\n'.toByte
          val _   = unistd.write(1, msg, 21.toUSize)
        } else ()

        stdlib.exit(128 + sig)
      }
    val _                              = signal.signal(signal.SIGINT, handler)
  }
}
