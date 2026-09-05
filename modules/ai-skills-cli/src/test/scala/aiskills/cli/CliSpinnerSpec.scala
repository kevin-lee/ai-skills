package aiskills.cli

import cats.Id
import cats.syntax.all.*
import effectie.instances.id.fx.idFx
import hedgehog.*
import hedgehog.runner.*
import just.spinner.*

import scala.concurrent.duration.FiniteDuration

object CliSpinnerSpec extends Properties {
  override def tests: List[Test] = List(
    example("wide terminal preserves completed lines across sequential spinners", preserved(160, none[Int])),
    example("actual width overrides stale environment width", preserved(160, 80.some)),
    example("narrow terminal clears only wrapped spinner rows", preserved(60, none[Int])),
    example("old fallback erases completed lines in a wide terminal", oldFallback),
    example("unknown initial width uses stable plain output", staticOutput(none[Int], true, true)),
    example("zero initial width uses stable plain output", staticOutput(0.some, true, true)),
    example("redirected output stays plain even when explicitly enabled", staticOutput(160.some, false, true)),
    example("explicitly disabled animation stays plain", staticOutput(160.some, true, false)),
    example("failed later measurements retain the last valid width", retainedWidth),
  )

  private val previous = List("Updated: first", "Updated: second", "Updated: third")
  private val escape   = "\u001b["
  private val tokens   = "\u001b\\[[0-9;?]*[A-Za-z]|[^\u001b]".r

  final private case class Screen(lines: Vector[String], row: Int, column: Int)

  private object Screen {
    def render(output: String, width: Int): Vector[String] =
      tokens
        .findAllIn(output)
        .foldLeft(Screen(Vector(""), 0, 0)) { (screen, token) =>
          def move(row: Int, column: Int): Screen =
            Screen(screen.lines.padTo(row + 1, ""), row, column)
          if (token.startsWith(escape)) {
            val amount = token.drop(2).dropRight(1).toIntOption.getOrElse(1)
            token.lastOption match {
              case Some('G') => move(screen.row, math.max(0, amount - 1))
              case Some('A') => move(math.max(0, screen.row - amount), screen.column)
              case Some('K') => screen.copy(lines = screen.lines.updated(screen.row, ""))
              case _ => screen
            }
          } else if (token === "\r") {
            move(screen.row, 0)
          } else if (token === "\n") {
            move(screen.row + 1, 0)
          } else {
            val positioned = if (screen.column >= width) move(screen.row + 1, 0) else screen
            val line       = positioned.lines.lift(positioned.row).getOrElse("").padTo(positioned.column, ' ')
            positioned.copy(
              lines = positioned
                .lines
                .updated(positioned.row, line.take(positioned.column) + token + line.drop(positioned.column + 1)),
              column = positioned.column + 1,
            )
          }
        }
        .lines
  }

  final private class Output(tty: Boolean, reportedColumns: Option[Int]) extends TerminalOutput[Id] {
    private val content                      = SpinnerRef.atomicRef[Id, String]("")
    override def write(s: String): Unit      = content.update(_ + s)
    override def isTTY: Boolean              = tty
    override def columns: Option[Int]        = reportedColumns
    override def rows: Option[Int]           = 50.some
    def raw: String                          = content.get
    def rendered(width: Int): Vector[String] = Screen.render(raw, width)
  }

  final private class Timer extends SpinnerTimer[Id] {
    private val scheduled = SpinnerRef.atomicRef[Id, Option[() => Unit]](none[() => Unit])
    override def scheduleAtFixedRate(interval: FiniteDuration)(task: => Unit): SpinnerTimer.CancelToken[Id] = {
      scheduled.set((() => task).some)
      SpinnerTimer.CancelToken[Id](() => scheduled.set(none[() => Unit]))
    }
    def tick(): Unit    = scheduled.get.foreach(task => task())
    def active: Boolean = scheduled.get.nonEmpty
  }

  private def config(length: Int): SpinnerConfig =
    SpinnerConfig.default.withText("x" * (length - 2)).withNoColor.withEnabled(true)

  private def preserved(width: Int, reported: Option[Int]): Result = {
    val output     = new Output(true, reported)
    val timer      = new Timer
    previous.foreach(line => output.write(line + "\n"))
    List(86, 90).foreach { length =>
      val spinner = CliSpinner.create(config(length), output, timer, () => width.some)
      val _       = spinner.start()
      (1 to 3).foreach(_ => timer.tick())
      val _       = spinner.succeed(s"Cloned: $length".some)
      output.write(s"Updated: after $length\n")
    }
    val beforeTick = output.raw
    timer.tick()
    val lines      = output.rendered(width)
    Result.all(
      List(
        Result.assert(previous.forall(lines.contains)),
        Result.assert(
          List(86, 90).forall(n => lines.exists(_.contains(s"Cloned: $n")) && lines.contains(s"Updated: after $n"))
        ),
        Result.assert(output.raw.contains(AnsiCode.moveUp(1)) === (width < 90)),
        Result.assert(!timer.active),
        output.raw ==== beforeTick,
      )
    )
  }

  private def oldFallback: Result = {
    val output  = new Output(true, none[Int])
    val timer   = new Timer
    previous.foreach(line => output.write(line + "\n"))
    val spinner = Spinner.create[Id](config(90), output, timer, SpinnerRefMaker.atomicRef[Id])
    val _       = spinner.start()
    (1 to 3).foreach(_ => timer.tick())
    val _       = spinner.succeed("Cloned".some)
    Result.all(
      List(
        Result.assert(output.raw.contains(AnsiCode.moveUp(1))),
        Result.assert(!previous.forall(output.rendered(160).contains)),
        Result.assert(!timer.active),
      )
    )
  }

  private def staticOutput(measured: Option[Int], tty: Boolean, enabled: Boolean): Result = {
    val output  = new Output(tty, 80.some)
    val timer   = new Timer
    previous.foreach(line => output.write(line + "\n"))
    val spinner = CliSpinner.create(config(90).withEnabled(enabled), output, timer, () => measured)
    val _       = spinner.start()
    timer.tick()
    val _       = spinner.succeed("Cloned".some)
    val lines   = output.rendered(160)
    Result.all(
      List(
        Result.assert(previous.forall(lines.contains)),
        Result.assert(lines.contains("- " + "x" * 88)),
        Result.assert(lines.exists(_.contains("Cloned"))),
        Result.assert(!output.raw.contains(AnsiCode.cursorTo(0))),
        Result.assert(!output.raw.contains(AnsiCode.moveUp(1))),
        Result.assert(!timer.active),
      )
    )
  }

  private def retainedWidth: Result = {
    val measured = SpinnerRef.atomicRef[Id, Option[Int]](160.some)
    val output   = new Output(true, 80.some)
    val timer    = new Timer
    previous.foreach(line => output.write(line + "\n"))
    val spinner  = CliSpinner.create(config(90), output, timer, () => measured.get)
    val _        = spinner.start()
    measured.set(none[Int])
    (1 to 3).foreach(_ => timer.tick())
    val _        = spinner.succeed("Cloned".some)
    Result.all(
      List(
        Result.assert(previous.forall(output.rendered(160).contains)),
        Result.assert(!output.raw.contains(AnsiCode.moveUp(1))),
        Result.assert(!timer.active),
      )
    )
  }
}
