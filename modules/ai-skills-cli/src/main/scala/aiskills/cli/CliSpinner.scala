package aiskills.cli

import aiskills.core.utils.TerminalWidth
import cats.Id
import effectie.instances.id.fx.idFx
import just.spinner.*

object CliSpinner {

  def createDefaultSideEffect(config: SpinnerConfig): SpinnerNoFx =
    create(config, TerminalOutput.stderr[Id], SpinnerTimer.create, () => TerminalWidth.getStderrTerminalWidth())

  private[cli] def create(
    config: SpinnerConfig,
    underlying: TerminalOutput[Id],
    timer: SpinnerTimer[Id],
    readColumns: () => Option[Int],
  ): SpinnerNoFx = {
    val initialColumns  = readColumns().filter(_ > 0)
    val lastColumns     = SpinnerRef.atomicRef[Id, Option[Int]](initialColumns)
    val output          = new TerminalOutput[Id] {
      override def write(s: String): Unit = underlying.write(s)
      override def isTTY: Boolean         = underlying.isTTY
      override def rows: Option[Int]      = underlying.rows
      override def columns: Option[Int]   = {
        val measured = readColumns().filter(_ > 0)
        lastColumns.updateAndGet(previous => measured.orElse(previous))
      }
    }
    val effectiveConfig =
      if (initialColumns.isEmpty || !underlying.isTTY) config.withEnabled(false) else config
    Spinner.create[Id](effectiveConfig, output, timer, SpinnerRefMaker.atomicRef[Id])
  }
}
