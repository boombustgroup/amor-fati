package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import zio.{Console, ZIO}

import java.util.Locale

private[montecarlo] object McRunnerConsole:

  enum Event:
    case RunId(runId: String)
    case MonthProgress(seed: Long, totalSeeds: Int, month: ExecutionMonth, durationMonths: Int)
    case SeedDone(
        seed: Long,
        totalSeeds: Int,
        elapsedMillis: Long,
        adoption: Double,
        inflation: Double,
        unemployment: Double,
    )
    case BlankLine
    case SavedFile(path: String)
    case TotalTime(seconds: Double)

  private val BarWidth = 20

  def emit(event: Event): ZIO[Any, SimError, Unit] =
    event match
      case runId: Event.RunId            => Console.printLine(render(runId)).mapError(runtimeFailure("print run id"))
      case progress: Event.MonthProgress => Console.print(render(progress)).mapError(runtimeFailure("print month progress"))
      case done: Event.SeedDone          => Console.printLine(render(done)).mapError(runtimeFailure("print seed summary"))
      case Event.BlankLine               => Console.printLine("").mapError(runtimeFailure("print separator"))
      case saved: Event.SavedFile        => Console.printLine(render(saved)).mapError(runtimeFailure("print saved file paths"))
      case total: Event.TotalTime        => Console.printLine(render(total)).mapError(runtimeFailure("print total time"))

  def emitAll(events: Iterable[Event]): ZIO[Any, SimError, Unit] =
    ZIO.foreachDiscard(events)(emit)

  private[montecarlo] def render(event: Event): String =
    event match
      case runId: Event.RunId            => renderRunId(runId)
      case progress: Event.MonthProgress => renderMonth(progress)
      case done: Event.SeedDone          => renderSeedDone(done)
      case Event.BlankLine               => ""
      case saved: Event.SavedFile        => renderSaved(saved)
      case total: Event.TotalTime        => renderTotalTime(total)

  private def renderRunId(runId: Event.RunId): String =
    s"  run-id: ${runId.runId}"

  private def renderMonth(progress: Event.MonthProgress): String =
    val frac   = progress.month.toInt.toDouble / progress.durationMonths
    val filled = (frac * BarWidth).toInt
    val bar    = "\u2588" * filled + "\u2591" * (BarWidth - filled)
    val pct    = (frac * 100).toInt
    f"\r  Seed ${progress.seed}%3d/${progress.totalSeeds} [$bar] ${progress.month.toInt}%3d/${progress.durationMonths}m ($pct%3d%%)"

  private def renderSeedDone(done: Event.SeedDone): String =
    val bar = "\u2588" * BarWidth
    f"\r  Seed ${done.seed}%3d/${done.totalSeeds} [$bar] done (${done.elapsedMillis}ms) | " +
      s"Adopt=${formatPct(done.adoption)} | pi=${formatPct(done.inflation)} | " +
      s"Unemp=${formatPct(done.unemployment)}"

  private def renderSaved(saved: Event.SavedFile): String =
    s"Saved: ${saved.path}"

  private def renderTotalTime(total: Event.TotalTime): String =
    String.format(Locale.US, "\nTotal time: %.1f seconds", Double.box(total.seconds))

  private def formatPct(value: Double): String =
    String.format(Locale.US, "%5.1f%%", Double.box(value * 100.0))

  private def runtimeFailure(operation: String)(err: Throwable): SimError =
    SimError.RuntimeFailure(operation, Option(err.getMessage).filter(_.nonEmpty).getOrElse(err.getClass.getSimpleName))
