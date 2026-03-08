package sfc.util

import java.io.{File, PrintWriter}
import scala.util.Using

/** Generic CSV writer with [[Using.resource]] for safe cleanup. */
object CsvWriter:
  def write[A](file: File, header: String, rows: Iterable[A])(render: A => String): Unit =
    Using.resource(new PrintWriter(file)) { pw =>
      pw.write(header + "\n")
      for row <- rows do pw.write(render(row) + "\n")
    }
