package badlang

import cats.effect.IO
import cats.parse.Caret
import cats.parse.LocationMap
import langoustine.lsp.aliases.DocumentDiagnosticReport
import langoustine.lsp.enumerations.DiagnosticSeverity
import langoustine.lsp.runtime.Opt
import langoustine.lsp.structures.Diagnostic
import langoustine.lsp.structures.DocumentDiagnosticParams
import langoustine.lsp.structures.Location
import langoustine.lsp.structures.Position
import langoustine.lsp.structures.RelatedFullDocumentDiagnosticReport

def diagnostics(
  in: DocumentDiagnosticParams,
  docs: TextDocuments,
): IO[DocumentDiagnosticReport] = docs.get(in.textDocument.uri).map(_.content).map { fileText =>
  val map = LocationMap(fileText)
  val lastOffset = map.toCaretUnsafe(fileText.length)
  extension (
    c: Caret
  ) def toLSPPosition: Position = Position(c.line, c.col)

  import analysis.*
  import parser.*

  def withParsed(
    parsed: SourceFile[parser.T]
  ) = {
    val lints = parsed.lint.fold(_.toList, _ => Nil)

    parsed
      .typecheck
      .match {
        case Right(_)     => Vector.empty
        case Left(errors) => errors.toList.toVector
      }
      .concat(lints)
      .map { diag =>
        Diagnostic(
          range = diag.range.toLSP,
          severity = Opt(diag.level match {
            case badlang.DiagnosticLevel.Error   => DiagnosticSeverity.Error
            case badlang.DiagnosticLevel.Warning => DiagnosticSeverity.Warning
          }),
          message = diag.issue.message,
        )
      }
  }
  val items =
    parser.parse(fileText) match {
      case Right(parsed) => withParsed(parsed)

      case Left((msg, offset)) =>
        val parseError = Vector(
          Diagnostic(
            range = langoustine
              .lsp
              .structures
              .Range(
                map.toCaretUnsafe(offset).toLSPPosition,
                lastOffset.toLSPPosition,
              ),
            severity = Opt(DiagnosticSeverity.Error),
            message = "Parsing error: expected " + msg,
          )
        )

        // minimal-effort attempt: parse all previous lines and get some diagnostics from that.
        // technically we could try and parse each line separately anyway (and probably should), but this is a start.
        val parsedEarlier = {
          val lineFailed = map.toCaretUnsafe(offset)
          val lineStart = map.toOffset(lineFailed.line, 0).getOrElse(sys.error("no offset"))
          parser.parse(fileText.take(lineStart))
        }

        val extraDiags = parsedEarlier.fold(_ => Nil, withParsed)

        parseError ++ extraDiags
    }

  DocumentDiagnosticReport(
    RelatedFullDocumentDiagnosticReport(
      kind = "full",
      items = items,
    )
  )
}
