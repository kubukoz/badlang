package badlang

import cats.Applicative
import cats.Functor
import cats.data.NonEmptyList
import cats.data.NonEmptyList as NEL
import cats.implicits.*
import cats.parse.Numbers

enum Op[F[_]] {

  case Let(
    name: F[Name],
    value: F[Value],
  )

  case Inc(
    name: F[Name]
  )

  case Show(
    names: F[NonEmptyList[F[Name]]]
  )

}

opaque type Name = String

object Name {

  def apply(
    value: String
  ): Name = value

}

extension (
  name: Name
) def value: String = name

enum Value {

  case Str(
    value: String
  )

  case Num(
    value: Long
  )

}

case class SourceFile[F[_]](
  ops: F[List[F[Op[F]]]]
)

object parser {

  extension (
    sf: SourceFile[T]
  )

    def names: List[T[Name]] = sf.ops.value.flatMap {
      _.value
        .match {
          case Op.Let(name, _) => name.pure[NEL]
          case Op.Inc(name)    => name.pure[NEL]
          case Op.Show(names)  => names.value
        }
        .toList
    }

    def findNameAt(
      pos: Position
    ): Option[T[Name]] = sf.ops.value.map(_.value).collectFirstSome {
      case Op.Let(name, _) if name.range.contains(pos) => name.some
      case Op.Inc(name) if name.range.contains(pos)    => name.some
      case Op.Show(names)                              => names.value.find(_.range.contains(pos))
      case (_: Op.Let[_]) | (_: Op.Inc[_])             => None
    }

    def findDefinitionAt(
      pos: Position
    ): Option[T[Name]] = sf.ops.value.map(_.value).collectFirst {
      case Op.Let(name, _) if name.range.contains(pos) => name
    }

    def findReferences(
      of: Name
    ): List[T[Name]] = sf.ops.value.map(_.value).flatMap {
      case Op.Inc(name) if name.value == of => name :: Nil
      case Op.Show(names)                   => names.value.find(_.value == of)
      case (_: Op.Let[_]) | (_: Op.Inc[_])  => Nil
    }

    def findDefinition(
      of: Name
    ): Option[T[Name]] = sf.ops.value.map(_.value).collectFirst {
      case Op.Let(name, _) if name.value == of => name
    }

    def findReferenceAt(
      pos: Position
    ): Option[T[Name]] = sf.ops.value.map(_.value).collectFirstSome {
      case Op.Inc(name) if name.range.contains(pos) => name.some
      case Op.Show(names)                           => names.value.find(_.range.contains(pos))
      case (_: Op.Let[_]) | (_: Op.Inc[_])          => None
    }

  case class T[A](
    value: A,
    range: Range,
  )

  case class Range(
    start: Position,
    end: Position,
  ) {

    def contains(
      pos: Position
    ): Boolean =
      start.line <= pos.line && pos.line <= end.line &&
        start.col <= pos.col && pos.col <= end.col

  }

  case class Position(
    line: Int,
    col: Int,
  )

  extension (
    r: Range
  ) {

    def toLSP: langoustine.lsp.structures.Range = langoustine
      .lsp
      .structures
      .Range(
        start = r.start.toLSP,
        end = r.end.toLSP,
      )

  }

  extension (
    r: langoustine.lsp.structures.Range
  )

    def toModel: Range = Range(
      start = r.start.toModel,
      end = r.end.toModel,
    )

  extension (
    p: Position
  )

    def toLSP: langoustine.lsp.structures.Position = langoustine
      .lsp
      .structures
      .Position(
        line = p.line,
        character = p.col,
      )

  extension (
    p: langoustine.lsp.structures.Position
  )

    def toModel: Position = Position(
      line = p.line.value,
      col = p.character.value,
    )

  import cats.parse.{Parser => P}
  import cats.parse.{Parser0 => P0}

  val ws: P0[Unit] = P.charIn(" \t\r").rep0.void
  val newlineWithSpaces: P[Unit] = ws.with1 *> (P.string("\r\n") | P.char('\n').void) <* ws

  // lowercase chars
  val name: P[Name] = P.charIn('a' to 'z').rep.string.map(Name(_))
  val pos = P.caret.map(c => Position(line = c.line, col = c.col))

  extension [A](
    p: P[A]
  )

    def ranged: P[T[A]] = (pos.with1 ~ p ~ pos).map { case ((start, value), end) =>
      T(value, Range(start, end))
    }

  extension [A](
    p: P0[A]
  )

    def ranged: P0[T[A]] = (pos ~ p ~ pos).map { case ((start, value), end) =>
      T(value, Range(start, end))
    }

  val stringLiteral = P
    .anyChar
    .repUntil0(P.char('"'))
    .string
    .with1
    .surroundedBy(P.char('"'))

  val long = (Numbers.nonZeroDigit *> Numbers.digit).string.mapFilter(_.toLongOption)

  val value = stringLiteral.map(Value.Str(_)) | long.map(Value.Num(_))

  val show: P[Op.Show[T]] =
    (
      P.string("SHOW") *>
        ws *>
        name.ranged.repSep(ws)
    )
      .ranged
      .map(Op.Show(_))

  val inc: P[Op.Inc[T]] =
    (P.string("INC") *>
      ws *>
      name.ranged)
      .map(Op.Inc(_))

  val let: P[Op.Let[T]] =
    (
      P.string("LET") *> ws *> name.ranged <* ws,
      value.ranged,
    ).mapN(Op.Let.apply[T])

  val op: P[Op[T]] = (show | inc | let).widen

  val sourceFile = op
    .ranged
    .repSep0(newlineWithSpaces.rep)
    .surroundedBy(newlineWithSpaces.rep0)
    .ranged
    .map(SourceFile[T](_))

  def parse(
    s: String
  ) = sourceFile
    .parseAll(s)
    .leftMap(e => (e.expected.map(showExpectation).mkString_(" OR "), e.failedAtOffset))

  private def showExpectation(
    e: P.Expectation
  ): String = {
    import P.Expectation._

    e match {
      case OneOfStr(_, List(str))             => prep(str)
      case OneOfStr(_, strs)                  => strs.map(prep).mkString_(" OR ")
      case InRange(_, 'A', 'Z')               => "an uppercase letter"
      case InRange(_, 'a', 'z')               => "a lowercase letter"
      case InRange(_, '0', '9')               => "digit"
      case InRange(_, from, to) if from == to => prep(from.toString)
      case InRange(_, from, to) => s"one of ${prep(from.toString)} - ${prep(to.toString)}"
      case EndOfString(_, _)    => "end of string"
      case e                    => e.toString
    }
  }

  def expectationString(
    e: P.Error,
    verbose: Boolean,
  ): String = e
    .expected
    .map(showExpectation(_))
    .mkString_(" OR ")

  private def prep(
    s: String
  ): String = s.replace(' ', '·').replace("\n", "⏎\n")

}
