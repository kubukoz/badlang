package badlang

import badlang.parser.Position
import cats.Monad
import cats.Parallel
import cats.data.EitherNel
import cats.data.EitherT
import cats.data.NonEmptyList as NEL
import cats.data.State
import cats.implicits.*
import parser.T

case class Diagnostic(
  issue: ValidationIssue,
  range: parser.Range,
  level: DiagnosticLevel,
  tags: Set[DiagnosticTag] = Set.empty,
)

enum DiagnosticLevel {
  case Error
  case Warning
}

enum DiagnosticTag {
  case Unnecessary
}

enum ValidationIssue {

  case UnusedVariable

  case TypeMismatch(
    expected: Type,
    actual: Type,
  )

  case DuplicateDefinition
  case SymbolUnknown

  def message: String =
    this match {
      case UnusedVariable      => "Unused variable."
      case DuplicateDefinition => "This symbol has already been defined."
      case SymbolUnknown       => "Unknown symbol."
      case TypeMismatch(expected, actual) =>
        s"Type mismatch: expected a ${expected.show}, but found a ${actual.show}."
    }

}

object analysis {

  case class OutputLine(
    opRange: parser.Range,
    text: String,
  )

  case class InterpreterState(
    output: List[OutputLine],
    names: Map[Name, Value],
  )

  case class TyperState(
    names: Map[T[Name], Type]
  )

  extension (
    sf: SourceFile[T]
  )

    def lint: EitherNel[Diagnostic, Unit] = {

      // find unused variables
      // very crude implementation, but it'll work for now
      val ops = sf.ops.value

      def isUsed(
        name: Name
      ): Boolean = ops
        .mapFilter {
          _.value.match {
            case Op.Show(names) => names.value.some
            case _              => none
          }
        }
        .flatMap(_.toList)
        .exists(_.value == name)

      // go through each op
      // if it's not a show, check if there's a show using the given name afterwards
      // if not, keep that and return a warning
      val unusedVariables = sf.ops.value.parTraverse_ { op =>
        def report(
          name: T[?]
        ) =
          Diagnostic(
            ValidationIssue.UnusedVariable,
            name.range,
            DiagnosticLevel.Warning,
            tags = Set(DiagnosticTag.Unnecessary),
          ).leftNel

        op.value match {
          case Op.Let(name, _) if !isUsed(name.value) => report(name)
          case Op.Inc(name) if !isUsed(name.value)    => report(name)
          case _                                      => Right(())
        }
      }

      unusedVariables
    }

    def typecheck: EitherNel[Diagnostic, Unit] = {
      type Stateful[F[_]] = cats.mtl.Stateful[F, TyperState]
      type Raise[F[_]] = cats.mtl.Raise[F, NEL[Diagnostic]]

      def validateE[F[_]: Stateful: Raise: Monad: Parallel]: F[Unit] = {
        val S = summon[Stateful[F]]
        val R = summon[Raise[F]]

        def ensureKnown(
          name: T[Name]
        ) = S.get.flatMap { state =>
          state
            .names
            .find(_._1.value == name.value)
            .fold {
              R.raise(
                NEL
                  .one(Diagnostic(ValidationIssue.SymbolUnknown, name.range, DiagnosticLevel.Error))
              )
            }(v => v.pure[F])
        }

        def ensureKnownNum(
          name: T[Name]
        ): F[Unit] = ensureKnown(name).flatMap {
          case (_, tpe) if tpe == Type.Num => ().pure[F]
          case (_, tpe) =>
            R.raise(
              NEL
                .one(
                  Diagnostic(
                    ValidationIssue.TypeMismatch(expected = Type.Num, actual = tpe),
                    name.range,
                    DiagnosticLevel.Error,
                  )
                )
            )
        }

        def ensureUnknown(
          name: T[Name]
        ): F[Unit] = S.get.flatMap { state =>
          state
            .names
            .find(_._1.value == name.value)
            .fold {
              ().pure[F]
            }(_ =>
              R.raise(
                NEL
                  .one(
                    Diagnostic(
                      ValidationIssue.DuplicateDefinition,
                      name.range,
                      DiagnosticLevel.Error,
                    )
                  )
              )
            )
        }

        def setType(
          name: T[Name],
          tpe: Type,
        ): F[Unit] = S.modify { state =>
          state.copy(names = state.names + (name -> tpe))
        }

        sf.ops.value.parTraverse_ { op =>
          op.value.match {
            case Op.Inc(name)        => ensureKnownNum(name)
            case Op.Let(name, value) => ensureUnknown(name) &> setType(name, value.value.tpe)
            case Op.Show(names)      => names.value.parTraverse_(ensureKnown)
          }
        }
      }

      validateE[EitherT[State[TyperState, _], NEL[Diagnostic], _]]
        .value
        .runA(TyperState(Map.empty))
        .value
    }

    def execute: List[OutputLine] =
      sf.ops
        .value
        .foldLeft(InterpreterState(Nil, Map.empty)) {
          (
            state,
            op,
          ) =>
            op.value match {
              case Op.Let(name, v) => state.copy(names = state.names + (name.value -> v.value))
              case Op.Inc(name) =>
                val v = state.names.getOrElse(name.value, Value.Num(0)).asInstanceOf[Value.Num]
                state.copy(names = state.names + (name.value -> Value.Num(v.value + 1)))
              case Op.Show(names) =>
                state.copy(
                  output = state
                    .output
                    .appended(
                      OutputLine(
                        opRange = op.range,
                        text = names
                          .value
                          .map(n => state.names(n.value).renderString)
                          .mkString_(""),
                      )
                    )
                )
            }
        }
        .output

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
      case (_: Op.Let[?]) | (_: Op.Inc[?])             => None
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
      case Op.Show(names)                   => names.value.filter(_.value == of)
      case (_: Op.Let[?]) | (_: Op.Inc[?])  => Nil
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
      case (_: Op.Let[?]) | (_: Op.Inc[?])          => None
    }

}
