package badlang

import cats.data.NonEmptyList as NEL
import cats.implicits.*

enum Op[F[_]] {

  case Let(
    name: F[Name],
    value: F[Value],
  )

  case Inc(
    name: F[Name]
  )

  case Show(
    names: F[NEL[F[Name]]]
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

  def tpe: Type =
    this match {
      case Num(_) => Type.Num
      case Str(_) => Type.Str
    }

  def renderString: String =
    this match {
      case Num(value) => value.toString
      case Str(value) => value
    }

}

case class SourceFile[F[_]](
  ops: F[List[F[Op[F]]]]
)

enum Type {
  case Num
  case Str

  def show: String =
    this match {
      case Num => "number"
      case Str => "string"
    }

}
