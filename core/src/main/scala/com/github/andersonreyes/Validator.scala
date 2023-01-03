package com.github.andersonreyes

import cats.data.Validated.Invalid
import cats.data.ValidatedNec
import cats.implicits._
import com.github.andersonreyes.Validator.ValidationResult
import com.spotify.elitzur.validators.BaseValidationType
import shapeless._

import scala.annotation.unused
import scala.language.implicitConversions

trait Validator[T] {
  def validate(v: T): ValidationResult
}

object Validator extends LabelledTypeClassCompanion[Validator] {


  case class InvalidField(field: String)
  // Return unit because we only check for errors, no need to construct type again
  type ValidationResult = ValidatedNec[InvalidField, Unit]

  /** Native types are always valid (compiler handles this. For example you cant have val b: Int = true)
    */
  private def ignore[T](
      @unused v: T
  ): ValidationResult = ().validNec

  implicit def elitzurBaseValidator[T <: BaseValidationType[_]]: Validator[T] =
    (v: T) =>
      if (!v.checkValid) {
        InvalidField("data").invalidNec
      } else { ().validNec }

  implicit val longValidator: Validator[Long] = ignore[Long]

  implicit val intValidator: Validator[Int] = ignore[Int]

  implicit val stringValidator: Validator[String] = ignore[String]

  implicit val booleanValidator: Validator[Boolean] = ignore[Boolean]

  implicit val byteValidator: Validator[Byte] = ignore[Byte]

  implicit val doubleValidator: Validator[Double] = ignore[Double]

  implicit def vectorValidator[T](implicit
      validator: Validator[T]
  ): Validator[Vector[T]] = (v: Vector[T]) => {
    // Fix: Can we return the list of invalid values here?
    val bad = v
      .mapWithIndex { (v, i) =>
        (validator.validate(v), i)
      }
      .flatMap {
        case (Invalid(_), i) => Some(i)
        case _               => None
      }
      .mkString(",")

    if (bad.nonEmpty) {
      InvalidField(s"($bad)").invalidNec
    } else { ().validNec }
  }

  implicit def arrayValidator[T](implicit
      validator: Validator[T]
  ): Validator[Array[T]] = (v: Array[T]) =>
    vectorValidator(validator).validate(v.toVector)

  implicit def listValidator[T](implicit
      validator: Validator[T]
  ): Validator[List[T]] = (v: List[T]) =>
    vectorValidator(validator).validate(v.toVector)

  implicit def optionValidator[T](implicit
      validator: Validator[T]
  ): Validator[Option[T]] = {
    case Some(value) => validator.validate(value)
    case None        => ignore[Option[T]](None)
  }

  object typeClass extends LabelledTypeClass[Validator] {
    def emptyProduct: Validator[HNil] = ignore

    def product[F, T <: HList](
        name: String,
        sh: Validator[F],
        st: Validator[T]
    ): Validator[F :: T] = (ft: F :: T) => {
      val head = sh.validate(ft.head)
      val tail = st.validate(ft.tail)
      head
        // Include field name here if head is invalid
        .leftMap(e => e.map(f => f.copy(s"$name.${f.field}")))
        .combine(tail)
    }

    def emptyCoproduct: Validator[CNil] = ignore

    def coproduct[L, R <: Coproduct](
        name: String,
        sl: => Validator[L],
        sr: => Validator[R]
    ): Validator[L :+: R] = {
      case Inl(l) =>
        if (sl.validate(l).isInvalid) InvalidField(l.toString).invalidNec
        else ().validNec
      case Inr(r) =>
        if (sr.validate(r).isInvalid) InvalidField(r.toString).invalidNec
        else ().validNec
    }

    def project[F, G](
        instance: => Validator[G],
        to: F => G,
        from: G => F
    ): Validator[F] = (v: F) => instance.validate(to(v))
  }

}
