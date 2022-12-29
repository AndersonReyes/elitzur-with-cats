package com.github.andersonreyes

import cats.data.ValidatedNec
import cats.implicits._
import com.github.andersonreyes.Validator.ValidationResult
import com.spotify.elitzur.validators.BaseValidationType
import shapeless._

import scala.annotation.unused
import scala.language.implicitConversions

trait Validator[T] {
  def validate(v: T): ValidationResult = validate(v, v.getClass.getSimpleName)
  def validate(v: T, fieldName: String): ValidationResult
}

object Validator extends LabelledTypeClassCompanion[Validator] {
  case class InvalidField(name: String, ttype: String) {
    override def toString: String =
      s"InvalidField(name = $name, type = $ttype)"
  }
  type ValidationResult = ValidatedNec[InvalidField, Any]

  private def ignore[T](
      v: T,
      @unused fieldName: String = ""
  ): ValidationResult =
    InvalidField(fieldName, v.getClass.getSimpleName).validNec

  implicit def elitzurBaseValidator[T <: BaseValidationType[_]]: Validator[T] =
    (v: T, fieldName: String) =>
      if (v.checkValid) {
        v.validNec
      } else {
        InvalidField(fieldName, v.getClass.getSimpleName).invalidNec

      }

  implicit def longValidator: Validator[Long] = ignore[Long]

  implicit def intValidator: Validator[Int] = ignore[Int]

  implicit def stringValidator: Validator[String] = ignore[String]

  implicit def booleanValidator: Validator[Boolean] = ignore[Boolean]

  implicit def byteValidator: Validator[Byte] = ignore[Byte]

  implicit def doubleValidator: Validator[Double] = ignore[Double]

  implicit def optionValidator[T](implicit
      validator: Validator[T]
  ): Validator[Option[T]] = (v: Option[T], fieldName: String) =>
    v match {
      case Some(value) => validator.validate(value, fieldName)
      case None        => ignore[Option[T]](v, fieldName)
    }

  object typeClass extends LabelledTypeClass[Validator] {
    def emptyProduct: Validator[HNil] = ignore

    def product[F, T <: HList](
        name: String,
        sh: Validator[F],
        st: Validator[T]
    ): Validator[F :: T] = (ft: F :: T, _) => {
      // TODO: name is not full path only raw field name. Difficult to read
      // for nested case classes.
      val head = sh.validate(ft.head, name)
      val tail = st.validate(ft.tail)
      val combined = List(head, tail).sequence_
      combined
    }

    def emptyCoproduct: Validator[CNil] = ignore

    def coproduct[L, R <: Coproduct](
        name: String,
        sl: => Validator[L],
        sr: => Validator[R]
    ): Validator[L :+: R] = (v, _) =>
      v match {
        case Inl(l) => List(sl.validate(l, name)).sequence_
        case Inr(r) => List(sr.validate(r)).sequence_
      }

    def project[F, G](
        instance: => Validator[G],
        to: F => G,
        from: G => F
    ): Validator[F] =
      (f: F, _) => instance.validate(to(f))
  }

}

object ValidatorSyntax {
  def validate[T](v: T)(implicit
      validator: Validator[T]
  ): ValidationResult =
    validator.validate(v)
}
