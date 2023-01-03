package com.github.andersonreyes

import cats.data.Validated
import cats.implicits._
import com.github.andersonreyes.Validator.InvalidField

object ValidationRunner {
  def validate[T](v: T)(implicit
      validator: Validator[T]
  ): Option[List[InvalidField]] =
    validator.validate(v) match {
      case Validated.Valid(_)   => None
      case Validated.Invalid(e) => Some(e.toList)
    }
}
