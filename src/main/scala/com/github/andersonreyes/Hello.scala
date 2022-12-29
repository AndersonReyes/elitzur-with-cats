package com.github.andersonreyes

import com.spotify.elitzur.validators.BaseValidationType

case class NotEmpty(data: String) extends BaseValidationType[String] {
  override def checkValid: Boolean = data.nonEmpty
}

object Hello extends App {
  import Validator._
  import ValidatorSyntax._

  case class T(long: Long, empty: NotEmpty, notEmpty: NotEmpty)
  case class W(int: Int, t: T)
  println(validate(5))
  println(validate("sdfsdfd"))
  println(validate(Option(5)))
  println(validate(NotEmpty("")))
  println(validate(NotEmpty("dfsddf")))
  println(validate(W(5, T(5, NotEmpty(""), NotEmpty("")))))

}
