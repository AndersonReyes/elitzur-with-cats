package com.github.andersonreyes

import com.spotify.elitzur.validators.BaseValidationType

case class NotEmpty(data: String) extends BaseValidationType[String] {
  override def checkValid: Boolean = data.nonEmpty
}
//
object Hello extends App {
  import ValidationRunner._
  import Validator._

  case class T(long: Long, notEmpty: NotEmpty)
  case class W(
      option: Option[String],
      vector: List[NotEmpty],
      int: Int,
      bool: Boolean,
      t: T,
      empty: NotEmpty
  )

  val w = W(
    None,
    Vector(NotEmpty("hello"), NotEmpty("")).toList,
    int = 3,
    false,
    t = T(7, NotEmpty("")),
    empty = NotEmpty("")
  )
//  println(validate("sdfsdfd"))
//  println(validate(Option(5)))
//  println(validate(NotEmpty("")))
//  println(validate(NotEmpty("dfsddf")))
  println(validate(w))
//  println(validate(Vector(NotEmpty("true"), NotEmpty(""))))
//  println(validate(Array(NotEmpty("true"), NotEmpty(""))))
//  println(validate(List(NotEmpty("true"), NotEmpty(""))))

  println()

}
