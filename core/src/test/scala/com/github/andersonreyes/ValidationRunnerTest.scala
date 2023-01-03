package com.github.andersonreyes

import cats.implicits._
import com.github.andersonreyes.ValidationRunner.validate
import org.scalatest.matchers.should

class ValidationRunnerTest
    extends org.scalatest.flatspec.AnyFlatSpec
    with should.Matchers {
  it should "validate Int" in {

    validate(5) should be(5.valid)
  }
}
