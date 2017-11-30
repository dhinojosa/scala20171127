package com.xyzcorp

import org.scalatest.{FunSuite, Matchers}

class LazyValsSpec extends FunSuite with Matchers {
  test("lazy val forward reference") {
     lazy val a = 10 + b
     lazy val b = 19
     a should be (29)
   }

  test("Strip Margin") {
    val prose = """I see trees of green,
               @|red roses too
               @I see @ them bloom,
               @for me and you,
               @and I think to myself,
               @what I wonderful world""".stripMargin('@')
    println(prose)
  }
}