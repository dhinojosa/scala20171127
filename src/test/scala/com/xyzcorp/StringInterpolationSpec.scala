package com.xyzcorp

import org.scalatest.FunSuite
import org.scalatest.Matchers

class StringInterpolationSpec extends FunSuite with Matchers {
  test("Basic String interpolation") {
    val a = 99 //Setting up a value within the context
    println(s"$a luftballoons floating in the summer sky")  //a + 3
  }

  test("String interpolation with plus 3") {
    val a = 99 //Setting up a value within the context
    println(s"${a + 3} luftballoons floating in the summer sky")  //a + 3
  }
}