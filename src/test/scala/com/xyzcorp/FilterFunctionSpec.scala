package com.xyzcorp

import org.scalatest.{FunSuite, Matchers}

class FilterFunctionSpec extends FunSuite with Matchers {
  test("""A filter will take a function that
      returns a boolean, and will remove all elements
      where the function evaluates to false, in a
      List of Options this will do something peculiar""") {
    import scala.language.postfixOps

    val list = List(Some(4), Some(10), None,
      Some(11), None, Some(50),
      Some(3), None, Some(100))
    val result = list.filter(None!=).map(_.get)
    result should be (List(4, 10, 11, 50, 3, 100))
  }
}
