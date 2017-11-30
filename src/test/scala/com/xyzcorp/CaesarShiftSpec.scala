package com.xyzcorp

import org.scalatest.{FunSuite, GivenWhenThen, Matchers}

class CaesarShiftSpec extends FunSuite with Matchers with GivenWhenThen {
  test("An empty with shift of zero") {
    Given("An empty string")
    val msg = ""
    And("a shift of zero")
    val shift = 0
    When("encode is called with the string and shift")
    val result = CaesarShift.encode("", 0)
    Then("the result should be an empty string")
    result should be("")
  }

  //IMPORTANT: You only want possible red bar tests
  test("A string of one char with shift of zero") {
    Given("A string with one char")
    val msg = "a"
    And("a shift of zero")
    val shift = 0
    When("encode is called with the string and shift")
    val result = CaesarShift.encode(msg, 0)
    Then("the result should be an empty string")
    result should be("a")
  }

  test("A string of one char with shift of one") {
    Given("A string with one char, an a")
    val msg = "a"
    And("a shift of one")
    val shift = 1
    When("encode is called with the string and shift")
    val result = CaesarShift.encode(msg, shift)
    Then("the result should be a b")
    result should be("b")
  }

  test("A string of a z with shift of one") {
    Given("A string with one char, an z")
    val msg = "z"
    And("a shift of one")
    val shift = 1
    When("encode is called with the string and shift")
    val result = CaesarShift.encode(msg, shift)
    Then("the result should be a b")
    result should be("a")
  }
}
