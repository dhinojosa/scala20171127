package com.xyzcorp

import org.scalatest.{FunSpec, FunSuite, Matchers}

class PatternMatchingSpec extends FunSuite with Matchers {

  def whatIsTheMiddleName_?[A](x:Option[A]):String = {
    val str = x match {
      case Some(Employee("Bob", _, Department(dn))) => s"Bob's Department: $dn"
      case Some(Employee(_, _, Department(dn))) => s"Department: $dn"
      case Some(m) => s"The answer is $m"
      case None => "N/A"
      case _ => "I have no idea"
    }
    str
  }

  test("Pattern matching with Option"){
     whatIsTheMiddleName_?(Some("Bob")) should be ("The answer is Bob")
     whatIsTheMiddleName_?(None:Option[String]) should be ("N/A")
     whatIsTheMiddleName_?(Some(Employee("Willie", "Nelson", Department("High")))) should be ("Department: High")
  }


}
