package com.xyzcorp

import org.scalatest.{FunSuite, Matchers}

class ClassesSpec extends FunSuite with Matchers {
    test("Instantiate an Employee") {
      val emp = Employee("Bill", "Gates", 
          Department("Philanthropy"))
      
      emp.firstName should be ("Bill")
      emp.lastName should be ("Gates")
      
      val emp2 = Employee("Bill", "Gates", 
          Department("Philanthropy"))
      //These 2 are the same thing
      (emp == emp2) should be (true)
      (emp.equals(emp2)) should be (true)
      
      //Test whether the same ref
      (emp eq emp2) should be (false)
      emp should be (emp2)
      
      val Employee(fn, _, _) = emp
      fn should be ("Bill")
      
    }
}
