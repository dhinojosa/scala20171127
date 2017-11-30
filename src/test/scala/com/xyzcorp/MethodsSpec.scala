package com.xyzcorp

import org.scalatest.{FunSuite, Matchers}

import scala.annotation.tailrec

class MethodsSpec extends FunSuite with Matchers {
  def numberStatus(a:Int) =
    if (a < 10) "Less than 10"
    else if (a > 10) "Greater than 10"
    else "It is 10!"

  test("a method invocation") {
    val result = numberStatus(3)
    result should be ("Less than 10")
  }
  
  def combine[A](x:List[A], y:List[A]) = {
    x ++ y
  }
  
  test("combine my generic collections") {
    val listA = List(List("Foo", "Mariners"), Set(1,2,0))
    val listB = List("foo", "bar", "baz")
    val combined = combine(listA, listB)
    combined.length should be (5)
  }



  def myMax(x:List[Int]):Option[Int] = {
    @tailrec
    def myInnerMax(x:List[Int], currentMax:Int):Option[Int] = {
      if (x.isEmpty) None
      else if (x.length == 1) {
        if (x.head > currentMax) Some(x.head)
        else Some(currentMax)
      }
      else {
        if (x.head > currentMax) {
          myInnerMax(x.tail, x.head)
        }
        else {
          myInnerMax(x.tail, currentMax)
        }
      }
    }
    myInnerMax(x, -1000000)
  }

  test("Using myMax") {
    myMax(Nil) should be (None)
    myMax(List(1)) should be (Some(1))
    myMax(List(1,2)) should be (Some(2))
    myMax(List(2,1)) should be (Some(2))
    myMax(List(10,20,5,100,1)) should be (Some(100))
  }



}