package com.xyzcorp

import java.math.BigInteger
import java.time.ZoneId
import java.util.concurrent.Executors

import org.scalatest.{FunSuite, Matchers}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}


class ImplicitsSpec extends FunSuite with Matchers {

  implicit val num = 50

  test("simple implicit scope 1") {
    //implicit val numLocal1 = 10
    def foo(x: Int)(implicit y: Int) = x + y

    foo(20) should be(30)
  }

  test("simple implicit scope 2") {
    //implicit val numLocal2 = 40
    def foo(x: Int)(implicit y: Int) = x + y

    foo(20) should be(60)
  }


  test("calc rate") {
    case class Rate(i: Int)
    implicit val rate = Rate(10)

    def calcRate(hrs: Int)(implicit rate: Rate) = hrs * rate.i
  }

  test("creating a Future") {
    case class Rate(i: Int)
    implicit val rate = Rate(10)

    //    implicit val executionContext: ExecutionContextExecutor = ExecutionContext
    //      .fromExecutor(Executors.newFixedThreadPool(5))
    //
    import ExecutionContext.Implicits.global

    val future: Future[Int] = Future {
      println(s"In future block: ${Thread.currentThread().getName}")
      Thread.sleep(4000)
      implicitly[Rate].i * 1000
    }

    val future2 = Future {
      println(s"In future2 block: ${Thread.currentThread().getName}")
      Thread.sleep(2000)
      (20, 40)
    }

    //    val r: Future[Int] =
    //      future.flatMap(x => future2.map(y => x + y))

    val r = for {x <- future
                 (age, score) <- future2} yield x + age + score

    r.foreach { x =>
      println(s"In foreach: ${Thread.currentThread().getName}")
      println(x)
    }

    Thread.sleep(5000)
  }

  test("Future with List") {
    import ExecutionContext.Implicits.global

    val future = Future {
      println(s"In future block: ${Thread.currentThread().getName}")
      List(1, 2, 3, 4)
    }

    val future2 = Future {
      println(s"In future2 block: ${Thread.currentThread().getName}")
      List(5, 6, 7, 8)
    }

//    val result3 =
//      for {lst1 <- future
//           lst2 <- future2}
//        yield
//          for {x <- lst1
//               y <- lst2}
//            yield x * y

    val result3 =
      for {lst1 <- future
           lst2 <- future2}
        yield lst1.zip(lst2).par.map{case (x, y) => x * y}
    result3.foreach(println)

    Thread.sleep(1000)
  }

  test("java.util.BigInteger") {
    val bi = new BigInteger("302390439")
    val bi2 = new BigInteger("3294923492394")
  }

  test("BigInt") {
    val bi = BigInt("302390439")
    val bi2 = BigInt("3023904332949234923949")

    val result = bi + bi2 + 10
    println(result)
  }

  test("Implicit Converter") {
    def hireEmployee(e:Employee) =
      s"Just hired ${e.firstName}"

    val value:Employee = "Luke Skywalker"
    println(hireEmployee(value))
  }

  test("Implicit Decorators with class") {
    class MyIntWrapper(x:Int) {
      def isOdd = x % 2 != 0
      def isEven = !isOdd
    }

    implicit def convertInt2IntWrapper(x:Int)
         = new MyIntWrapper(x)

    10.isOdd should be (false)
  }

  test("Implicit Decorators with def") {
    class MyIntWrapper(x:Int) {
      def isOdd = x % 2 != 0
      def isEven = !isOdd
    }

    implicit val convertInt2IntWrapper = (x:Int) =>
      new MyIntWrapper(x)

    10.isOdd should be (false)
  }


  test("Implicit Decorators with implicit class in its own object") {
    import MyPredef._
    10.isOdd should be (false)
  }

  test("Use implicits to make things Scala") {
    import scala.collection.JavaConverters._
    val set = ZoneId
      .getAvailableZoneIds
      .asScala
      .toSet
      .filter(w => w.startsWith("America"))
      .map(w => w.split("/").last).size

    println(set)
  }


  test("Use an Ordering type class to sort") {

    val dept = Department("Automotive")
    val employees = List(
      Employee("Glenn", "Close", dept),
      Employee("Jamie", "Farr", dept),
      Employee("Tony", "Stark", dept),
      Employee("Albert", "Einstein", dept),
      Employee("Roger", "Moore", dept),
      Employee("Amanda", "Moore", dept),
      Employee("Dudley", "Moore", dept),
      Employee("Mandy", "Moore", dept),
      Employee("Lessis", "Moore", dept)
    )

    import Employee.employeeOrderByLastThenFirst

    val str = employees.sorted.mkString(",")
    println(str)
  }


  test("Equality") {
    val dept = Department("Automotive")

    trait Eq[A] {
      def equals(a1: A, a2: A):Boolean
    }

    def isEquals[A](a1:A, a2: A)(implicit eq:Eq[A]) = {
         if (eq.equals(a1, a2)) "Yes"
         else "No"
    }

    implicit val employeeEqualsFirstNameOnly = new Eq[Employee] {
      override def equals(a1: Employee, a2: Employee): Boolean = {
        a1.firstName.equals(a2.firstName)
      }
    }


    val result = isEquals(new Employee("Bob", "Bobberson", dept),
      new Employee("Bob", "Bobberson", dept))
    println(result)



  }

}
