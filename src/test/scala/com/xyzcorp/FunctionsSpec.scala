package com.xyzcorp

import java.net.URL

import org.scalatest.{FunSuite, Matchers}
import MyFunctions.combineFunctions

import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

class FunctionsSpec extends FunSuite with Matchers {
  test(
    """the compose function takes two functions,
         and composes them into one""") {
    val add1: Int => Int = 1 +
    val stringSize: String => Int = _.length
    val stringSizePlus1 = combineFunctions(add1, stringSize)
    stringSizePlus1("Foo") should be(4)
  }


  test("simple closure") {
    val shift = 2
    val result = "Hello".map(c => (c + shift).toChar)
  }

  test("class closure") {
    class Calculator(val x: Int, val y: Int) {
      def total: Int = x + y
    }

    val calculator = new Calculator(10, 12)

    List(1, 2, 3, 4).map(i => calculator.total + i)
  }

  test("class closure with a calc class") {
    class Calculator(val x: Int, val y: Int) {
      def total: Int = x + y

      def addToTotal(z: Int): Int = x + y + z
    }

    val calculator = new Calculator(10, 12)

    val result = List(1, 2, 3).map(calculator.addToTotal)
    result should be(List(23, 24, 25))
  }

  test("Closure curried ") {
    val f2: Int => Int => Int => Int =
      (x: Int) => { (y: Int) => { (z: Int) => x + y + z } }
    val f3: Int => Int => Int = f2(4)
    val f4: Int => Int = f3(10)
    val f5: Int = f4(5)

    val function: (Int, Int, Int) => Int =
      Function.uncurried(f2)
  }
  //
  //
  //  test("Get stock quote") {
  //    val google = "http://www.google.com/finance/historical?q=NASDAQ%3aSTOCK&startdate=Jan+01%2C+2009&enddate=Aug+2%2C+2012&output=csv"
  //    val yahoo = "...."
  //
  //
  //    def buyOrSell_?(x:(String) => Double): Unit = {
  //
  //    }
  //
  //
  //    val f = (url:String, stock:String) => {
  //      val u = new URL(url.replace("STOCK", stock))
  //      //parse double
  //      40.00
  //    }
  //
  //    val googleLookup = f.curried(google)
  //    val yahooLookup = f.curried(google)
  //  }
  //
  //  test("reorder parameters") {
  //    def orig(s:String, i:Int, j:Float) = s + i + j
  //
  //    def newMethod(i:Int, j:Float, s:String) = orig(s, i, j)
  //
  //    val intToString = orig("Foo", _, 40F)
  //  }

  test("flatMap") {
    val text =
      """In the town where I was born
        |Lived a man who sailed to sea
        |And he told us of his life
        |In the land of submarines
        |
        |So we sailed up to the sun
        |Till we found the sea of green
        |And we lived beneath the waves
        |In our yellow submarine
        |
        |We all live in a yellow submarine
        |Yellow submarine, yellow submarine
        |We all live in a yellow submarine
        |Yellow submarine, yellow submarine
        |
        |And our friends are all on board
        |Many more of them live next door
        |And the band begins to play
        |
        |We all live in a yellow submarine
        |Yellow submarine, yellow submarine
        |We all live in a yellow submarine
        |Yellow submarine, yellow submarine
        |
        |{Full speed ahead, Mr. Boatswain [pronounced bo'sun], full speed ahead!
        |Full speed it is, Sgt.!
        |Cut the cable, drop the cable!
        |Aye, sir, aye!
        |Captain, captain! [pronounced cap'n, cap'n]}
        |
        |As we live a life of ease (A life of ease)
        |Everyone of us (Everyone of us) has all we need (Has all we need)
        |Sky of blue (Sky of blue) and sea of green (Sea of green)
        |In our yellow (In our yellow) submarine (Submarine, ha, ha)
        |
        |We all live in a yellow submarine
        |Yellow submarine, yellow submarine
        |We all live in a yellow submarine
        |Yellow submarine, yellow submarine
        |We all live in a yellow submarine
        |Yellow submarine, yellow submarine
        |We all live in a yellow submarine
        |Yellow submarine, yellow submarine
      """.stripMargin

    val uglyChars = List('}', '{', ',', ';', '(', ')', '[', ']', '!')

    val stringses =
      text
        .split("\n")
        .view
        .filter(_.trim.length != 0)
        .flatMap(_.split(" "))
        .map(_.filterNot(uglyChars.contains))
        .map(_.toLowerCase)
        .groupBy(identity)
        .mapValues(_.length)
        .toList
        .sortBy(_.swap)
        .reverse


    println(stringses)
  }

  test("flatMap with Map") {
    val origMap = Map(1 -> "One",
      2 -> "Two",
      3 -> "Three")

    println(origMap.flatMap(t =>
      Map(t._1 -> t._2, (t._1 * 100) -> (t._2 + " Hundred"))))
  }


  test("Lists with flatMap") {
    val xs = List(1,2,3,4)
    val ys = List(5,6,7,8)

    val result: List[(Int, Int)] =
      xs.flatMap(x => ys.map(y => (x,y)))

    println(result)

    val result2 = for (x <- xs;
                       y <- ys) yield (x,y)

    println(result2)

    val result3 = for (x <- xs;
                       y <- Nil) yield (x,y)

    println(result3)
  }


  def timeThis[A](bl: => A): (A, Long) = {
    val startTime = System.currentTimeMillis()
    val result = bl
    (result, System.currentTimeMillis() - startTime)
  }

  test("by-name parameter") {
    val tuple: (Int, Long) = timeThis {
      Thread.sleep(3000)
      7 * 100
    }
    println(tuple)


    val tupleB = timeThis {
      val a = "Foo"
      val b = "Bar"
      Thread.sleep(3000)
      a + b
    }

    println(tupleB)

    val tupleC = timeThis{40; 40 + 10}
    println(tupleC)
  }


  test("Monadic Try") {
    val triedInt = Try {5 * 100}

    val triedInt2 = Try {5 / 0}

    val result = for (i <- triedInt;
                      j <- triedInt2 ) yield i + j

    val str = result match {
      case Success(x:Int) => s"Yay! $x"
      case Failure(t:Throwable) => s"Oops ${t.getMessage}"
      case _ => "I don't know what..."
    }

    println(str)
  }
}





