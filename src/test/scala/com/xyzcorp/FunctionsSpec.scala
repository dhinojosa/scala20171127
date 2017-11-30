package com.xyzcorp

import java.net.URL

import org.scalatest.{FunSuite, Matchers}
import MyFunctions.combineFunctions

import scala.collection.immutable
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
        .sortBy(t => -t._2 -> t._1)


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
    val xs = List(1, 2, 3, 4)
    val ys = List(5, 6, 7, 8)

    val result: List[(Int, Int)] =
      xs.flatMap(x => ys.map(y => (x, y)))

    println(result)

    val result2 = for (x <- xs;
                       y <- ys) yield (x, y)

    println(result2)

    val result3 = for (x <- xs;
                       y <- Nil) yield (x, y)

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

    val tupleC = timeThis {
      40;
      40 + 10
    }
    println(tupleC)
  }


  test("Monadic Try") {
    val triedInt = Try {
      5 * 100
    }

    val triedInt2 = Try {
      5 / 0
    }

    val result = for (i <- triedInt;
                      j <- triedInt2) yield i + j

    val str = result match {
      case Success(x: Int) => s"Yay! $x"
      case Failure(t: Throwable) => s"Oops ${t.getMessage}"
      case _ => "I don't know what..."
    }

    println(str)
  }

  test("Grocery List") {
    val list = List("Eggs", "Milk", "Naan", "Broccoli",
      "Salmon", "Apples", "Green Lettuce", "Peas")
    val result = list
      .sorted
      .zip(1 to list.length)
      .map(t => s"${t._2}. ${t._1}")
      .mkString("\n")
    // println(result)

    val value = for {x <- list
                     str <- s"$x"} yield str
    println(value)

    //Dennis'
    val dennis = list.sorted.map { x => list.sorted.indexOf(x) + 1 -> x }
      .map(x => s"${x._1}. ${x._2}").mkString("\n")

    println(dennis)

    val resultFor =
      for (x <- list.sorted.zip(1 to list.length))
        yield
          s"${x._2}. ${x._1}"
    println(resultFor.mkString(","))


  }

  val stateCaps =
    """Albany
      |Annapolis
      |Atlanta
      |Augusta
      |Austin
      |Baton Rouge
      |Bismarck
      |Boise
      |Boston
      |Carson City
      |Charleston
      |Cheyenne
      |Columbia
      |Columbus
      |Concord
      |Denver
      |Des Moines
      |Dover
      |Frankfort
      |Harrisburg
      |Hartford
      |Helena
      |Honolulu
      |Indianapolis
      |Jackson
      |Jefferson City
      |Juneau
      |Lansing
      |Lincoln
      |Little Rock
      |Madison
      |Montgomery
      |Montpelier
      |Nashville
      |Oklahoma City
      |Olympia
      |Phoenix
      |Pierre
      |Providence
      |Raleigh
      |Richmond
      |Sacramento
      |Saint Paul
      |Salem
      |Salt Lake City
      |Santa Fe
      |Springfield
      |Tallahassee
      |Topeka
      |Trenton
    """.stripMargin


  val countryCaps =
    """Afghanistan,Kabul
      |Albania,Tirana
      |Andorra,Andorra la Vella
      |Angola,Luanda
      |Antigua and Barbuda,St. John’s
      |Argentina,Buenos Aires
      |Armenia,Yerevan
      |Australia,Canberra
      |Austria,Vienna
      |Azerbaijan,Baku
      |Bahamas,Nassau
      |Bahrain,Manama
      |Bangladesh,Dhaka
      |Barbados,Bridgetown
      |Belarus,Minsk
      |Belgium,Brussels
      |Belize,Belmopan
      |Benin,Porto Novo, Cotonou
      |Bhutan,Thimphu
      |Bolivia,La Paz and Sucre
      |Bosnia and Herzegovina,Sarajevo
      |Botswana,Gaborone
      |Brazil,Brasília
      |Brunei,Bandar Seri Begawan
      |Bulgaria,Sofia
      |Burkina Faso,Ouagadougou
      |Burundi,Bujumbura
      |Cambodia,Phnom Penh
      |Cameroon,Yaoundé
      |Canada,Ottawa
      |Cape Verde,Praia
      |Cayman Islands,George Town
      |Central African Republic,Bangui
      |Chad,N’Djamena
      |Chile,Santiago
      |China,Beijing
      |Colombia,Bogotá
      |Comoros,Moroni
      |Costa Rica,San José
      |Côte d’Ivoire,Yamoussoukro
      |Croatia,Zagreb
      |Cuba,Havana
      |Cyprus,Nicosia
      |Czech Republic,Prague
      |Democratic Republic of the Congo,Kinshasa
      |Denmark,Copenhagen
      |Djibouti,Djibouti
      |Dominica,Roseau
      |Dominican Republic,Santo Domingo
      |East Timor,Dili
      |Ecuador,Quito
      |Egypt,Cairo
      |El Salvador,San Salvador
      |Equatorial Guinea,Malabo
      |Eritrea,Asmara
      |Estonia,Tallinn
      |Ethiopia,Addis Ababa
      |Fiji,Suva
      |Finland,Helsinki
      |France,Paris
      |French Guiana,Cayenne
      |Gabon,Libreville
      |Georgia,Tbilisi
      |Germany,Berlin
      |Ghana,Accra
      |Greece,Athens
      |Grenada,St George’s
      |Guatemala,Guatemala
      |Guinea,Conakry
      |Guinea-Bissau,
      |Guyana,Georgetown
      |Haiti,Port-au-Prince
      |Honduras,Tegucigalpa
      |Hungary,Budapest
      |Iceland,Reykjavik
      |India,New Delhi
      |Indonesia,Jakarta
      |Iran,Tehran
      |Iraq,Baghdad
      |Israel,Jerusalem
      |Italy,Rome
      |Jamaica,Kingston, Jamaica
      |Japan,Tokyo
      |Jordan,Amman
      |Kazakhstan,Astana
      |Kenya,Nairobi
      |Kiribati,South Tarawa
      |Kuwait,Kuwait
      |Kyrgyzstan,Bishkek
      |Laos,Vientiane
      |Latvia,Riga
      |Lebanon,Beirut
      |Lesotho,Maseru
      |Liberia,Monrovia
      |Libya,Tripoli
      |Liechtenstein,Vaduz
      |Lithuania,Vilnius
      |Luxembourg,Luxembourg City
      |Madagascar,Antananarivo
      |Malawi,Lilongwe
      |Malaysia,Kuala Lumpur
      |Maldives,Malé
      |Mali,Bamako
      |Malta,Valletta
      |Marshall Islands,Majuro
      |Mauritania,Nouakchott
      |Mauritius,Port Louis
      |Mexico,Mexico City
      |Micronesia,Palikir
      |Moldova,Chisinau
      |Monaco,Monaco
      |Mongolia,Ulaanbaatar
      |Montenegro,Podgorica
      |Morocco,Rabat
      |Mozambique,Maputo
      |Myanmar,Naypyidaw
      |Namibia,Windhoek
      |Nauru,Yaren
      |Nepal,Kathmandu
      |Netherlands ,Amsterdam
      |New Zealand,Wellington
      |Nicaragua,Managua
      |Niger,Niamey
      |Nigeria,Abuja
      |North Korea,Pyongyang
      |Norway,Oslo
      |Oman,Muscat
      |Pakistan,Islamabad
      |Palau,Koror
      |Palestine,Jerusalem
      |Panama,Panama City
      |Papua New Guinea,Port Moresby
      |Paraguay,Asuncion
      |Peru,Lima
      |Philippines,Manila
      |Poland,Warsaw
      |Portugal,Lisbon
      |Puerto Rico,San Juan
      |Qatar,Doha
      |Republic of Ireland,Dublin
      |Republic of Macedonia,Skopje
      |Republic of the Congo,Brazzaville
      |Romania,Bucharest
      |Russia,Moscow
      |Russia,Moscow
      |Rwanda,Kigali
      |Saint Kitts and Nevis,Basseterre
      |Saint Lucia,Castries
      |Saint Vincent and the Grenadines,Kingstown
      |Samoa,Apia
      |San Marino,San Marino
      |São Tomé and Príncipe,São Tomé
      |Saudi Arabia,Riyadh
      |Senegal,Dakar
      |Serbia,Belgrade
      |Seychelles,Victoria, Seychelles
      |Sierra Leone,Freetown
      |Singapore,Singapore
      |Slovakia,Bratislava
      |Slovenia,Ljubljana
      |Solomon Islands,Honiara
      |Somalia,Mogadishu
      |South Africa,Pretoria
      |South Korea,Seoul
      |South Sudan,Juba
      |Spain,Madrid
      |Sri Lanka,Sri Jayawardenapura Kotte and Colombo now
      |Sudan,Khartoum
      |Suriname,Paramaribo
      |Swaziland,Mbabane
      |Sweden,Stockholm
      |Switzerland,Bern
      |Syria,Damascus
      |Taiwan,Taipei
      |Taiwan,Taipei
      |Tajikistan,Dushanbe
      |Tanzania,Dar es Salaam, Dodoma
      |Thailand,Bangkok
      |The Gambia,Banjul
      |Togo,Lome
      |Tonga,Nuku’alofa
      |Trinidad and Tobago,Port of Spain
      |Tunisia,Tunis
      |Turkey,Ankara
      |Turkey,Ankara
      |Turkmenistan,Asgabat
      |Turks and Caicos,Cockburn Town
      |Tuvalu,Funafuti
      |Uganda,Kampala
      |Ukraine,Kyiv or Kiev
      |United Arab Emirates,Abu Dhabi
      |United Kingdom,London
      |United States,Washington DC
      |Uruguay,Montevideo
      |Uzbekistan,Tashkent
      |Vanuatu,Port Vila
      |Vatican City,Vatican City
      |Venezuela,Caracas
      |Vietnam,Hanoi
      |Western Sahara,La’youn
      |Yemen,Sana’a
      |Zambia,Lusaka
      |Zimbabwe,Harare""".stripMargin

  test("State caps") {
    val cleanCountries = countryCaps
      .split("\n")
      .map(w => w.split(",").last)
      .map(cp => cp.replace(" ", ""))

    println(cleanCountries.toList)
  }


  test("For Comprehension Stream") {
    lazy val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #::
      fibs.zip(fibs.tail).map { n => n._1 + n._2 }

    def loop(x:Int):Stream[Int] = x #:: loop(x+1)

    val result1 = for
        {f <- fibs
         i <- loop(0)} yield (i, f)

    println(result1.take(10).mkString(","))

    val result2 = fibs.flatMap(i => loop(0).map(j => (j, i)))

    println(result2 take 10 mkString ",")
  }
}





