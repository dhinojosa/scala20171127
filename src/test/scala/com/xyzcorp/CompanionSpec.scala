package com.xyzcorp

import org.scalatest.{FunSuite, Matchers}

class CompanionSpec extends FunSuite with Matchers {
  class SecretAgent(val name: String) {
    def shoot(n: Int) {
      SecretAgent.decrementBullets(n) //Can be imported and shortened
      //using import SecretAgent._
    }
  }

  object SecretAgent {
    //This is encapsulated!
    private var b: Int = 3000 //only available privately

    private def decrementBullets(count: Int) {  //only available privately
      if (b - count <= 0) b = 0
      else b = b - count
    }

    def bullets: Int = b
  }

  test("Companion object") {
    val bond = new SecretAgent("James Bond")
    val felix = new SecretAgent("Felix Leitner")
    val jason = new SecretAgent("Jason Bourne")
    val _99 = new SecretAgent("99")
    val max = new SecretAgent("Max Smart")

    bond.shoot(800)
    felix.shoot(200)
    jason.shoot(150)
    _99.shoot(150)
    max.shoot(200)

    println(SecretAgent.bullets) //How many bullets are left?
  }


  test("Superhero") {
    val unknown = new Superhero("Clark Kent", "Superman")

    Superhero.unlock(unknown) should be ("Superman")
  }
}






