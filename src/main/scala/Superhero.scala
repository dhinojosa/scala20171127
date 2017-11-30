package com.xyzcorp

class Superhero(val name:String, secretIdentity: String) {
  private def revealSecret():String = secretIdentity
}

object Superhero {
  def unlock(s:Superhero) = s.revealSecret()
}
