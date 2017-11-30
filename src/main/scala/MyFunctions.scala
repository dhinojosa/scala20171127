package com.xyzcorp

object MyFunctions {
  //A String
  //B Int
  //C Int
  def combineFunctions[A, B, C](f: B => C, g: A => B):A => C = {
    //num => f(g(num)) ***
    //f.compose(g)
    g andThen (f)
  }
}
