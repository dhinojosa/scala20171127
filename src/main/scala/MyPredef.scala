package com.xyzcorp

object MyPredef {
  implicit class MyIntWrapper(x:Int) {
    def isOdd = x % 2 != 0
    def isEven = !isOdd
  }

}
