package com.xyzcorp

object CaesarShift {
  def encode(str: String, shift: Int):String =
    if (shift == 0) str
    else (str.charAt(0) + shift - 'z' + 96).toChar.toString
}
