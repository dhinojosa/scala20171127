package com.xyzcorp

case class Employee(
  firstName: String,
  lastName: String,
  department: Department)
  
case class Department(name: String)


object MyRunner {
  def main(args: Array[String]) {
    println("Hello, Scala")
  }
}