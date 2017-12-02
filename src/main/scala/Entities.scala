package com.xyzcorp

import java.util.Comparator

case class Employee(
  firstName: String,
  lastName: String,
  department: Department)

object Employee {
  import scala.language.implicitConversions
  implicit def convertStringToEmployee(x:String): Employee = {
    val items = x.split(" ")
    Employee(items.head, items.tail.mkString(""), Department("Toys"))
  }


  implicit val employeeOrderByFirstName = new Ordering[Employee] {
    override def compare(x: Employee, y: Employee): Int
    = x.firstName.compareTo(y.firstName)
  }

  implicit val employeeOrderByLastName = new Ordering[Employee] {
    override def compare(x: Employee, y: Employee): Int
    = x.lastName.compareTo(y.lastName)
  }

  //I hate this
  implicit val employeeOrderByLastThenFirst: Ordering[Employee] =
    new Ordering[Employee] {
    override def compare(x: Employee, y: Employee): Int
    =  employeeOrderByLastName.thenComparing(employeeOrderByFirstName).compare(x,y)
  }


}
  
case class Department(name: String)


object MyRunner {
  def main(args: Array[String]) {
    println("Hello, Scala")
  }
}