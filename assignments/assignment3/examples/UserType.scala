object TestType{
  abstract class Person

  case class Student (name: String) extends Person

  val a: Student = Student("Aarov"); 1 + 2
  a.name
}