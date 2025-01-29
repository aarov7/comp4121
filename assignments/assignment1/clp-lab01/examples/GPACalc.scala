object GPACalc{

  def grade(score: Int): String = {
    score match {
      case 0 => "F"
      case 1 => "D"
      case 2 => "C"
      case 3 => "B"
      case 4 => "A"
      case _ => "Not a valid score"
    }
  }

  val letterGrade: String = grade(4);
  Std.printString(letterGrade)
}