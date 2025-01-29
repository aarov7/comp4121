object CountDigits{

  def countDigits(num: Int): Int = {
    if(num < 0) {
      countDigits(-1*num)
    }
    else {
      if (num < 10) {
        1
      }
      else {
        1 + countDigits(num / 10)
      }
    }
  }

  val value: Int = -231;
  val numDigits: Int = countDigits(value);
  Std.printInt(numDigits)
}