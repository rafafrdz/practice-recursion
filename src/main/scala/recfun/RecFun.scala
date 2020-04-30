package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c==0 || c == r) 1
    else pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def aux(ac: Int,chars: List[Char]): Boolean={
      if(chars.isEmpty) (ac == 0)
      else if (ac<0) false
      else if (chars.head == '(') aux(ac+1,chars.tail)
      else if (chars.head == ')') aux(ac-1,chars.tail)
      else aux(ac,chars.tail)
    }
    aux(0,chars)
  }

  /**
   * Exercise 3
   */

   def countChange(money: Int, coins: List[Int]): Int = {
    if(money==0) 1
    else if (coins.isEmpty) 0
    else if (money < coins.head) 0
    else countChange(money-coins.head,coins) + countChange(money,coins.tail)
}

  // def countChange(money: Int, coins: List[Int]): Int = {
  //   def aux(ac: Int, xs: List[Int]): Int = {
  //     if(ac==money) 1
  //     else if (xs.isEmpty) 0
  //     else if (ac > money) 0
  //     else aux(ac+xs.head,xs) + aux(ac,xs.tail)
  //   }
  //   aux(0,coins)
  // }
}
