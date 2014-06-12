package recfun
import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c || r == 0) 1
    else pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    balance(chars,0)
  }

  @tailrec
  def balance(chars: List[Char],open: Int): Boolean = {
    if (chars.isEmpty) open == 0
    else if (chars.head.equals(')') && open <=0 ) false
    else if (chars.head.equals('(')) balance(chars.tail,open+1)
    else if (chars.head.equals(')')) balance(chars.tail,open-1)
    else balance(chars.tail,open)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money-coins.head,coins)
  }
}
