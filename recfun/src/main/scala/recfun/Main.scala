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

    println(balance("((if (zero? x) max (/ 1 x)))".toList))
    println(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(balance("()()((()))".toList))

    println(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
  (c, r) match {
    case (0, 0) => 1
    case (col, row) if col < 0 || col > row => 0
    case (col, row) => pascal(col - 1, row - 1) + pascal(col, row - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def _balance(chars: List[Char], nofParens: Int): Boolean =
      chars match {
        case _ if nofParens < 0 => false
        case Nil => nofParens == 0
        case '(' :: tailChars => _balance(tailChars, nofParens + 1)
        case ')' :: tailChars => _balance(tailChars, nofParens - 1)
        case _ :: tailChars => _balance(tailChars, nofParens)
      }

    _balance(chars, 0);
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
