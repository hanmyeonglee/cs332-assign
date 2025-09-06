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

    println(balance("((if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(balance("()()((()))".toList))

    println(countChange(4, List(1,2)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, 0) => 1
    case (col, row) if col < 0 || col > row => 0
    case (col, row) => pascal(col - 1, row - 1) + pascal(col, row - 1)
  }

  /**
   * Exercise 2
   */
  @tailrec
  def balance(chars: List[Char]): Boolean = {
    val length = chars.length;

    if (chars.isEmpty) true
    else if (length == 1) false
    else {
      val headParenthesisIndex = chars.indexOf('(');
      var tailParenthesisIndex = -1;
      var count = 1;

      (headParenthesisIndex + 1 until length)
        .iterator
        .takeWhile(_ => count > 0)
        .foreach(i => {
          val ch = chars(i);
          count += {
            if (ch == '(') 1
            else if (ch == ')') -1
            else 0
          }
          tailParenthesisIndex = {
            if (ch == ')') i
            else tailParenthesisIndex
          }
        });

      if (headParenthesisIndex == -1) tailParenthesisIndex == -1
      else if (tailParenthesisIndex == -1) false
      else balance(
        chars.slice(0, headParenthesisIndex) ++
          chars.slice(tailParenthesisIndex + 1, length)
      )
    }
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 1
    case (money, _) if money < 0 => 0
    case (_, Nil) => 0
    case (money, c :: cs) => countChange(money - c, c :: cs) + countChange(money, cs)
  }
//  {
//    if (money == 0) 1
//    else if (money < 0 || coins.isEmpty) 0
//    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
//  }
}
