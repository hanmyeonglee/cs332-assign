package recfun
import common._

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
    println(balance("())(".toList))
    println(balance("((()))".toList))
    println(balance("()()()".toList))
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
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
  def countChange(money: Int, coins: List[Int]): Int = {
    1
  }
}
