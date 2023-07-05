package StringCalculator.Util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try

trait calculatorUtils {

  def isNumeric(token: String): Boolean = {
    Try(token.toDouble).isSuccess
  }

  def extractSubstringWithNestedBrackets(str: String): String = {
    @tailrec
    def extractSubstring(str: String, startIndex: Int, depth: Int, substring: mutable.StringBuilder): Unit = {
      if (startIndex >= str.length) return
      val char = str(startIndex)
      if (char == '(') {
        if (depth > 0) substring.append(char)
        extractSubstring(str, startIndex + 1, depth + 1, substring)
      } else if (char == ')') {
        if (depth > 1) substring.append(char)
        else return
        extractSubstring(str, startIndex + 1, depth - 1, substring)
      } else {
        if (depth > 0) substring.append(char)
        extractSubstring(str, startIndex + 1, depth, substring)
      }
    }

    val substringBuilder = new mutable.StringBuilder()
    extractSubstring(str, 0, 0, substringBuilder)
    substringBuilder.toString()
  }

}
