package StringCalculator

import StringCalculator.Util.calculatorUtils
import StringCalculator.model.binary.BinaryOperation
import StringCalculator.model.constant.Constant
import StringCalculator.model.unary.UnaryFunction

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

trait StringCalculator extends calculatorUtils {

  private val binaryOperations: ListBuffer[BinaryOperation] = new ListBuffer[BinaryOperation]
  private val unaryFunctions: ListBuffer[UnaryFunction] = new ListBuffer[UnaryFunction]
  private val constants: ListBuffer[Constant] = new ListBuffer[Constant]
  private val separator: String = "†"

  def calculate(expression: String): Any = {

    val replacedExpression = replaceExpressionsWithExclamation(expression.replace(" ", ""))

    val startExpression = binaryOperations.foldLeft(replacedExpression)((acc, binary) => {
      if (acc.startsWith(binary.getDesignation)) s"${binary.getSingleElement}$acc" else acc
    })

    val reducedExpression: String = binaryOperations.map(binary => binary.getDesignation)
      .foldLeft(startExpression)((str, designation) => str.replace(designation.toString, s"$separator$designation$separator"))
      .replace("(", s"$separator($separator")
      .replace(")", s"$separator)$separator")
      .replace(s"$separator$separator", separator)

    val tokens: List[String] = reducedExpression.split(separator).toList

    val tokenWithConstant = constants.foldLeft(tokens)((acc, const) =>
      acc.foldLeft(List[String]())((acc, token) => {
        if (token == const.getDesignation) acc :+ const.getValue.toString else acc :+ token
      })
    )

    val fin = binaryOperations.foldLeft(tokenWithConstant)((acc, binary) => {

      val indexing = acc.zipWithIndex.foldLeft(List[Int]())((acc, tokenIndex) => {
        if (tokenIndex._1 == binary.getDesignation) acc :+ tokenIndex._2 else acc
      })

      indexing.sorted.reverse.foldLeft(acc)((acc, index) => {
        if (!isNumeric(acc(index - 1))) (acc.take(index) :+ calculate(s"${binary.getSingleElement}${binary.getDesignation}" + acc(index + 1)).toString) ++ acc.takeRight(acc.size - index - 2) else acc
      })

    })

    val tokensUnWithBinary = fin.reverse.foldLeft(List[String] {tokens.last})((acc, token) => if (unaryFunctions.contains(token)) {
      val num = acc.last.toDouble
      acc.init :+ unaryFunctions.filter(_.getDesignation == token).head.calc(num).toString
    } else {
      acc :+ token
    }).reverse.init.filter(_.nonEmpty)

    val minPriority = binaryOperations.map(_.getPriority).min
    implicit val ordering: Ordering[BinaryOperation] = Ordering.by(1.0 / _.getPriority)
    val orderedOperations = binaryOperations.toList.filter(_.getPriority != minPriority).sorted

    val finalTokens = orderedOperations.foldLeft(tokensUnWithBinary) ((acc, binary) => {

      val indexes = tokensUnWithBinary.zipWithIndex.filter(_._1 == binary.getDesignation).map(_._2)

      indexes.sorted.foldRight(acc)((index, acc) => {
        val left = tokensUnWithBinary(index - 1).toDouble
        val right = tokensUnWithBinary(index + 1).toDouble
        (acc.take(index - 1) :+ binary.calc(left, right).toString) ++ acc.takeRight(acc.size - index - 2)
      })

    })

    val numbers: List[Double] = finalTokens.filter(token => isNumeric(token)).map(token => token.toDouble)
    val operations: List[String] = finalTokens.filter(token => !isNumeric(token))

    performCalculations(numbers, operations)

  }

  def addBinaryOperation(binary: BinaryOperation): Unit = {
    binaryOperations += binary
  }

  def addUnaryFunction(unary: UnaryFunction): Unit = {
    unaryFunctions += unary
  }

  def addConstant(const: Constant): Unit = {
    constants += const
  }

  @tailrec
  private def performCalculations(numbers: List[Double], operations: List[String]): Double = {

    val right: Double = numbers.last

    if (operations.nonEmpty) {
      operations.last match {
        case x if (binaryOperations.contains(x)) =>
          val res = binaryOperations.filter(op => op.equals(x)).head.calc(numbers(numbers.size - 2), right)
          if (numbers.init.init.isEmpty && operations.init.isEmpty) res else performCalculations(numbers.init.init :+ res, operations.init)
        case token => throw new Exception(s"$token - неизвестный токен")
      }
    } else {
      numbers.last
    }

  }

  def replaceExpressionsWithExclamation(str: String): String = {

    (separator + "(.*?)" + separator).r.replaceAllIn(str.foldLeft(("", 0)) { (acc, char) =>
      val (output, bracketDepth) = acc
      if (char == '(') (output + (if (bracketDepth > 0) char else separator), bracketDepth + 1)
      else if (char == ')') (output + (if (bracketDepth > 1) char else separator), bracketDepth - 1)
      else (output + char, bracketDepth)
    }._1 , { m =>
      s"$separator${calculate(m.group(1))}$separator"
    })

  }

}
