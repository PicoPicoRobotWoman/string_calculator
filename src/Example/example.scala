package Example

import StringCalculator.StringCalculator
import _root_.StringCalculator.model.constant.ConstantFabric
import _root_.StringCalculator.model.unary.UnaryFunctionFabric
import _root_.StringCalculator.model.binary.BinaryOperationFabric

import scala.math._

object example extends App with StringCalculator with BinaryOperationFabric with UnaryFunctionFabric with ConstantFabric {

  //добавлем необходимые бинарные операторы
  addBinaryOperation(createBinary("+", 1, (left, right) => left + right, 0)) // сложение
  addBinaryOperation(createBinary("-", 1, (left, right) => left - right, 0)) // вычетание
  addBinaryOperation(createBinary("*", 2, (left, right) => left * right, 1)) // умножение
  addBinaryOperation(createBinary("/", 2, (left, right) => left / right, 0)) // деление
  addBinaryOperation(createBinary("^", 3, (left, right) => Math.pow(left, right), 0)) // возведение в степень
  addBinaryOperation(createBinary("%", 3, (left, right) => left % right, 0)) // взятие модуля

  //добовляем унарные функции
  addUnaryFunction(createUnary("abs", num => abs(num))) // модуль
  addUnaryFunction(createUnary("exp", num => exp(num))) //
  addUnaryFunction(createUnary("sqrt", num => sqrt(num))) // квадратный корень
  addUnaryFunction(createUnary("cbrt", num => cbrt(num))) //кубический корень
  addUnaryFunction(createUnary("ln", num => log(num))) // натуральный логарифм
  addUnaryFunction(createUnary("lb", num => log(num)/Math.log(2))) // логарифм по основанию 2
  addUnaryFunction(createUnary("lg", num => log10(num))) // логарифм по основанию 10
  addUnaryFunction(createUnary("cos", num => cos(num))) // косинуч
  addUnaryFunction(createUnary("sin", num => sin(num))) // синус
  addUnaryFunction(createUnary("tan", num => tan(num))) // тангенс
  addUnaryFunction(createUnary("cot", num => 1/atan(num))) // котангенс
  addUnaryFunction(createUnary("asin", num => asin(num))) // арксинус
  addUnaryFunction(createUnary("acos", num => acos(num))) // аркосинус
  addUnaryFunction(createUnary("atan", num => atan(num))) // арктангенс
  addUnaryFunction(createUnary("acot", num => atan(1/num))) // аркотангенс
  addUnaryFunction(createUnary("sinh", num => sinh(num))) // гиперболический синус
  addUnaryFunction(createUnary("cosh", num => cosh(num))) // гиперболический косинус
  addUnaryFunction(createUnary("tanh", num => tanh(num))) // гиперболический тангенс
  addUnaryFunction(createUnary("coth", num => 1 / tanh(num))) // гиперболический котангенс
  addUnaryFunction(createUnary("asinh", num => log(num + sqrt(pow(num, 2) + 1 )))) // ареасинус
  addUnaryFunction(createUnary("acosh", num => log(num + sqrt(pow(num, 2) - 1 )))) // ареакосинус
  addUnaryFunction(createUnary("atanh", num => log((1 + num)/(1 - num))/2)) // ареатангенс
  addUnaryFunction(createUnary("acoth", num => log((num + 1)/(num - 1))/2)) // ареакотангенс
  addUnaryFunction(createUnary("floor", num => floor(num))) // округление в меньшую сторону
  addUnaryFunction(createUnary("round", num => round(num))) // округление

  //добавляем константы
  addConstant(createConstant("pi", Math.PI)) // Пи
  addConstant(createConstant("tau", 2 * Math.PI)) // тау
  addConstant(createConstant("e", Math.E)) // число эйлера
  addConstant(createConstant("fi", (1 + Math.sqrt(5)) / 2)) // число фибаначи

  // примеры
  printResalt("cos(-pi/2)^2 + sin(-pi/2)^2")
  printResalt("lg(1 - 1/2)+lg(1 - 1/3)+lg(1 - 1/4)+lg(1 - 1/5)+lg(1 - 1/6)+lg(1 - 1/7)+lg(1 - 1/8)+lg(1 - 1/9)+lg(1 - 1/10)")
  printResalt("round((tanh(-2*pi)-(e^(-2*pi)-1)/(e^(-2*pi)+1))*100)")
  printResalt("round(fi^20)")
  printResalt("cosh(pi+e) - (cosh(pi)*sinh(e)+sinh(pi)*cosh(e))")

  //функцияя для удобства
  def printResalt(expression: String): Unit = {
    println(s"$expression = ${calculate(expression)}")
  }

}
