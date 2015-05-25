package main.scala

// I stole this from Keimi

sealed trait AST

final case class Num(n: Int) extends AST

final case class Add(l: AST, r: AST) extends AST

final case class Mul(l: AST, r: AST) extends AST

final case class Minus(l: AST, r: AST) extends AST

object AST {
  def evaluate(node: AST): Int = node match {
    case Num(n) => n
    case Add(x, y) => evaluate(x) + evaluate(y)
    case Mul(x, y) => evaluate(x) * evaluate(y)
    case Minus(x, y) => evaluate(x) - evaluate(y)
  }

  def prettyPrint(node: AST): String = node match {
    case Num(n) => n.toString
    case Add(x, y) => "(" + prettyPrint(x) + " + " + prettyPrint(y) + ")"
    case Mul(x, y) => "(" + prettyPrint(x) + " * " + prettyPrint(y) + ")"
    case Minus(x, y) => "(" + prettyPrint(x) + " - " + prettyPrint(y) + ")"
  }
}