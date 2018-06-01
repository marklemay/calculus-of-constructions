package cc

import scala.util.parsing.combinator._
import cc.Cc._
import scala.annotation.migration

class CcParser extends RegexParsers { // with JavaTokenParsers{ // ident is probably to permissive with unicode
  def prop: Parser[Prop] = "●" ^^^ Prop()
  def typ: Parser[Typ] = "□" ^^^ Typ()

  def varName: Parser[String] = """[a-z|A-Z]+""".r

  def variable(ctx: List[String]): Parser[Var] = varName ^? (
    { case name if ctx.contains(name) => Var(ctx.indexOf(name)) },
    { name => s"no var in scope with name $name" })

  def scope(v: String, ctx: List[String]): Parser[Exp] = ("." ~> exp(v :: ctx))

  def lam(ctx: List[String]): Parser[Lam] = (("λ" ~> varName <~ ":") >> { v => exp(ctx) ~ scope(v, ctx) }) ^^
    { case ty ~ bod => Lam(ty, bod) }

  def pi(ctx: List[String]): Parser[Pi] = (("Π" ~> varName <~ ":") >> { v => exp(ctx) ~ scope(v, ctx) }) ^^
    { case ty ~ bod => Pi(ty, bod) }

  def apps(ctx: List[String]): Parser[Exp] = "(" ~> exp(ctx) ~ (exp(ctx) *) <~ ")" ^^
    { case head ~ rest => rest.foldLeft[Exp](head)(App) }

  def exp(ctx: List[String]): Parser[Exp] = apps(ctx) | prop | typ | variable(ctx) | lam(ctx) | pi(ctx)

}

object CcParser extends CcParser {


  implicit class SimpleCCParserHelper(val sc: StringContext) extends AnyVal {
    def cc(args: Any*): Exp = parse(exp(List()), sc.standardInterpolator({ x => x }, args)) match {
      case Success(matched, _) => matched //TODO: if "" is empty
      //      case Failure(msg, _)     => println("FAILURE: " + msg)
      //      case Error(msg, _)       => println("ERROR: " + msg)
    }
  }

}