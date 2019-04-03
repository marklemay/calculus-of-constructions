package cc

import scala.util.parsing.combinator._
import cc.Cc._
import scala.annotation.migration
import scala.language.postfixOps

class CcParser extends RegexParsers {
  def prop: Parser[Prop] = "●" ^^^ Prop()
  def typ: Parser[Typ] = "□" ^^^ Typ()

  def varName: Parser[String] = """[a-z|A-Z|_][a-z|A-Z|0-9|'|_]*""".r

  def variable(ctx: List[String]): Parser[Var] = varName ^? (
    { case name if ctx.contains(name) => Var(ctx.indexOf(name)) },
    { name => s"no var in scope with name $name" })

  def scope(v: String, ctx: List[String]): Parser[Exp] = ("." ~> exp(v :: ctx))

  def lam(ctx: List[String]): Parser[Lam] = (("λ" ~> varName <~ ":") >> { v => apps(ctx) ~ scope(v, ctx) }) ^^
    { case ty ~ bod => Lam(ty, bod) }

  def pi(ctx: List[String]): Parser[Pi] = (("Π" ~> varName <~ ":") >> { v => apps(ctx) ~ scope(v, ctx) }) ^^
    { case ty ~ bod => Pi(ty, bod) }

  def apps(ctx: List[String]): Parser[Exp] = (exp(ctx) ~ (exp(ctx) *)) ^^
    { case head ~ rest => rest.foldLeft[Exp](head)(App) }

  def exp(ctx: List[String]): Parser[Exp] = prop | typ | variable(ctx) | lam(ctx) | pi(ctx) | "(" ~> apps(ctx) <~ ")" | "[" ~> exp(ctx) <~ "]"

  def totalParse = apps(List()) <~ """\s*""".r

}

object CcParser extends CcParser {

  def parse(s: String): ParseResult[Exp] = parse(totalParse, s)

  implicit class CcParserHelper(val sc: StringContext) extends AnyVal {
    //    def cc(args: Any*): Exp = parse(sc.standardInterpolator({ x => "( " ++ x ++ " )" }, args)) match {
    def cc(args: Any*): Exp = parse(sc.standardInterpolator({ x => x }, args.map(x => "( " ++ x.toString() ++ " )"))) match {
      case Success(matched, rest) if rest.atEnd => matched
      case Success(matched, rest) => {
        scala.sys.error("error in the code starting at:\r\n" + rest.pos.longString)
      }
      //      case Failure(msg, _)     => println("FAILURE: " + msg)
      //      case Error(msg, _)       => println("ERROR: " + msg)
    }
  }

}