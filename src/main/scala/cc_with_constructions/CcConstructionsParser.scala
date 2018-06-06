package cc_with_constructions

import scala.util.parsing.combinator._
import cc.Cc._
import scala.annotation.migration

//TODO: still very inefficient
class CcConstructionsParser extends RegexParsers {
  def prop: Parser[Prop] = "●" ^^^ Prop()
  def typ: Parser[Typ] = "□" ^^^ Typ()

  def varName: Parser[String] = """[a-z|A-Z]+""".r

  def variable(ctx: List[String]): Parser[Var] = varName ^? (
    { case name if ctx.contains(name) => Var(ctx.indexOf(name)) },
    { name => s"no var in scope with name $name" })

  def scope(v: String, ctx: List[String])(config: ConstructionsConfig): Parser[Exp] = ("." ~> subExp(v :: ctx)(config))

  def lam(ctx: List[String])(config: ConstructionsConfig): Parser[Lam] = (("λ" ~> varName <~ ":") >> { v => subExp(ctx)(config) ~ scope(v, ctx)(config) }) ^^
    { case ty ~ bod => Lam(ty, bod) }

  def pi(ctx: List[String])(config: ConstructionsConfig): Parser[Pi] = (("Π" ~> varName <~ ":") >> { v => subExp(ctx)(config) ~ scope(v, ctx)(config) }) ^^
    { case ty ~ bod => Pi(ty, bod) }

  def apps(ctx: List[String])(config: ConstructionsConfig): Parser[Exp] = (exp(ctx)(config) ~ (exp(ctx)(config) *)) ^^
    { case head ~ rest => rest.foldLeft[Exp](head)(App) }

  def parseConstruction(ls: List[Constructions.Construction])(ctx: List[String])(config: ConstructionsConfig): Parser[Exp] = ls match {
    case List()       => apps(ctx)(config)
    case head :: tail => head.parserExp(ctx)(config)(this)(parseConstruction(tail)(ctx)(config)) | parseConstruction(tail)(ctx)(config)
  }

  def subExp(ctx: List[String])(config: ConstructionsConfig) = parseConstruction(config.configs.toList)(ctx)(config)

  def exp(ctx: List[String])(config: ConstructionsConfig): Parser[Exp] = prop | typ | variable(ctx) | lam(ctx)(config) | pi(ctx)(config) | "(" ~> subExp(ctx)(config) <~ ")" | "[" ~> subExp(ctx)(config) <~ "]"

}

object CcConstructionsParser extends CcConstructionsParser {

  def parse(s: String)(config: ConstructionsConfig): ParseResult[Exp] = parse(subExp(List())(config), s)

  implicit class CcConstructionsParserHelper(val sc: StringContext) extends AnyVal {
    def ccc(args: Any*)(implicit config: ConstructionsConfig = ConstructionsConfig()): Exp = parse(sc.standardInterpolator({ x => x }, args))(config) match {
      case Success(matched, _) => matched //TODO: if "" is empty
      //      case Failure(msg, _)     => println("FAILURE: " + msg)
      //      case Error(msg, _)       => println("ERROR: " + msg)
    }
  }

}