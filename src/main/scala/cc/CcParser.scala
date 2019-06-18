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

  def atoms(ctx: List[String]): Parser[Exp] = prop | typ | variable(ctx) | "(" ~> arrows(ctx) <~ ")" | "[" ~> arrows(ctx) <~ "]" | lam(ctx) | pi(ctx)

  def scope[X](v: String, ctx: List[String])(ctxParser: List[String] => Parser[X]): Parser[X] = ("." ~> ctxParser(v :: ctx))

  //left associative, TODO: there should be something in the defualt lib for this?
  def withInfix[X](underlying: Parser[X], s: String, f: X => X => X): Parser[X] = {

    def rest(l: X): Parser[X] = (s ~> underlying >> { r => rest(f(l)(r)) }) | "" ^^^ l //TODO: more idomatic way?

    for {
      x <- underlying
      res <- rest(x)
    } yield res
  }

  def apps(ctx: List[String]): Parser[Exp] = withInfix[Exp](atoms(ctx), "", { x => y => App(x, y) })

  def arrows(ctx: List[String]): Parser[Exp] = apps(ctx) ~ (("→" ~> arrows(ctx)).?) ^^ {
    case ty ~ None      => ty
    case ty ~ Some(bod) => Pi(ty, bod.open())
  }

  def lam(ctx: List[String]): Parser[Lam] = (("λ" ~> varName <~ ":") >> { v => arrows(ctx) ~ scope[Exp](v, ctx)(arrows) }) ^^
    { case ty ~ bod => Lam(ty, bod) }
  
    def pi(ctx: List[String]): Parser[Pi] = (("Π" ~> varName <~ ":") >> { v => arrows(ctx) ~ scope[Exp](v, ctx)(arrows) }) ^^
      { case ty ~ bod => Pi(ty, bod) }
  

  def exp: Parser[Exp] = arrows(List()) <~ """\s*""".r

}


object CcParser extends CcParser {

  def parse(s: String): ParseResult[Exp] = parse(exp, s)

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