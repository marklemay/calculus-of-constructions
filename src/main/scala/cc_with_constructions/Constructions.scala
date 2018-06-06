package cc_with_constructions

import cc.Cc.Exp
import cc.Cc.Pi

import cc.CcProperties._
import cc.Cc.Var
import cc.CcConstructionsPrettyPrinter._
import cc.Cc.Prop

import cc.CcPrettyPrinter.PartialOutput
import cc.CcPrettyPrinter.Atom
import cc.CcPrettyPrinter.AppList
import cc.CcPrettyPrinter.Outer
import cc.CcPrettyPrinter.showTy
import cc.CcPrettyPrinter.showBod
import cc.CcPrettyPrinter.showApp

//TODO: figure out what should be implicit

//somthing that looks and acts like a Cc expression
object Constructions { //TODO: paramiterixe these over generalized epressions?  expresion functors?

  trait Construction {

    //TODO: implicit conversion to expressions?

    //TODO: Embed the notion of signature?

    type Constructed

    def toExp(c: Constructed): Exp
    def fromExp(e: Exp): Option[Constructed]

    def pettyPrint(c: Constructed, ctx: PrintCtx, p: Printer)(config: ConstructionsConfig): (Set[String], PartialOutput) // (implicit config: ConstructionsConfig)

    def parser(ctx: List[String])(config: ConstructionsConfig)(restParser: CcConstructionsParser)(parseLessPrecidence: restParser.Parser[Exp]): restParser.Parser[Constructed]

    //TODO a little funny
    def parserExp(ctx: List[String])(config: ConstructionsConfig)(restParser: CcConstructionsParser)(parseLessPrecidence: restParser.Parser[Exp]) = parser(ctx)(config)(restParser)(parseLessPrecidence).map(toExp)
  }

  //TODO: split these into seperate files?
  /** non depndent function */
  case object ArrowConstruction extends Construction {

    object Arrow {
      def unapply(e: Exp): Option[(Exp, Exp)] = fromExp(e).map(a => (a.ty, a.bod))

    }
    case class Arrow(ty: Exp, bod: Exp)

    override type Constructed = Arrow

    def toExp(a: Arrow): Exp = {
      val Arrow(ty, bod) = a
      Pi(ty, bod.open())
    }

    def fromExp(e: Exp): Option[Constructed] = e match {
      case Pi(ty, bod) if !bod.freeVars.contains(Var(0)) => Some(Arrow(ty, bod.sub(Var(0))))
      case _ => None
    }

    def pettyPrint(c: Arrow, ctx: PrintCtx, p: Printer)(config: ConstructionsConfig): (Set[String], PartialOutput) = {
      //The best pretty printer would accumulate all the arrows... and then make at the end
      val Arrow(ty, bod) = c
      val (vars1, prettyTy) = p(ty, ctx)(config)
      val (vars2, prettyBod) = p(bod, (vars1, ctx._2))(config)

      (vars2, AppList(s"${showApp(prettyTy)} → ${showTy(prettyBod)}"))
    }

    def parser(ctx: List[String])(config: ConstructionsConfig)(restParser: CcConstructionsParser)(parseLessPrecidence: restParser.Parser[Exp]): restParser.Parser[Arrow] = {
      import restParser._

      (parseLessPrecidence ~ ("→" ~> subExp(ctx)(config))) ^^ { case (in ~ out) => Arrow(in, out) }
    }
  }

  case object ProdConstruction extends Construction {

    object Prod {
      def unapplySeq(e: Exp): Option[Seq[Exp]] = fromExp(e).map(_.ls)
    }
    case class Prod(ls: Exp*) //{ require(!ls.isEmpty, "so it doesn't override unit") }

    override type Constructed = Prod

    def toExp(p: Prod): Exp = {

      def rec(ls: List[Exp])(out: Var): Exp = ls match {
        case List()       => out
        case head :: tail => ArrowConstruction.toExp(ArrowConstruction.Arrow(head.open(), rec(tail)(out)))
      }

      Pi(Prop(), Pi(rec(p.ls.toList)(Var(0)), Var(1)))
    }

    def fromExp(e: Exp): Option[Constructed] =
      e match {
        case Pi(Prop(), Pi(bod, Var(1))) => {

          import ArrowConstruction.Arrow

          def extract(e: Exp)(v: Var): Option[List[Exp]] = e match {
            case Arrow(from, to) if !from.freeVars.contains(v) => for { ls <- extract(to)(v) } yield from.sub(Var(0)) :: ls
            case _ if e == v                                   => Some(List())
            case _ if e != v                                   => None
          }

          extract(bod)(Var(0)).map(ls => Prod((ls.toSeq): _*))
        }
        case _ => None
      }

    def pettyPrint(pr: Prod, ctx: PrintCtx, prt: Printer)(config: ConstructionsConfig): (Set[String], PartialOutput) = {
      var tempctx = ctx
      val outseq = for (exp <- pr.ls) yield { //yea I know should be a fold

        val (s, out) = prt(exp, tempctx)(config)

        tempctx = (s, tempctx._2)

        showApp(out)
      }

      (tempctx._1, AppList(outseq.mkString(" × ")))
    }

    def parser(ctx: List[String])(config: ConstructionsConfig)(restParser: CcConstructionsParser)(parseLessPrecidence: restParser.Parser[Exp]): restParser.Parser[Prod] = {
      import restParser._

      (parseLessPrecidence) ~ ("×" ~> parseLessPrecidence).+ ^^ { case head ~ tail => Prod((head :: tail): _*) }
    }

    //TODO: also intro and elim constructions?
  }

  //TODO: bot, neg, nproducts, nsum, eq, church bools, church num, ...

  //how to deal with "implicit" type level information

}