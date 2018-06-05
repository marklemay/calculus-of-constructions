package cc_with_constructions

import cc.Cc.Exp
import cc.Cc.Pi

import cc.CcProperties._
import cc.Cc.Var
import cc.CcConstructionsPrettyPrinter._
import cc.Cc.Prop

//TODO: figure out what should be implicit

//somthing that looks and acts like a Cc expression
object Constructions { //TODO: paramiterixe these over generalized epressions?  expresion functors?

  trait Construction {

    //    def form(se: Seq[Exp]): Constructed

    type Constructed

    def toExp(c: Constructed): Exp
    def fromExp(e: Exp): Option[Constructed]

    def pettyPrint(c: Constructed, ctx: PrintCtx, p: Printer)(config: ConstructionsConfig): (Set[String], PartialOutput) // (implicit config: ConstructionsConfig)

    def parser(ctx: List[String])(config: ConstructionsConfig)(restParser: CcConstructionsParser): restParser.Parser[Constructed]

    //TODO a little funny
    def parserExp(ctx: List[String])(config: ConstructionsConfig)(restParser: CcConstructionsParser) = parser(ctx)(config)(restParser).map(toExp)
    //CcConstructionsParser
  }

  //non depndent function
  case object ArrowConstruction extends Construction {

    case class Arrow(ty: Exp, bod: Exp)

    override type Constructed = Arrow

    def toExp(a: Arrow): Exp = {
      val Arrow(ty, bod) = a
      Pi(ty, bod.open())
    }

    def fromExp(e: Exp): Option[Constructed] = e match {
      case Pi(ty, bod) if !freeVars(bod).contains(Var(0)) => Some(Arrow(ty, bod.sub(Var(0))))
      case _ => None
    }

    def pettyPrint(c: Arrow, ctx: PrintCtx, p: Printer)(config: ConstructionsConfig): (Set[String], PartialOutput) = {
      //The best pretty printer would accumulate all the arrows... and then make at the end
      val Arrow(ty, bod) = c
      val (vars1, prettyTy) = p(ty, ctx)(config)
      val (vars2, prettyBod) = p(bod, (vars1, ctx._2))(config)

      (vars2, AppList(s"${showApp(prettyTy)} → ${showTy(prettyBod)}"))
    }

    //TODO: needs to have both the ful config and the partial config
    def parser(ctx: List[String])(config: ConstructionsConfig)(restParser: CcConstructionsParser): restParser.Parser[Arrow] = {
      import restParser._

      (parseConstruction(config.configs.toList.filter(_ != ArrowConstruction))(ctx)(config) ~ ("→" ~> subExp(ctx)(config))) ^^ { case (in ~ out) => Arrow(in, out) }
    }
  }

  //  case object BinderConstruction extends Construction {
  //
  //    case class Binder(tys: List[Exp], bod: Exp)
  //
  //    override type Constructed = Binder
  //
  //    def toExp(b: Binder): Exp = b match {
  //      case Binder(List(), bod)       => bod
  //      case Binder(head :: tail, bod) => Pi(head, toExp(Binder(tail, bod)).open())
  //    }
  //
  //    def fromExp(e: Exp): Option[Constructed] = Some(e match {
  //      case Pi(ty, bod) if  => Some(Arrow(ty, bod.sub(Var(0))))
  //      case exp => Binder(List(), exp)
  //    })
  //
  //    def pettyPrint(c: Binder, ctx: PrintCtx, p: Printer)(config: ConstructionsConfig): (Set[String], PartialOutput) = ???
  //    //    {
  //    //      //The best pretty printer would accumulate all the arrows... and then make at the end
  //    //      val Arrow(ty, bod) = c
  //    //      val (vars1, prettyTy) = p(ty, ctx)(config)
  //    //      val (vars2, prettyBod) = p(bod, (vars1, ctx._2))(config)
  //    //
  //    //      (vars2, AppList(s"${showApp(prettyTy)} → ${showTy(prettyBod)}"))
  //    //    }
  //
  //  }

  case object ProdConstruction extends Construction {

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

          def extract(e: Exp)(v: Var): Option[List[Exp]] = ArrowConstruction.fromExp(e) match {
            case Some(ArrowConstruction.Arrow(from, to)) if !freeVars(from).contains(v) => for { ls <- extract(to)(v) } yield from.sub(Var(0)) :: ls
            case None if e == v => Some(List())
            case None if e != v => None
          }

          extract(bod)(Var(0)) match {
            case Some(ls) => Some(Prod((ls.toSeq): _*))
            case None     => None
          }
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

    def parser(ctx: List[String])(config: ConstructionsConfig)(restParser: CcConstructionsParser): restParser.Parser[Prod] = {
      import restParser._

      def subProd(ctx: List[String])(config: ConstructionsConfig): Parser[List[Exp]] = ???

      //      (subExp(ctx)(config) ~ ("→" ~> subExp(ctx)(config))) ^^ { case (in ~ out) => Arrow(in, out) }

      ???
    }

    //

    //    def form(ty: Exp, bod: Exp) = Arrow

    //  def Parse: ((c: _1.Product)String) forSome { val _1: cc_with_constructions.Constructions.PruductConstruction } = ???
    //    def form(se: Seq[cc.Cc.Exp]): cc_with_constructions.Constructions.PruductConstruction#Product = ???
    //    def fromExp(e: cc.Cc.Exp): Option[cc_with_constructions.Constructions.PruductConstruction#Product] = ???
    //  def pettyPrint: ((c: _1.Product)String) forSome { val _1: cc_with_constructions.Constructions.PruductConstruction } = ???
    //  def toExp: ((c: _1.Product)cc.Cc.Exp) forSome { val _1: cc_with_constructions.Constructions.PruductConstruction } = ???
  }

  //  case object PruductConstruction extends Construction {
  //
  //    case class Product(ls: List[Exp])
  //
  //    override type Constructed = Product
  //
  //    def toExp(prod: Product): Exp = {
  //
  //      ???
  //    }
  //
  //    //  def Parse: ((c: _1.Product)String) forSome { val _1: cc_with_constructions.Constructions.PruductConstruction } = ???
  //    //    def form(se: Seq[cc.Cc.Exp]): cc_with_constructions.Constructions.PruductConstruction#Product = ???
  //    //    def fromExp(e: cc.Cc.Exp): Option[cc_with_constructions.Constructions.PruductConstruction#Product] = ???
  //    //  def pettyPrint: ((c: _1.Product)String) forSome { val _1: cc_with_constructions.Constructions.PruductConstruction } = ???
  //    //  def toExp: ((c: _1.Product)cc.Cc.Exp) forSome { val _1: cc_with_constructions.Constructions.PruductConstruction } = ???
  //  }

  //TODO: neg, bot, nproducts, nsum, eq, church bools, church num, ...

  //how to deal with "implicit" type level information

}