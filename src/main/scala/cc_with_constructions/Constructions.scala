package cc_with_constructions

import cc.Cc.Exp
import cc.Cc.Pi

import cc.CcProperties._
import cc.Cc.Var
import cc.CcConstructionsPrettyPrinter._

//TODO: figure out what should be implicit

//somthing that looks and acts like a Cc expression
object Constructions {

  trait Construction {

    //    def form(se: Seq[Exp]): Constructed

    type Constructed

    def toExp(c: Constructed): Exp
    def fromExp(e: Exp): Option[Constructed]

    def pettyPrint(c: Constructed, ctx: PrintCtx, p: Printer)(config: ConstructionsConfig): (Set[String], PartialOutput) // (implicit config: ConstructionsConfig)
    //
    //    def Parse(c: Constructed): String

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

  //TODO: neg, arrow, nproducts, nsum, eq, ...

  //how to deal with "implicit" type level information

}