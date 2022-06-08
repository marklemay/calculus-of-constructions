package cc

import cc.Cc._
import cc_with_constructions.ConstructionsConfig

// additional features that are nice to have but are not needed for the core normalization and type checking
object CcProperties {

  implicit def expToRichExp(exp: Exp): RichExp = new RichExp(exp)

  class RichExp(exp: Exp) {

    private def removeBinder(vs: Set[Var]): Set[Var] = vs.filter(_.v != 0).map(va => Var(va.v - 1))

    def freeVars: Set[Var] = exp match {
      case Var(i)       => Set(Var(i))

      case Prop()       => Set()
      case Typ()        => Set()

      case Lam(ty, bod) => ty.freeVars ++ removeBinder(bod.freeVars)
      case Pi(ty, bod)  => ty.freeVars ++ removeBinder(bod.freeVars)

      case App(f, a)    => f.freeVars ++ a.freeVars
    }

    def replaceVar(here: Var, withThis: Exp): Exp = exp match {
      case v: Var if v == here => withThis
      case v: Var if v != here => v

      case Prop()              => Prop()
      case Typ()               => Typ()

      case Lam(ty, bod)        => Lam(ty.replaceVar(here, withThis), bod.replaceVar(Var(here.v + 1), withThis.open()))
      case Pi(ty, bod)         => Pi(ty.replaceVar(here, withThis), bod.replaceVar(Var(here.v + 1), withThis.open()))

      case App(f, a)           => App(f.replaceVar(here, withThis), a.replaceVar(here, withThis))
    }

    def smallStep: Exp = exp match {
      case Var(i)                  => Var(i)

      case Prop()                  => Prop()
      case Typ()                   => Typ()

      case Lam(ty, bod)            => Lam(ty.smallStep, bod.smallStep)
      case Pi(ty, bod)             => Pi(ty.smallStep, bod.smallStep)

      case App(Lam(_, bod), a)     => bod.sub(a)
      case App(f, a) if !f.isValue => App(f.smallStep, a)
      case App(f, a) if f.isValue  => App(f, a.smallStep)
    }

    def isValue: Boolean = exp match {
      case Var(i)              => true

      case Prop()              => true
      case Typ()               => true

      case Lam(ty, bod)        => ty.isValue && bod.isValue
      case Pi(ty, bod)         => ty.isValue && bod.isValue

      case App(Lam(_, bod), a) => false
      case App(f, a)           => f.isValue && a.isValue
    }

    def isType: Boolean = exp.ty(List()) match {
      case Some(x) => x.isSort
      case None    => false
    }

    //can't do anything cleaner with toString
    def show(implicit config: ConstructionsConfig = ConstructionsConfig()): String = CcConstructionsPrettyPrinter.printer(exp, Set(), List())(config)._2.s

    def toScala: String = exp match {
      case Var(i)       => s"Var($i)"

      case Prop()       => s"Prop()"
      case Typ()        => s"Typ()"

      case Lam(ty, bod) => s"Lam(${ty.toScala}, ${bod.toScala})"
      case Pi(ty, bod)  => s"Pi(${ty.toScala}, ${bod.toScala})"

      case App(f, a)    => s"Pi(${f.toScala}, ${a.toScala})"
    }

    // TODO: should be coerce as lazily as possible to avoid having to deal with term irelevent things
    //TODO: implicit interpertation of prop, type and Pi?
    //TODO: better error reporting?
    //TODO: allow constuctions to have a natural scala interpertation
    /** a cheesy way to turn a syntacitc term into a runnable Scala function */
    def asObject[O]: O = {

      def rec(e: Exp, ctx: List[Any]): Any = e match {
        case Prop()      => ???
        case Typ()       => ???
        case Pi(ty, bod) => ???

        case Var(i)      => ctx(i)

        case Lam(_, bod) => { (x: Any) => rec(bod, x :: ctx) }

        case App(f, a) => {
          val ff = rec(f, ctx).asInstanceOf[Any => Any]
          ff(rec(a, ctx))
        }
      }

      rec(exp, List()).asInstanceOf[O]
    }

    //TODO: another nice exercise would be to write an eta normal form
  }

  object Appls {
    def unapply(e: Exp): Option[List[Exp]] = e match {
      case App(f, a) => {
        unapply(f) match {
          case None     => Some(List(f, a))
          case Some(ls) => Some(ls ++ List(a))
        }
      }
      case _ => None
    }

    //    def apply(ls: List[Exp])
  }

}