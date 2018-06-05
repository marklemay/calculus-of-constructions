package cc

import cc.Cc._

// additional features that are nice to have but are not needed for the core normalization and type checking
object CcProperties {
  //TODO: can do some implicit magic so that it looks like these are members of Exp without cluttering the definition

  def removeBinder(vs: Set[Var]): Set[Var] = vs.filter(_.v != 0).map(va => Var(va.v - 1))

  def freeVars(e: Exp): Set[Var] = e match {
    case Var(i)       => Set(Var(i))

    case Prop()       => Set()
    case Typ()        => Set()

    case Lam(ty, bod) => freeVars(ty) ++ removeBinder(freeVars(bod))
    case Pi(ty, bod)  => freeVars(ty) ++ removeBinder(freeVars(bod))

    case App(f, a)    => freeVars(f) ++ freeVars(a)
  }

  def replaceVar(e: Exp, here: Var, withThis: Exp): Exp = e match {
    case v: Var if v == here => withThis
    case v: Var if v != here => v

    case Prop()              => Prop()
    case Typ()               => Typ()

    case Lam(ty, bod)        => Lam(replaceVar(ty, here, withThis), replaceVar(bod, Var(here.v + 1), withThis.open()))
    case Pi(ty, bod)         => Pi(replaceVar(ty, here, withThis), replaceVar(bod, Var(here.v + 1), withThis.open()))

    case App(f, a)           => App(replaceVar(f, here, withThis), replaceVar(a, here, withThis))
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

  def smallStep(e: Exp): Exp = e match {
    case Var(i)                   => Var(i)

    case Prop()                   => Prop()
    case Typ()                    => Typ()

    case Lam(ty, bod)             => Lam(smallStep(ty), smallStep(bod))
    case Pi(ty, bod)              => Pi(smallStep(ty), smallStep(bod))

    case App(Lam(_, bod), a)      => bod.sub(a)
    case App(f, a) if !isValue(f) => App(smallStep(f), a)
    case App(f, a) if isValue(f)  => App(f, smallStep(a))
  }

  def isValue(e: Exp): Boolean = e match {
    case Var(i)              => true

    case Prop()              => true
    case Typ()               => true

    case Lam(ty, bod)        => isValue(ty) && isValue(bod)
    case Pi(ty, bod)         => isValue(ty) && isValue(bod)

    case App(Lam(_, bod), a) => false
    case App(f, a)           => isValue(f) && isValue(a)
  }
}