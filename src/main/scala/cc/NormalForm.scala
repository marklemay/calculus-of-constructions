package cc

import cc.Cc.Exp

object NormalForm {

  // see: https://cs.stackexchange.com/questions/69434/intuitive-explanation-of-neutral-normal-form-in-lambda-calculus
  // TODO it would be more efficient for these to be lists...

  //  sealed trait Reduced

  sealed trait Normal
  case class Lam(ty: TyNormal, bod: Normal) extends Normal

  sealed trait TyNormal extends Normal
  case class Prop() extends TyNormal
  case class Typ() extends TyNormal
  case class Pi(ty: TyNormal, bod: TyNormal) extends TyNormal

  sealed trait Neutral extends Normal with TyNormal
  case class Var(v: Int) extends Neutral { require(v >= 0) }
  case class App(f: Neutral, a: Normal) extends Neutral

  def toNomal(exp: Exp): Option[Normal] = exp match {
    case Cc.Lam(ty, bod) => for {
      ty <- toTyNomal(ty)
      bod <- toNomal(bod)
    } yield Lam(ty, bod)
    case exp => toTyNomal(exp)
  }

  def toTyNomal(exp: Exp): Option[TyNormal] = exp match {
    case Cc.Prop() => Some(Prop())
    case Cc.Typ()  => Some(Typ())
    case Cc.Pi(ty, bod) => for {
      ty <- toTyNomal(ty)
      bod <- toTyNomal(bod)
    } yield Pi(ty, bod)

    case exp => toNeutral(exp)
  }

  def toNeutral(exp: Exp): Option[Neutral] = exp match {
    case Cc.Var(v) => Some(Var(v))
    case Cc.App(f, a) => for {
      f <- toNeutral(f)
      a <- toNomal(a)
    } yield App(f, a)

    case _ => None
  }

  def toExp(n: Normal): Exp = n match {
    case Prop()       => Cc.Prop()
    case Typ()        => Cc.Typ()
    case Pi(ty, bod)  => Cc.Pi(toExp(ty), toExp(bod))
    case Lam(ty, bod) => Cc.Lam(toExp(ty), toExp(bod))
    case Var(v)       => Cc.Var(v)
    case App(f, a)    => Cc.App(toExp(f), toExp(a))
  }
  //  def toExp(n: Normal): Exp = exp match {

  //
  //  case class Prop() extends TyNormal
  //  case class Typ() extends TyNormal
  //  //  case class JustTyNeutral(n: TyNeutral) extends TyNormal
  //  case class Pi(ty: TyNormal, bod: TyNormal) extends TyNormal
  //
  //  sealed trait TyNeutral extends TyNormal
  //  case class TyVar(v: Int) extends TyNeutral { require(v >= 0) }
  //  case class TyApp(f: TyNormal, a: TyNeutral) extends TyNeutral //TODO: this A should be a regular Neutral

  //  case class Prop() extends Ty
  //  case class Typ() extends Ty
  //  case class Pi(ty: Ty, bod: Ty) extends Ty
  //
  //
  //  sealed trait Normal extends Reduced
  //  case class JustNeutral( extends Normal
  //  case class JustTy(n: Ty) extends Normal
  //  case class Lam(ty: Ty, bod: Normal) extends Normal
  //
  //  //  case class Value()
  //
  //  sealed trait Neutral extends Reduced
  //  case class Var(v: Int) extends Neutral with TyNeutral { require(v >= 0) }
  //  case class App(f: Neutral, a: Normal) extends Neutral

  //  def toNormalForm(exp: Exp): Reduced = exp.norm match {
  //    case Cc.Prop()   => Prop()
  //    case Cc.Typ()    => Typ()
  //    case Cc.Pi(ty, bod) => Pi(toExp(ty), toExp(bod))
  //    //    case JustNeutral(n) => toExp(n)
  //    //    case JustTy(n)      => toExp(n)
  //    //    case Lam(ty, bod)   => Cc.Lam(toExp(ty), toExp(bod))
  //    //    case Var(v)         => Cc.Var(v)
  //    //    case App(f, a)      => Cc.App(toExp(f), toExp(a))
  //
  //  }

  //  def toTyNormal(exp: Exp): Option[TyNormal] = exp.norm match {
  //    case Cc.Prop() => Some(Prop())
  //    case Cc.Typ()  => Some(Typ())
  //    case Cc.Pi(ty, bod) => for {
  //      ty <- toTyNormal(ty)
  //      bod <- toTyNormal(bod)
  //    } yield Pi(ty, bod)
  //    //    //    case JustNeutral(n) => toExp(n)
  //    //    //    case JustTy(n)      => toExp(n)
  //    //    //    case Lam(ty, bod)   => Cc.Lam(toExp(ty), toExp(bod))
  //    //    //    case Var(v)         => Cc.Var(v)
  //    //    //    case App(f, a)      => Cc.App(toExp(f), toExp(a))
  //    //
  //  }
  //
  //  def toTyNeutral(exp: Exp): Option[TyNeutral] = exp.norm match {
  //    case Cc.Var(v) => Some(TyVar(v))
  //    case Cc.App(f, a) => for {
  //      ty <- toTyNormal(f)
  //      bod <- toTyNormal(a)
  //    } yield Pi(ty, bod)
  //    //    case Cc.Prop() => Some(Prop())
  //    //    case Cc.Typ()  => Some(Typ())
  //    //    case Cc.Pi(ty, bod) => for {
  //    //      ty <- toTy(ty)
  //    //      bod <- toTy(bod)
  //    //    } yield Pi(ty, bod)
  //    //    //    case JustNeutral(n) => toExp(n)
  //    //    //    case JustTy(n)      => toExp(n)
  //    //    //    case Lam(ty, bod)   => Cc.Lam(toExp(ty), toExp(bod))
  //    //    //    case App(f, a)      => Cc.App(toExp(f), toExp(a))
  //    //
  //  }

  //  def toExp(r: Reduced): Exp = r match {
  //    case Prop()         => Cc.Prop()
  //    case Typ()          => Cc.Typ()
  //    case Pi(ty, bod)    => Cc.Pi(toExp(ty), toExp(bod))
  //    case JustNeutral(n) => toExp(n)
  //    case JustTy(n)      => toExp(n)
  //    case Lam(ty, bod)   => Cc.Lam(toExp(ty), toExp(bod))
  //    case Var(v)         => Cc.Var(v)
  //    case App(f, a)      => Cc.App(toExp(f), toExp(a))
  //  }

  //  sealed trait Reduced

  //  sealed trait TyNormal extends Reduced
  //
  //  case class Prop() extends TyNormal
  //  case class Typ() extends TyNormal
  //  //  case class JustTyNeutral(n: TyNeutral) extends TyNormal
  //  case class Pi(ty: TyNormal, bod: TyNormal) extends TyNormal
  //
  //  sealed trait TyNeutral extends TyNormal
  //  case class TyVar(v: Int) extends TyNeutral { require(v >= 0) }
  //  case class TyApp(f: TyNormal, a: TyNeutral) extends TyNeutral //TODO: this A should be a regular Neutral

  //  case class Prop() extends Ty
  //  case class Typ() extends Ty
  //  case class Pi(ty: Ty, bod: Ty) extends Ty
  //
  //
  //  sealed trait Normal extends Reduced
  //  case class JustNeutral( extends Normal
  //  case class JustTy(n: Ty) extends Normal
  //  case class Lam(ty: Ty, bod: Normal) extends Normal
  //
  //  //  case class Value()
  //
  //  sealed trait Neutral extends Reduced
  //  case class Var(v: Int) extends Neutral with TyNeutral { require(v >= 0) }
  //  case class App(f: Neutral, a: Normal) extends Neutral

}