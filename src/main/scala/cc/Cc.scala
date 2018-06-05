package cc

object Cc {
  case class Prop() extends Exp
  case class Typ() extends Exp

  case class Var(v: Int) extends Exp { require(v >= 0) }

  case class Lam(ty: Exp, bod: Exp) extends Exp
  case class Pi(ty: Exp, bod: Exp) extends Exp

  case class App(f: Exp, arg: Exp) extends Exp

  sealed trait Exp {

    def sub(withThis: Exp, under: Int = 0): Exp = {
      require(under >= 0)
      this match {
        case Var(i) if i == under => withThis
        case Var(i) if i > under  => Var(i - 1) // "global"
        case Var(i) if i < under  => Var(i) // "local"

        case Prop()               => Prop()
        case Typ()                => Typ()

        case Lam(ty, bod)         => Lam(ty.sub(withThis, under), bod.sub(withThis.open(under), under + 1))
        case Pi(ty, bod)          => Pi(ty.sub(withThis, under), bod.sub(withThis.open(under), under + 1))

        case App(f, a)            => App(f.sub(withThis, under), a.sub(withThis, under))
      }
    }

    def open(under: Int = 0): Exp = this match {
      case Var(v) if v < under => Var(v)
      case Var(v)              => Var(v + 1)

      case Prop()              => Prop()
      case Typ()               => Typ()

      case Lam(ty, bod)        => Lam(ty.open(under), bod.open(under + 1))
      case Pi(ty, bod)         => Pi(ty.open(under), bod.open(under + 1))

      case App(f, a)           => App(f.open(under), a.open(under))
    }

    val isSort = this match {
      case Prop() => true
      case Typ()  => true
      case _      => false
    }

    lazy val norm: Exp = this match {
      case Var(v)       => Var(v)
      case Prop()       => Prop()
      case Typ()        => Typ()

      case Lam(ty, bod) => Lam(ty.norm, bod.norm)
      case Pi(ty, bod)  => Pi(ty.norm, bod.norm)

      case App(f, a) => (f.norm, a.norm) match {
        case (Lam(_, bod), aNorm) => bod.sub(aNorm).norm
        case (fNorm, aNorm)       => App(fNorm, aNorm)
      }
    }

    def ty(ctx: List[Exp] = List()): Option[Exp] = this match {
      case Var(v) if v < ctx.size => Some(ctx(v))
      case Prop()                 => Some(Typ())

      case Lam(aty, bod) => for {
        aTyTy <- aty.ty(ctx)
        if aTyTy.isSort
        bodTy <- bod.ty((aty :: ctx).map(_.open()))
        fullTy = Pi(aty.norm, bodTy.norm)
        fullTyTy <- fullTy.ty(ctx)
        if fullTyTy.isSort
      } yield fullTy

      case Pi(aty, bod) => for {
        aTyTy <- aty.ty(ctx)
        if aTyTy.isSort
        bodTy <- bod.ty((aty :: ctx).map(_.open()))
        if bodTy.isSort
      } yield bodTy

      case App(f, a) => for {
        Pi(aTy1, bodTy) <- f.ty(ctx).map(_.norm) //TODO: break to cleaner multi line?
        aTy2 <- a.ty(ctx)
        if aTy1.norm == aTy2.norm
      } yield bodTy.sub(a).norm

      case _ => None
    }

    override def toString(): String = CcPrettyPrinter.Printer(this, Set(), List())._2.s
  }

}