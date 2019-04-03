package cc

import cc.Cc._
import CcProperties._
import cc.CcProperties.Appls

object CcPrettyPrinter {

  def paren(outer: Int)(inner: Int)(s: String) = if (outer < inner) { "(" ++ s ++ ")" } else s

  //TODO: rewrite monadicly

  def prettyShow(e: Exp)(i: Int)(allVars: Set[String])(nameCtx: List[String]): (Set[String], String) = e match {
    case Prop()                     => (allVars, s"●")
    case Typ()                      => (allVars, s"□")
    case Var(i) if i < nameCtx.size => (allVars, nameCtx(i))
    case Var(i)                     => (allVars, (i - nameCtx.size).toString() ++ "?")

    case App(f, a) => {
      val (allVars2, fs) = prettyShow(f)(1)(allVars)(nameCtx)
      val (allVars3, as) = prettyShow(a)(0)(allVars)(nameCtx)
      (allVars3, paren(i)(1)(s"$fs $as"))
    }
    case Lam(t, bod) => {

      val x = if (bod.freeVars.contains(Var(0))) {
        freshVar(allVars, nameCtx, t)
      } else {
        "_"
      }

      val allVars2 = (allVars ++ Set(x))

      val (allVars3, ts) = prettyShow(t)(4)(allVars2)(nameCtx)

      val (allVars4, bods) = prettyShow(bod)(5)(allVars2)(x :: nameCtx)
      (allVars4, paren(i)(5)(s"λ $x : $ts . $bods"))
    }
    case Pi(t, bod) => {

      if (bod.freeVars.contains(Var(0))) {
        val x = freshVar(allVars, nameCtx, t)

        val allVars2 = (allVars ++ Set(x))

        val (allVars3, ts) = prettyShow(t)(4)(allVars2)(nameCtx)

        val (allVars4, bods) = prettyShow(bod)(5)(allVars2)(x :: nameCtx)
        (allVars4, paren(i)(5)(s"Π $x : $ts . $bods"))
      } else {
        val x = "_"
        val allVars2 = (allVars ++ Set(x))

        val (allVars3, ts) = prettyShow(t)(2)(allVars2)(nameCtx)

        val (allVars4, bods) = prettyShow(bod)(3)(allVars2)(x :: nameCtx)
        (allVars4, paren(i)(3)(s"$ts → $bods"))
      }

    }
  }

  def showFullyParen(e: Exp)(allVars: Set[String])(nameCtx: List[String]): (Set[String], String) = e match {
    case Prop()                     => (allVars, s"●")
    case Typ()                      => (allVars, s"□")
    case Var(i) if i < nameCtx.size => (allVars, nameCtx(i))
    case Var(i)                     => (allVars, (i - nameCtx.size).toString() ++ "?")

    case App(f, a) => {
      val (allVars2, fs) = showFullyParen(f)(allVars)(nameCtx)
      val (allVars3, as) = showFullyParen(a)(allVars)(nameCtx)
      (allVars3, (s"($fs $as)"))
    }
    case Lam(t, bod) => {

      val x = if (bod.freeVars.contains(Var(0))) {
        freshVar(allVars, nameCtx, t)
      } else {
        "_"
      }

      val allVars2 = (allVars ++ Set(x))

      val (allVars3, ts) = showFullyParen(t)(allVars2)(nameCtx)

      val (allVars4, bods) = showFullyParen(bod)(allVars2)(x :: nameCtx)
      (allVars4, (s"(λ $x : $ts . $bods)"))
    }
    case Pi(t, bod) => {

      if (bod.freeVars.contains(Var(0))) {
        val x = freshVar(allVars, nameCtx, t)

        val allVars2 = (allVars ++ Set(x))

        val (allVars3, ts) = showFullyParen(t)(allVars2)(nameCtx)

        val (allVars4, bods) = showFullyParen(bod)(allVars2)(x :: nameCtx)
        (allVars4, (s"(Π $x : $ts . $bods)"))
      } else {
        val x = "_"
        val allVars2 = (allVars ++ Set(x))

        val (allVars3, ts) = showFullyParen(t)(allVars2)(nameCtx)

        val (allVars4, bods) = showFullyParen(bod)(allVars2)(x :: nameCtx)
        (allVars4, (s"($ts → $bods)"))
      }

    }
  }

  // ignore bellow

  //TODO: this is a bit of a mess
  //TODO: can give vars better names if they have atomic type
  sealed trait PartialOutput {
    val s: String
  }

  //todo: add meta data?
  case class Atom(s: String) extends PartialOutput

  case class AppList(s: String) extends PartialOutput
  case class Outer(s: String) extends PartialOutput

  //TODO: put in class
  def showTy(out: PartialOutput): String = out match {
    case Atom(s)    => s"$s"
    case Outer(s)   => s"[$s]"
    case AppList(s) => s"$s"
  }

  def showBod(out: PartialOutput): String = out match {
    case Atom(s)    => s"$s"
    case Outer(s)   => s"$s"
    case AppList(s) => s"($s)" //TODO: unneeded?
  }

  def showApp(out: PartialOutput): String = out match {
    case Atom(s)    => s"$s"
    case Outer(s)   => s"[$s]"
    case AppList(s) => s"($s)"
  }

  //TODO: does scala have a state monad? if so this should be rewritten with that
  def Printer(e: Exp, allVars: Set[String], nameCtx: List[String]): (Set[String], PartialOutput) = e match {
    case Prop()                     => (allVars, Atom(s"●"))
    case Typ()                      => (allVars, Atom(s"□"))

    case Var(i) if i < nameCtx.size => (allVars, Atom(nameCtx(i)))
    case Var(i)                     => (allVars, Atom(s"${i - nameCtx.size}?"))

    case Lam(ty, bod) => {
      val newvar = freshVar(allVars, nameCtx, ty)
      val vars1 = allVars + newvar

      val (vars2, prettyTy) = Printer(ty, vars1, nameCtx)
      val (vars3, prettyBod) = Printer(bod, vars2, newvar :: nameCtx)

      (vars3, Outer(s"λ $newvar : ${showTy(prettyTy)} . ${showBod(prettyBod)}"))
    }

    case Pi(ty, bod) => {
      val newvar = freshVar(allVars, nameCtx, ty)
      val vars1 = allVars + newvar

      val (vars2, prettyTy) = Printer(ty, vars1, nameCtx)
      val (vars3, prettyBod) = Printer(bod, vars2, newvar :: nameCtx)

      (vars3, Outer(s"Π $newvar : ${showTy(prettyTy)} . ${showBod(prettyBod)}"))
    }

    case Appls(ls) => {
      var temp = allVars

      val out: List[PartialOutput] = for (e <- ls) yield {
        val (t, part) = Printer(e, temp, nameCtx)
        temp = t
        part
      }

      (temp, AppList(out.map(showApp).mkString(" ")))
    }
  }

  // rename: freshname TODO: also monadic
  def freshVar(printCtx: Set[String], nameCtx: List[String], ty: Exp): String = ty match {
    case Var(v) if v < nameCtx.size => freshVar(printCtx, nameCtx(v))
    case _                          => freshVar(printCtx)
  }

  def freshVar(printCtx: Set[String], tyName: String): String = {
    val base = tyName.toLowerCase()

    if (!printCtx.contains(base.toString())) {
      base
    } else {
      var tmp = 1
      while (printCtx.contains(base + tmp)) {
        tmp += 1
      }
      base + tmp
    }
  }

  def freshVar(printCtx: Set[String]): String = {

    freeVars.filter(!printCtx.contains(_)).head
  }

  val freeVars: Stream[String] = ('A' to 'Z').map(_.toString()).toStream append freeVars.map(_ ++ "'")

  def freshVarFromType(printCtx: Set[String], ty: String): String = {
    val base = ty.toLowerCase()

    if (!printCtx.contains(base)) {
      base
    } else {

      var tmp = 1

      while (printCtx.contains(base + tmp.toString())) {
        tmp += 1
      }
      base + tmp.toString()
    }
  }

}