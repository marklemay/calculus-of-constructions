package cc

import cc.Cc._

object CcPrettyPrinter {
  //TODO: this is a bit of a mess
  //TODO: should be more sparing with the brackets
  //TODO: can give vars better names if they have atomic type

  sealed trait PartialOutput {
    val s: String
  }

  //todo: add meta data?
  case class Atom(s: String) extends PartialOutput

  case class AppList(s: String) extends PartialOutput
  case class Outer(s: String) extends PartialOutput

  //TODO: put in class
  def showClosed(out: PartialOutput): String = out match {
    case Atom(s)    => s"$s"
    case Outer(s)   => s"[$s]"
    case AppList(s) => s"$s"
  }

  def showOpen(out: PartialOutput): String = out match {
    case Atom(s)    => s"$s"
    case Outer(s)   => s"[$s]"
    case AppList(s) => s"($s)"
  }

  object Apps {
    def unapply(e: Exp): Option[List[Exp]] = e match {
      case App(f, a) => {
        unapply(f) match {
          case None     => Some(List(f, a))
          case Some(ls) => Some(ls ++ List(a))
        }
      }
      case _ => None
    }
  }

  //TODO: does scala have a state monad? if so this should be rewritten with that
  def Printer(e: Exp, allVars: Set[String], nameCtx: List[String]): (Set[String], PartialOutput) = e match {
    case Prop()                     => (allVars, Atom(s"●"))
    case Typ()                      => (allVars, Atom(s"□"))

    case Var(i) if i < nameCtx.size => (allVars, Atom(nameCtx(i)))
    case Var(i)                     => (allVars, Atom(s"$i?"))

    case Lam(ty, bod) => {
      val newvar = freshVar(allVars)
      val vars1 = allVars + newvar

      val (vars2, prettyTy) = Printer(ty, vars1, nameCtx)
      val (vars3, prettyBod) = Printer(bod, vars2, newvar :: nameCtx)

      (vars3, Outer(s"λ $newvar : ${showClosed(prettyTy)} . ${showOpen(prettyBod)}"))
    }

    case Pi(ty, bod) => {
      val newvar = freshVar(allVars)
      val vars1 = allVars + newvar

      val (vars2, prettyTy) = Printer(ty, vars1, nameCtx)
      val (vars3, prettyBod) = Printer(bod, vars2, newvar :: nameCtx)

      (vars3, Outer(s"Π $newvar : ${showClosed(prettyTy)} . ${showOpen(prettyBod)}"))
    }

    case Apps(ls) => {
      var temp = allVars

      val out: List[PartialOutput] = for (e <- ls) yield {
        val (t, part) = Printer(e, temp, nameCtx)
        temp = t
        part
      }

      (temp, AppList(out.map(showOpen).mkString(" ")))
    }
  }

  def freshVar(printCtx: Set[String]): String = {
    var tmp = 'A'

    while (printCtx.contains(tmp.toString())) {
      tmp = (tmp.toInt + 1).toChar
    }
    tmp.toString()
  }

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