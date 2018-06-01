package cc

import cc.Cc._
import cc.CcProperties.Appls
import cc_with_constructions.ConstructionsConfig


//TOOD: index the whole class by the configuration?
object CcConstructionsPrettyPrinter {
  //TODO: this is a bit of a mess
  //TODO: can give vars better names if they have atomic type

  type PrintCtx = (Set[String], List[String])
  type Printer = (Exp, PrintCtx) => (ConstructionsConfig => (Set[String], PartialOutput))

  //TODO: does scala have a state monad? if so this should be rewritten with that
  //TOOD: make config implicit
  def printer(e: Exp, allVars: Set[String], nameCtx: List[String])(config: ConstructionsConfig): (Set[String], PartialOutput) = {

    for (c <- config.configs) {
      c.fromExp(e) match {
        case Some(cons) => return c.pettyPrint(cons, (allVars, nameCtx), { (e: Exp, p: PrintCtx) => printer(e, p._1, p._2)(_) })(config) //printer)
        case _          => ()
      }
    }

    e match {
      case Prop()                     => (allVars, Atom(s"●"))
      case Typ()                      => (allVars, Atom(s"□"))

      case Var(i) if i < nameCtx.size => (allVars, Atom(nameCtx(i)))
      case Var(i)                     => (allVars, Atom(s"$i?"))

      case Lam(ty, bod) => {
        val newvar = freshVar(allVars)
        val vars1 = allVars + newvar

        val (vars2, prettyTy) = printer(ty, vars1, nameCtx)(config)
        val (vars3, prettyBod) = printer(bod, vars2, newvar :: nameCtx)(config)

        (vars3, Outer(s"λ $newvar : ${showTy(prettyTy)} . ${showBod(prettyBod)}"))
      }

      case Pi(ty, bod) => {
        val newvar = freshVar(allVars)
        val vars1 = allVars + newvar

        val (vars2, prettyTy) = printer(ty, vars1, nameCtx)(config)
        val (vars3, prettyBod) = printer(bod, vars2, newvar :: nameCtx)(config)

        (vars3, Outer(s"Π $newvar : ${showTy(prettyTy)} . ${showBod(prettyBod)}"))
      }

      case Appls(ls) => {
        var temp = allVars

        val out: List[PartialOutput] = for (e <- ls) yield {
          val (t, part) = printer(e, temp, nameCtx)(config)
          temp = t
          part
        }

        (temp, AppList(out.map(showApp).mkString(" ")))
      }
    }
  }

  //TODO: import these
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
    case AppList(s) => s"($s)"
  }

  def showApp(out: PartialOutput): String = out match {
    case Atom(s)    => s"$s"
    case Outer(s)   => s"[$s]"
    case AppList(s) => s"($s)"
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