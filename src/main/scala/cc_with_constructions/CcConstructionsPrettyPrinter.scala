package cc

import cc.Cc._
import cc.CcProperties.Appls
import cc_with_constructions.ConstructionsConfig

import cc.CcPrettyPrinter.PartialOutput
import cc.CcPrettyPrinter.Atom
import cc.CcPrettyPrinter.AppList
import cc.CcPrettyPrinter.Outer

import cc.CcPrettyPrinter.showTy
import cc.CcPrettyPrinter.showBod
import cc.CcPrettyPrinter.showApp

import cc.CcPrettyPrinter.freshVar

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
        val newvar = freshVar(allVars, nameCtx, ty)
        val vars1 = allVars + newvar

        val (vars2, prettyTy) = printer(ty, vars1, nameCtx)(config)
        val (vars3, prettyBod) = printer(bod, vars2, newvar :: nameCtx)(config)

        (vars3, Outer(s"λ $newvar : ${showTy(prettyTy)} . ${showBod(prettyBod)}"))
      }

      case Pi(ty, bod) => {
        val newvar = freshVar(allVars, nameCtx, ty)
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

}