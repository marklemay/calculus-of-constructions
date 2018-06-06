package cc

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._

import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cc.Cc.Var
import cc.Cc.Exp
import CcProperties._

import org.scalacheck.ScalacheckShapeless._

object CcSpec extends Properties("CcSpec") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000) // 1000000 is good for a coffee break :)

  def genVar: Gen[Var] = {
    for {
      i <- Gen.choose(0, 100)
    } yield Var(i)
  }

  implicit def shrinkExp: Shrink[Var] = Shrink { case _ => Stream() }

  implicit val arbVar: Arbitrary[Var] = Arbitrary(genVar)

  property("typcheck normalizes to the same type") = forAll { e: Exp =>
    e.ty().isDefined ==> {
      e.ty().get == e.norm.ty().get
    }
  }

  property("small step and big step match") = forAll { e: Exp =>
    eventuallyNormalizes(e) ==> {
      e.norm == eventualValue(e).get
    }
  }

  property("small step preserves type") = forAll { e: Exp =>
    e.ty().isDefined ==> {
      e.ty() == e.smallStep.ty()
    }
  }

  property("sub is equivelent to replaceVar @ Var(0) after eating the free var") = forAll { (e: Exp, withThis: Exp) =>
    e.sub(withThis) == e.replaceVar(Var(0), withThis.open()).sub(Var(0))
  }

  property("sub expression normalize consistently (church-rosser)") = forAll { (e: Exp, sube: Exp) =>
    (!e.freeVars.isEmpty) ==> {
      val v = e.freeVars.last
      val fullExp = e.replaceVar(v, sube)
      val fullExp2 = e.replaceVar(v, sube.smallStep)

      (eventuallyNormalizes(fullExp) || eventuallyNormalizes(fullExp2)) ==> {
        fullExp.norm == fullExp2.norm
      }
    }
  }

  def eventuallyNormalizes(e: Exp): Boolean = eventualValue(e).isDefined

  def eventualValue(e: Exp): Option[Exp] = {
    var temp = e
    for (_ <- 0.until(100)) {
      temp = temp.smallStep
    }
    Some(temp)
  }

}