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

object NormalFormSpec extends Properties("NormalFormSpec.scala") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(1000) // 1000000 is good for a coffee break :)

  //TODO: refactor out redundent generator bits
  def genVar: Gen[Var] = {
    for {
      i <- Gen.choose(0, 10)
    } yield Var(i)
  }

  implicit def shrinkExp: Shrink[Var] = Shrink { case _ => Stream() }

  implicit val arbVar: Arbitrary[Var] = Arbitrary(genVar)

  def genNFVar: Gen[NormalForm.Var] = {
    for {
      i <- Gen.choose(0, 10)
    } yield NormalForm.Var(i)
  }

  implicit def shrinkNF: Shrink[NormalForm.Var] = Shrink { case _ => Stream() }

  implicit val arbNFVar: Arbitrary[NormalForm.Var] = Arbitrary(genNFVar)

  property("normal forms round trip to expressions") = forAll { n: NormalForm.Normal =>
    n == NormalForm.toNomal(NormalForm.toExp(n)).get
  }

  property("normal types round trip to expressions") = forAll { n: NormalForm.TyNormal =>
    n == NormalForm.toTyNomal(NormalForm.toExp(n)).get
  }

  property("neutral types round trip to expressions") = forAll { n: NormalForm.Neutral =>
    n == NormalForm.toNeutral(NormalForm.toExp(n)).get
  }

  property("Ty Normal expressions represent types if they typecheck (could be an open expression)") = forAll { n: NormalForm.TyNormal =>
    NormalForm.toExp(n).ty().isDefined ==> {
      NormalForm.toExp(n).isType
    }
  }

  property("Normal forms correspond to values") = forAll { n: NormalForm.Normal =>
    NormalForm.toExp(n).isValue
  }

  property("Normal types correspond to values") = forAll { n: NormalForm.TyNormal =>
    NormalForm.toExp(n).isValue
  }

  property("Neutral forms correspond to values") = forAll { n: NormalForm.Neutral =>
    NormalForm.toExp(n).isValue
  }

  property("if it typechecks (it normalizes) then can be reduced to normal form") = forAll { e: Exp =>
    e.ty().isDefined ==> {
      NormalForm.toNomal(e.norm).isDefined
    }
  }

  //Note: not actually true since "λ A : [λ B : ● . □] . ●"
  //  property("if it normalizes, itcan be reduced to normal form") = forAll { e: Exp =>
  //    eventuallyNormalizes(e) ==> {
  //      NormalForm.toNomal(e.norm).isDefined
  //    }
  //  }

  property("if is type (it normalizes) and can be reduced to normal form") = forAll { e: Exp =>
    e.isType ==> {
      NormalForm.toTyNomal(e.norm).isDefined
    }
  }

  //TODO: these will be pretty sparse
  property("if an exppression can be put in normal from, it can be round tripped") = forAll { e: Exp =>
    NormalForm.toNomal(e).isDefined ==> {
      NormalForm.toExp(NormalForm.toNomal(e).get) == e
    }
  }

  property("if an exppression can be put in type normal from, it can be round tripped") = forAll { e: Exp =>
    NormalForm.toTyNomal(e).isDefined ==> {
      NormalForm.toExp(NormalForm.toTyNomal(e).get) == e
    }
  }

  property("if an exppression can be put in Neutral from, it can be round tripped") = forAll { e: Exp =>
    NormalForm.toNeutral(e).isDefined ==> {
      NormalForm.toExp(NormalForm.toNeutral(e).get) == e
    }
  }

  //TODO: index by a rendomized number of steps
  def eventuallyNormalizes(e: Exp): Boolean = eventualValue(e).isDefined

  def eventualValue(e: Exp): Option[Exp] = {
    var temp = e
    for (_ <- 0.until(100)) {
      temp = temp.smallStep
    }
    if (temp.isValue) {
      Some(temp)
    } else {
      None
    }
  }

}