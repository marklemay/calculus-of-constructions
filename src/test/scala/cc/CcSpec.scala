package cc

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._

import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cc.Cc.Var
import cc.Cc.Exp

//import org.scalatest.prop.Checkers._

import org.scalacheck.ScalacheckShapeless._

object CcSpec extends Properties("CcSpec") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(10000) // 1000000 is good for a coffee break :)

  def genVar: Gen[Var] = {
    for {
      i <- Gen.choose(0, 100)
    } yield Var(i)
  }

  implicit val arbVar: Arbitrary[Var] = Arbitrary(genVar)

  property("typcheck normalizes to the same type") = forAll { e: Exp =>
    e.ty().isDefined ==> {
      e.ty().get == e.norm.ty().get
    }
  }

}