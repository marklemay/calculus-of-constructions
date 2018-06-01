package cc

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._

import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cc.Cc.Var
import cc.Cc.Exp

import org.scalacheck.ScalacheckShapeless._
import cc_with_constructions.Constructions.ArrowConstruction

object CcConstructionSpec extends Properties("CcConstructionSpec") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(10000) // 1000000 is good for a coffee break :)

  def genVar: Gen[Var] = {
    for {
      i <- Gen.choose(0, 100)
    } yield Var(i)
  }

  implicit val arbVar: Arbitrary[Var] = Arbitrary(genVar)

  property("arrow packs and unpacks") = forAll { (a: ArrowConstruction.Arrow) =>
    import ArrowConstruction._
    a == fromExp(toExp(a)).get
  }

}