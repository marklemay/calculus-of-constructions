package cc

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Prop._

import org.scalacheck._
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import cc.Cc.Var
import cc.Cc.Exp
import cc.Cc.Prop

import org.scalacheck.ScalacheckShapeless._

import CcProperties._

object CcParserSpec extends Properties("CcParserSpec") {

  override def overrideParameters(p: Test.Parameters): Test.Parameters =
    p.withMinSuccessfulTests(100) // 1000000 is good for a coffee break :)

  def genVar: Gen[Var] = {
    for {
      i <- Gen.choose(0, 1)
    } yield Var(i)
  }

  implicit def shrinkExp: Shrink[Var] = Shrink { case _ => Stream() }

  implicit val arbVar: Arbitrary[Var] = Arbitrary(genVar)

  property("can always parse a closed pretty printed term") = forAll { e: Exp =>
    e.freeVars.isEmpty ==> {
      CcParser.parse(CcPrettyPrinter.prettyShow(e)(100)(Set())(List())._2) match {
        case CcParser.Success(e2, rest) => e == e2 && rest.atEnd
        case _                          => false
      }
    }
  }

  property("can always parse a closed fully parened term") = forAll { e: Exp =>
    e.freeVars.isEmpty ==> {
//      println(CcPrettyPrinter.showFullyParen(e)(Set())(List())._2) something is fisshy here!
      CcParser.parse(CcPrettyPrinter.showFullyParen(e)(Set())(List())._2) match {
        case CcParser.Success(e2, rest) => e == e2 && rest.atEnd
        case _                          => false
      }
    }
  }

  // the above test makes this redundant
  property("can always parse a closed term") = forAll { e: Exp =>
    e.freeVars.isEmpty ==> {
      CcParser.parse(e.toString()).successful
    }
  }

}