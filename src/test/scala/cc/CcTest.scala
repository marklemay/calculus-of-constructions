package cc

import org.junit.Test
import cc.CcParser._

import CcProperties._

class CcTest {
  @Test
  def simpleTests: Unit = {
    val exp = cc"λ X: ●. λx:X. x "

    assert(exp == exp.norm)

    assert(exp.ty() == Some(cc"Π X: ●. Πx:X. X "))

    assert(cc"(λ X: ●. λx:X. x) " == exp)

    assert(cc"""( 
      (λ X: ●. λx:X. x)
      (Π X: ●. Πx:X. X)
      (λ X: ●. λx:X. x)
      )
      """.norm == exp)
  }

  @Test
  def smallstepMatchesBigStepTests: Unit = {
    val e = cc"λ A : ● . λ B : □ . (● ([λ C : □ . □] □))"

    assert(e.norm == e.smallStep)
  }

  @Test
  def ccAsScalaObject: Unit = {

    val e = cc"λ A : ● . A".asObject[Int => Int]

    assert(e(3) == 3)
    assert(e(3) != 4)

    val polyId = cc"λ A : ● . λ a : A . a".asObject[Any => (Int => Int)]

    assert(polyId("whatever")(3) == 3)
    assert(polyId("whatever")(3) != 4)

    val functionApp1 = cc"λ A : ● . λ f : [Πa:A. A] .  λ a : A . (f a) ".asObject[Any => (Int => Int) => (Int => Int)]

    assert(functionApp1("whatever") { (x: Int) => x * x }(3) == 9)

    val functionApp2 = cc"λ A : ● . λ f : [Πa:A. A] .  λ a : A . (f (f a)) ".asObject[Any => (Int => Int) => (Int => Int)]

    assert(functionApp2("whatever") { (x: Int) => x * x }(3) == 81)
  }
}