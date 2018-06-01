package cc

import org.junit.Assert.assertEquals
import org.junit.Test
import cc.CcParser._

//import cc

class CcTest {
  @Test
  def simpleTests {
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

}