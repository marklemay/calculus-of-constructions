package cc

import org.junit.Assert.assertEquals
import org.junit.Test
import cc.CcParser._
import cc.Cc._

//import cc

class CcParseTest {
  @Test
  def propTest {
    assert(cc"●" == Prop())

  }

  @Test
  def bracketTest {
    assert(cc"Π A : ● . [A]" == Pi(Prop(), Var(0)))

  }

  @Test
  def topLevelAppTest {
    assert(cc"● ●" == App(Prop(), Prop()))

  }
}