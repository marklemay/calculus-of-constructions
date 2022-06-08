package cc

import org.junit.Test
import cc.CcParser._
import cc.Cc._

class CcParseTest {
  @Test
  def propTest: Unit = {
    assert(cc"●" == Prop())

  }

  @Test
  def bracketTest: Unit = {
    assert(cc"Π A : ● . [A]" == Pi(Prop(), Var(0)))

  }

  @Test
  def topLevelAppTest: Unit = {
    assert(cc"● ●" == App(Prop(), Prop()))

  }
}