package cc

import org.junit.Assert.assertEquals
import org.junit.Test
import cc.CcParser._
import cc.Cc.Typ
import cc.Cc.Prop
import cc_with_constructions.Constructions

import cc.CcParser._
import cc_with_constructions.ConstructionsConfig

//import cc

class CcConstructionsTest {
  @Test
  def arrowTest {
    import Constructions.ArrowConstruction._

    val a = Arrow(Typ(), Prop())

    assert(a == fromExp(toExp(a)).get)
  }

  @Test
  def arrowprintTest {
    import Constructions.ArrowConstruction._

    import CcConstructionsPrettyPrinter._

    val a = cc"ΠA:●. ΠB:●. ΠC:●. ΠD:●. ΠE:●. ΠF:●. ΠG:●. ΠH:●. ΠI:●. ΠJ:●. E"

    println(printer(a, Set(), List())(ConstructionsConfig(Constructions.ArrowConstruction)))

  }
}