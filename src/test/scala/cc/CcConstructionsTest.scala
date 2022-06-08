package cc

import org.junit.Assert.assertEquals
import org.junit.Test
import cc.CcParser._
import cc.Cc.Typ
import cc.Cc.Prop
import cc_with_constructions.Constructions

import cc.CcParser._
import cc_with_constructions.ConstructionsConfig
import cc_with_constructions.CcConstructionsParser

class CcConstructionsTest {
  @Test
  def arrowTest: Unit = {
    import Constructions.ArrowConstruction._

    val a = Arrow(Typ(), Prop())

    assert(a == fromExp(toExp(a)).get)
  }

  @Test
  def arrowprintTest: Unit = {
    import Constructions.ArrowConstruction._

    import CcConstructionsPrettyPrinter._

    val a = cc"ΠA:●. ΠB:●. ΠC:●. ΠD:●. ΠE:●. ΠF:●. ΠG:●. ΠH:●. ΠI:●. ΠJ:●. E"

    println(printer(a, Set(), List())(ConstructionsConfig(Constructions.ArrowConstruction)))

  }

  @Test
  def prodTest: Unit = {
    import cc.Cc._
    import Constructions.ProdConstruction._

    val p = Prod(Var(0), Var(0))

    println(toExp(p))
    println(fromExp(toExp(p)))

    assert(p == fromExp(toExp(p)).get)

  }

  @Test
  def prodprintTest: Unit = {
    import Constructions.ProdConstruction._

    import CcConstructionsPrettyPrinter._

    val a = cc"ΠA:●. ΠOUT:●. Πf: [Πa:A. Πaa:A. OUT ] . OUT "

    println(printer(a, Set(), List())(ConstructionsConfig(Constructions.ProdConstruction)))

    val a2 = cc"ΠA:●. Πpr:( ΠOUT:●. Πf: [Πa:A. Πaa:A. OUT ] . OUT ). A "

    println(printer(a2, Set(), List())(ConstructionsConfig(Constructions.ProdConstruction, Constructions.ArrowConstruction)))

    //TODO: assertion
  }

  @Test
  def arrowParserTest: Unit = {
    val exp = CcConstructionsParser.parse("Π A : ● . Π B : ● . A → B")(ConstructionsConfig(Constructions.ArrowConstruction))
    //A → A → B
    //TODO: mixin application into

    println(exp)

    println(CcConstructionsParser.parse("Π A : ● . Π B : ● . A → A → B")(ConstructionsConfig(Constructions.ArrowConstruction)))

    println(CcConstructionsParser.parse("Π A : ● . Π B : ● . A → A A → B")(ConstructionsConfig(Constructions.ArrowConstruction)))

    //TODO: assertion
  }

  @Test
  def arrowProdParserTest: Unit = {
    val exp = CcConstructionsParser.parse("Π A : ● . Π B : ● . (A × A × A) → (B × B)")(ConstructionsConfig(Constructions.ProdConstruction, Constructions.ArrowConstruction))

    //A → A → B
    //TODO: mixin application into

    println(exp)

    import CcConstructionsPrettyPrinter._

    println(printer(exp.get, Set(), List())(ConstructionsConfig(Constructions.ProdConstruction, Constructions.ArrowConstruction)))

    //TODO: assertion
  }

}