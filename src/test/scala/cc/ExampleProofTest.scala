package cc

import org.junit.Assert.assertEquals
import org.junit.Test

import cc.Cc.Typ
import cc.Cc.Prop
import cc_with_constructions.Constructions

import cc_with_constructions.CcConstructionsParser._
import cc_with_constructions.ConstructionsConfig
import cc_with_constructions.Constructions.ArrowConstruction
import cc_with_constructions.Constructions.ProdConstruction

import CcProperties._

class ExampleProofTest {

  implicit val config = ConstructionsConfig(ArrowConstruction, ProdConstruction)

  @Test
  def AimpliesATest {

    val thrm = ccc"Π A : ● . A → A"

    val proof = ccc"λ A : ● . λ a : A . a"

    assert(proof.ty().get == thrm)
  }

  @Test
  def AimpliesAxATest {

    val thrm = ccc"Π A : ● . A → (A × A)"

    val proof = ccc"λ A : ● . λ a : A . λ OUT : ● . λ f : [A → A → OUT] . f a a"

    assert(proof.ty().get == thrm)
  }

  @Test
  def AxBimpliesATest {

    val thrm = ccc"Π A : ● . Π B : ● . (A × B) → A"

    val proof = ccc"λ A : ● . λ B : ● . λ pair : (A × B) . pair A (λ a:A. λ b:B. a)"

    assert(proof.ty().get == thrm)
  }

  //TODO: AxBimpliesBTest

  @Test
  def AxBimpliesBxATest {

    val thrm = ccc"Π A : ● . Π B : ● . (A × B) → (B × A)"

    //    println(thrm.show)

    val proof = ccc"""
      λ A : ● . λ B : ● . 
      λ pair : (A × B) . 
      pair (B × A) (λ a:A. λ b:B. λ OUT : ● . λ f : (B → A → OUT) . f b a)"""

    //    println(proof.show)

    assert(proof.ty().get == thrm)
  }

  @Test
  def prod_comm {
    val thrm = ccc"Π A : ● . Π B : ● . A × B → B × A"
    val proof = ccc"λ A : ● . λ B : ● . λ p : A × B . λ OUT : ● . λ f : B → A → OUT . f (p B (λ _ : A . λ b : B . b)) (p A (λ a : A . λ _ : B . a))"
    assertEquals(thrm, proof.ty().get)
  }

  @Test
  def prod_assoc {
    val thrm = ccc"Π A : ● . Π B : ● . Π C : ● . A × B × C → (A × B) × C"
    val proof = ccc"λ A : ● . λ B : ● . λ C : ● . λ p : A × B × C . λ OUT : ● . λ f : A × B → C → OUT . f (λ OUT0 : ● . λ f0 : A → B → OUT0 . f0 (p A (λ a : A . λ _ : B × C . a)) (p (B × C) (λ _ : A . λ b : B × C . b) B (λ a : B . λ _ : C . a))) (p (B × C) (λ _ : A . λ b : B × C . b) C (λ _ : B . λ b : C . b))"
    assertEquals(thrm, proof.ty().get)
  }

  //TOOD: the properties of sum types
  //TOOD: forall A, not (not (Lem A))
  //TOOD:  Eq refl, Eq symetric, Eq commutative, Eq allows replacement, Eq transitive,
  //TODO: for all A : ● , not ( A = not A)

}
