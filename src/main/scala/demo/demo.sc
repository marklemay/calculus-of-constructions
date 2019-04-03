package demo

import cc.CcParser._
import cc.CcProperties._

object demo {
  println("Welcome to the Scala wodfsheet")       //> Welcome to the Scala wodfsheet

  3 + 2 + 4                                       //> res0: Int = 9

  "λ"                                             //> res1: String("λ") = λ

  cc"λ X: ●. λx:X. x" //.t                        //> res2: cc.Cc.Exp = λ A : ● . λ a : A . a

  val polyidexample = cc"""(
      (λ X: ●. λx:X. x)
      (Π X: ●. Πx:X. X)
      (λ X: ●. λx:X. x)
      )
      """                                         //> polyidexample  : cc.Cc.Exp = [λ A : ● . λ a : A . a] [Π B : ● . Π b 
                                                  //| : B . B] [λ C : ● . λ c : C . c]
  polyidexample.norm                              //> res3: cc.Cc.Exp = λ A : ● . λ a : A . a
  polyidexample.t                                 //> res4: cc.Cc.Exp = Π A : ● . Π a : A . A

  val f = polyidexample.norm.asObject[String => Int => Int]
                                                  //> f  : String => (Int => Int) = cc.CcProperties$RichExp$$Lambda$80/1926764753@
                                                  //| 6e06451e
  f("Int")(3)                                     //> res5: Int = 3

  val bot = cc"Π T: ●. T"                         //> bot  : cc.Cc.Exp = Π A : ● . A
  bot.t                                           //> res6: cc.Cc.Exp = ●

  val and = cc"λ A :●. λ B :●. Π OUT: ●. Π f :(Π a:A. Π b:B. OUT). OUT "
                                                  //> and  : cc.Cc.Exp = λ A : ● . λ B : ● . Π C : ● . Π D : [Π a : A .
                                                  //|  Π b : B . C] . C
  and.t                                           //> res7: cc.Cc.Exp = Π A : ● . Π B : ● . ●

  val forAllAandBImpliesA = cc"""
  λ A :●. λ B :●.
  λ AandB : ($and A B) .
  ( AandB A (λ a :A. λ b :B. a ) ) """            //> forAllAandBImpliesA  : cc.Cc.Exp = λ A : ● . λ B : ● . λ C : [λ D : 
                                                  //| ● . λ E : ● . Π F : ● . Π G : [Π d : D . Π e : E . F] . F] A B . 
                                                  //| (C A [λ a : A . λ b : B . a])
  forAllAandBImpliesA.t                           //> res8: cc.Cc.Exp = Π A : ● . Π B : ● . Π C : [Π D : ● . Π E : [Π 
                                                  //| a : A . Π b : B . D] . D] . A

  val eq = cc"""
  λ A :●. λ a' :A. λ a'' :A.
  Π P: (Π _ : A . Π _ : A . ●).
  Π Peq: (Π a : A . (P a a) ).
     (P a' a'') """                               //> eq  : cc.Cc.Exp = λ A : ● . λ a : A . λ a1 : A . Π B : [Π a2 : A . Π
                                                  //|  a3 : A . ●] . Π C : [Π a4 : A . (B a4 a4)] . (B a a1)
  eq.t                                            //> res9: cc.Cc.Exp = Π A : ● . Π a : A . Π a1 : A . ●

  val eqrefl = cc"""
  λ A :●. λ a :A.
  λ P: (Π _ : A . Π _ : A . ●).
  λ Peq: (Π a : A . (P a a) ).
     (Peq a) """                                  //> eqrefl  : cc.Cc.Exp = λ A : ● . λ a : A . λ B : [Π a1 : A . Π a2 : A 
                                                  //| . ●] . λ C : [Π a3 : A . (B a3 a3)] . (C a)

  val eqSym = cc"""
  λ A :●. λ x :A. λ y :A.
  λ xEqy: ($eq A x y ).
  λ P: (Π _ : A . Π _ : A . ●) .
  λ Peq: (Π a : A . (P a a) ) .
  (xEqy (λ b: A. λ c:A. (P c b)) (Peq) )"""       //> eqSym  : cc.Cc.Exp = λ A : ● . λ a : A . λ a1 : A . λ B : [λ C : ●
                                                  //|  . λ c : C . λ c1 : C . Π D : [Π c2 : C . Π c3 : C . ●] . Π E : [Π
                                                  //|  c4 : C . (D c4 c4)] . (D c c1)] A a a1 . λ F : [Π a2 : A . Π a3 : A . �
                                                  //| ��] . λ G : [Π a4 : A . (F a4 a4)] . (B [λ a5 : A . λ a6 : A . (F a6 a5
                                                  //| )] G)

  eqSym.t                                         //> res10: cc.Cc.Exp = Π A : ● . Π a : A . Π a1 : A . Π B : [Π C : [Π a
                                                  //| 2 : A . Π a3 : A . ●] . Π D : [Π a4 : A . (C a4 a4)] . (C a a1)] . Π 
                                                  //| E : [Π a5 : A . Π a6 : A . ●] . Π F : [Π a7 : A . (E a7 a7)] . (E a1 
                                                  //| a)

  val eqReplace = cc"""
  λ A :●. λ x :A. λ y :A.
  λ xEqy: (($eq) A x y ).
  λ C: (Π _ : A .  ●) .
  (xEqy (λ a1: A. λ a2: A. Π _ : (C a1) . (C a2))
    (λ a : A . λ yeah : (C a) . yeah))"""         //> eqReplace  : cc.Cc.Exp = λ A : ● . λ a : A . λ a1 : A . λ B : [λ C :
                                                  //|  ● . λ c : C . λ c1 : C . Π D : [Π c2 : C . Π c3 : C . ●] . Π E :
                                                  //|  [Π c4 : C . (D c4 c4)] . (D c c1)] A a a1 . λ F : [Π a2 : A . ●] . (B
                                                  //|  [λ a3 : A . λ a4 : A . Π G : F a3 . (F a4)] [λ a5 : A . λ H : F a5 . 
                                                  //| H])
  eqReplace.t                                     //> res11: cc.Cc.Exp = Π A : ● . Π a : A . Π a1 : A . Π B : [Π C : [Π a
                                                  //| 2 : A . Π a3 : A . ●] . Π D : [Π a4 : A . (C a4 a4)] . (C a a1)] . Π 
                                                  //| E : [Π a5 : A . ●] . Π F : E a . (E a1)

  val eqTrans = cc"""
  λ A :●. λ x :A. λ y :A. λ z :A.
  λ xEqy: (($eq) A x y ).
  λ yEqz: (($eq) A y z ).
  λ P: (Π _ : A . Π _ : A . ●) .
  λ Peq: (Π a : A . (P a a) ) .
  (($eqReplace) A y z yEqz (λ a: A. (($eq) A x a )) xEqy)"""
                                                  //> eqTrans  : cc.Cc.Exp = λ A : ● . λ a : A . λ a1 : A . λ a2 : A . λ B
                                                  //|  : [λ C : ● . λ c : C . λ c1 : C . Π D : [Π c2 : C . Π c3 : C . ●
                                                  //| ] . Π E : [Π c4 : C . (D c4 c4)] . (D c c1)] A a a1 . λ F : [λ G : ● 
                                                  //| . λ g : G . λ g1 : G . Π H : [Π g2 : G . Π g3 : G . ●] . Π I : [Π 
                                                  //| g4 : G . (H g4 g4)] . (H g g1)] A a1 a2 . λ J : [Π a3 : A . Π a4 : A . �
                                                  //| ��] . λ K : [Π a5 : A . (J a5 a5)] . ([λ L : ● . λ l : L . λ l1 : L 
                                                  //| . λ M : [λ N : ● . λ n : N . λ n1 : N . Π O : [Π n2 : N . Π n3 : N
                                                  //|  . ●] . Π P : [Π n4 : N . (O n4 n4)] . (O n n1)] L l l1 . λ Q : [Π l2
                                                  //|  : L . ●] . (M [λ l3 : L . λ l4 : L . Π R : Q l3 . (Q l4)] [λ l5 : L 
                                                  //| . λ S : Q l5 . S])] A a1 a2 F [λ a6 : A . ([λ T : ● . λ t : T . λ t1
                                                  //|  : T . Π U : [Π t2 : T . Π t3 : T . ●] . Π V : [Π t4 : T . (U t4 t4)
                                                  //| ] . (U t t1)] A a a6)] B)
  eqTrans.t                                       //> res12: cc.Cc.Exp = Π A : ● . Π a : A . Π a1 : A . Π a2 : A . Π B : [
                                                  //| Π C : [Π a3 : A . Π a4 : A . ●] . Π D : [Π a5 : A . (C a5 a5)] . (C 
                                                  //| a a1)] . Π E : [Π F : [Π a6 : A . Π a7 : A . ●] . Π G : [Π a8 : A .
                                                  //|  (F a8 a8)] . (F a1 a2)] . Π H : [Π a9 : A . Π a10 : A . ●] . Π I : [
                                                  //| Π a11 : A . (H a11 a11)] . Π J : [Π a12 : A . Π a13 : A . ●] . Π K :
                                                  //|  [Π a14 : A . (J a14 a14)] . (J a a2)

  val not = cc""" λ A :●. $bot"""                 //> not  : cc.Cc.Exp = λ A : ● . Π B : ● . B
  not.t                                           //> res13: cc.Cc.Exp = Π A : ● . ●

  val or = cc""" λ A :●. λ B :●. Π OUT : ● . Π inL : (Π  a: A . OUT ) . Π inR : (Π  b: B . OUT ) . OUT """
                                                  //> or  : cc.Cc.Exp = λ A : ● . λ B : ● . Π C : ● . Π D : [Π a : A .
                                                  //|  C] . Π E : [Π b : B . C] . C

  or.t                                            //> res14: cc.Cc.Exp = Π A : ● . Π B : ● . ●

  val lem = cc""" λ A :●. (($or) A (($not)A)) """ //> lem  : cc.Cc.Exp = λ A : ● . ([λ B : ● . λ C : ● . Π D : ● . Π
                                                  //|  E : [Π b : B . D] . Π F : [Π c : C . D] . D] A ([λ G : ● . Π H : �
                                                  //| � . H] A))
  lem.t                                           //> res15: cc.Cc.Exp = Π A : ● . ●

  val notNotLemTy = cc"""
  Π A :●. (($not) (($not) (($lem) A)))"""         //> notNotLemTy  : cc.Cc.Exp = Π A : ● . ([λ B : ● . Π C : ● . C] ([λ
                                                  //|  D : ● . Π E : ● . E] ([λ F : ● . ([λ G : ● . λ H : ● . Π I 
                                                  //| : ● . Π J : [Π g : G . I] . Π K : [Π h : H . I] . I] F ([λ L : ● .
                                                  //|  Π M : ● . M] F))] A)))
  notNotLemTy.norm                                //> res16: cc.Cc.Exp = Π A : ● . Π B : ● . B
  notNotLemTy.t                                   //> res17: cc.Cc.Exp = ●

  val notNotLem = cc"""
  λ A :●. ($not ($not ($lem A)))"""               //> notNotLem  : cc.Cc.Exp = λ A : ● . ([λ B : ● . Π C : ● . C] ([λ D
                                                  //|  : ● . Π E : ● . E] ([λ F : ● . ([λ G : ● . λ H : ● . Π I : 
                                                  //| ● . Π J : [Π g : G . I] . Π K : [Π h : H . I] . I] F ([λ L : ● . �
                                                  //| � M : ● . M] F))] A)))
  //always assume LEM
  // readable types

}