
import cc.CcParser._
import cc.CcProperties._

println("HI")                                   //> HI

3 + 2 + 4                                       //> res0: Int = 9

val id = cc"λ X: ●. λx:X. x"                    //> id  : cc.Cc.Exp = λ A : ● . λ a : A . a
id.t                                            //> res1: cc.Cc.Exp = Π A : ● . A → A
id.t.t                                          //> res2: cc.Cc.Exp = ●
id.t.t.t                                        //> res3: cc.Cc.Exp = □

val polyidexample = cc"""(
    (λ X: ●. λx:X. x)
    (Π X: ●. Πx:X. X)
    (λ X: ●. λx:X. x)
    )
    """                                         //> polyidexample  : cc.Cc.Exp = (λ A : ● . λ a : A . a) (Π A : ● . A →
                                                //|  A) (λ A : ● . λ a : A . a)
polyidexample.norm                              //> res4: cc.Cc.Exp = λ A : ● . λ a : A . a

val f = polyidexample.norm.asObject[String => Int => Int]
                                                //> f  : String => (Int => Int) = cc.CcProperties$RichExp$$Lambda$88/485041780@5
                                                //| 700d6b1
f("Int")(3)                                     //> res5: Int = 3

val bot = cc"Π T: ●. T"                         //> bot  : cc.Cc.Exp = Π A : ● . A
bot.t                                           //> res6: cc.Cc.Exp = ●

val and = cc"λ A :●. λ B :●. Π OUT: ●. Π f :(Π a:A. Π b:B. OUT). OUT "
                                                //> and  : cc.Cc.Exp = λ A : ● . λ B : ● . Π C : ● . (A → B → C) �
                                                //| � C
and.t                                           //> res7: cc.Cc.Exp = ● → ● → ●
and.t.t                                         //> res8: cc.Cc.Exp = □
val forAllAandBImpliesA = cc"""
λ A :●. λ B :●.
λ AandB : ($and A B) .
( AandB A (λ a :A. λ b :B. a ) ) """            //> forAllAandBImpliesA  : cc.Cc.Exp = λ A : ● . λ B : ● . λ C : (λ D :
                                                //| ● . λ E : ● . Π F : ● . (D → E → F) → F) A B . C A (λ a : A .
                                                //|  λ _ : B . a)
forAllAandBImpliesA.t                           //> res9: cc.Cc.Exp = Π A : ● . Π B : ● . (Π C : ● . (A → B → C) �
                                                //| � C) → A
val libeq = cc"""
λ A :●. λ a' :A. λ a'' :A.
Π P: (Π _ : A . Π _ : A . ●).
Π Peq: (Π a : A . (P a a) ).
   (P a' a'') """                               //> eq  : cc.Cc.Exp = λ A : ● . λ a : A . λ a1 : A . Π B : A → A → ●
                                                //|  . (Π a2 : A . B a2 a2) → B a a1
libeq.t                                            //> res10: cc.Cc.Exp = Π A : ● . A → A → ●

val eqrefl = cc"""
λ A :●. λ a :A.
λ P: (Π _ : A . Π _ : A . ●).
λ Peq: (Π a : A . (P a a) ).
   (Peq a) """                                  //> eqrefl  : cc.Cc.Exp = λ A : ● . λ a : A . λ B : A → A → ● . λ C
                                                //| : (Π a1 : A . B a1 a1) . C a
eqrefl.t                                        //> res11: cc.Cc.Exp = Π A : ● . Π a : A . Π B : A → A → ● . (Π a1 :
                                                //|  A . B a1 a1) → B a a
val eqSym = cc"""
λ A :●. λ x :A. λ y :A.
λ xEqy: ($libeq A x y ).
λ P: (Π _ : A . Π _ : A . ●) .
λ Prefl: (Π a : A . (P a a) ) .
(xEqy (λ b: A. λ c:A. (P c b)) (Prefl) )"""     //> eqSym  : cc.Cc.Exp = λ A : ● . λ a : A . λ a1 : A . λ B : (λ C : ●
                                                //|  . λ c : C . λ c1 : C . Π D : C → C → ● . (Π c2 : C . D c2 c2) �
                                                //| � D c c1) A a a1 . λ C : A → A → ● . λ D : (Π a2 : A . C a2 a2) .
                                                //| B (λ a2 : A . λ a3 : A . C a3 a2) D

eqSym.t                                         //> res12: cc.Cc.Exp = Π A : ● . Π a : A . Π a1 : A . (Π B : A → A →
                                                //| ● . (Π a2 : A . B a2 a2) → B a a1) → (Π B : A → A → ● . (Π a
                                                //| 2 : A . B a2 a2) → B a1 a)

val eqReplace = cc"""
λ A :●. λ x :A. λ y :A.
λ xEqy: (($libeq) A x y ).
λ C: (Π _ : A .  ●) .
(xEqy (λ a1: A. λ a2: A. Π _ : (C a1) . (C a2))
  (λ a : A . λ yeah : (C a) . yeah))"""         //> eqReplace  : cc.Cc.Exp = λ A : ● . λ a : A . λ a1 : A . λ B : (λ C :
                                                //|  ● . λ c : C . λ c1 : C . Π D : C → C → ● . (Π c2 : C . D c2 c2
                                                //| ) → D c c1) A a a1 . λ C : A → ● . B (λ a2 : A . λ a3 : A . C a2 �
                                                //| �� C a3) (λ a2 : A . λ D : C a2 . D)
eqReplace.t                                     //> res13: cc.Cc.Exp = Π A : ● . Π a : A . Π a1 : A . (Π B : A → A →
                                                //| ● . (Π a2 : A . B a2 a2) → B a a1) → (Π B : A → ● . B a → B a
                                                //| 1)

val eqTrans = cc"""
λ A :●. λ x :A. λ y :A. λ z :A.
λ xEqy: (($libeq) A x y ).
λ yEqz: (($libeq) A y z ).
λ P: (Π _ : A . Π _ : A . ●) .
λ Peq: (Π a : A . (P a a) ) .
(($eqReplace) A y z yEqz (λ a: A. (($libeq) A x a )) xEqy)"""
                                                //> eqTrans  : cc.Cc.Exp = λ A : ● . λ a : A . λ a1 : A . λ a2 : A . λ B
                                                //|  : (λ C : ● . λ c : C . λ c1 : C . Π D : C → C → ● . (Π c2 : C
                                                //|  . D c2 c2) → D c c1) A a a1 . λ C : (λ D : ● . λ d : D . λ d1 : D
                                                //| . Π E : D → D → ● . (Π d2 : D . E d2 d2) → E d d1) A a1 a2 . λ D
                                                //|  : A → A → ● . λ _ : (Π a3 : A . D a3 a3) . (λ E : ● . λ e : E
                                                //| . λ e1 : E . λ F : (λ G : ● . λ g : G . λ g1 : G . Π H : G → G �
                                                //| � ● . (Π g2 : G . H g2 g2) → H g g1) E e e1 . λ G : E → ● . F (λ
                                                //|  e2 : E . λ e3 : E . G e2 → G e3) (λ e2 : E . λ H : G e2 . H)) A a1 a2
                                                //|  C (λ a3 : A . (λ E : ● . λ e : E . λ e1 : E . Π F : E → E → ●
                                                //|  . (Π e2 : E . F e2 e2) → F e e1) A a a3) B
eqTrans.t                                       //> res14: cc.Cc.Exp = Π A : ● . Π a : A . Π a1 : A . Π a2 : A . (Π B :
                                                //| A → A → ● . (Π a3 : A . B a3 a3) → B a a1) → (Π B : A → A →
                                                //|  ● . (Π a3 : A . B a3 a3) → B a1 a2) → (Π B : A → A → ● . (Π
                                                //|  a3 : A . B a3 a3) → (Π C : A → A → ● . (Π a3 : A . C a3 a3) →
                                                //| C a a2))
