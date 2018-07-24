# calculus-of-constructions
A simple reference implementation of the pure calculus of constructions

Start by reviewing the [example proofs](src/test/scala/cc/ExampleProofTest.scala).  Then review [Cc.scala](src/main/scala/cc/Cc.scala) to see a minimal implementation of the calculus of constructions.

There is a system to extend the base language with "constructions" so that common abbreviations can be used in the language interface.  They don't change the expressive power of the underlying theory at all, but make expressions much easier to read and write.  It's still a little half baked but you can see it implemented in [Constructions.scala](src/main/scala/cc_with_constructions/Constructions.scala)

## Build instructions

The build uses [sbt](https://www.scala-sbt.org/).

Running `sbt test` will download all dependencies, compile everything, and run all tests.

## What I learned after a few implementations
 * KISS
   * use a datatype that expresses the core AST (or ABT)
   * the expression problem is a real pain, I have not seen a practical solution at this point
   * I like De Bruijn indices but this seems to be a matter of taste (my advisor prefers string names) they each have costs and benefits, but I see no clear winner
   * don't prematurely optimize
     * when you optimize keep a reference copy of the slow version around for your tests
   * I don't know a good way to deal with holes
 * you need a pretty printer and a parser (even if you intend to have a better UI)
   * have found parser combinators the only viable lightweight solution
 * correctness
   * I have not had good luck using the type system to guarantee properties (this is possible in principle)
   * property based testing is as good as you can get in most current systems
     * kind of a pain to get the most out of these systems 
       * need to write custom generators and shrinkers
     * non termination/slow evaluation is a big pain
       * even in System F the Ackermann  function is computable, so that a language terminates is no guarantee that randomized automated tests will finish in a reasonable fashion (and there is no way to manage timeouts in scalacheck?)
     * something like tiny check would probably be ideal, especially if it could cache terms for use between tests.

 
## Not covered
 * UI
 * type inference
 * Elaboration
 * tactics
 * proof search

## TODO
 * review the normalization proof in the off chance it comes up "A short and flexible proof of Strong Normalization for the Calculus of Constructions"
 * rename everything
   * package and class names 
 * correct spelling/capitalization
 * add Scala docs
 * clean up pom
 * implicitly index everything by constructions
 * CI
 * there is much more information that could be included in the quick check tests
   * lots of redundant checks with the default generator
   * can shrink in a binding aware way
   * could generate closed expressions
   * could even generate well typed expressions (sort of)
   * tinycheck would be better for this, but noones built it in scala yet?
 * possible to abstract away the details of variables?
 * expression problem
 * a web UI would be cool
 * some proof automation would be cool
