# calculus-of-constructions
a simple implementation of the pure calculus of constructions

 * seperated into 2 parts...

## insights after implementing these things a lot
 * KISS
   * use a datatype that expresses the core AST (or ABT)
   * the expression problem is a real pain, I have not seen a practical solution at this point
   * I like debroign indexies but this seems to be a matter of taste (my advisor prefers string names) they each have costs and benifits, but I see no clear winner
   * don't prematurely optimize
     * when you optimize keep a reference copy of the slow version around for your tests
   * I don't know a good way to deal with holes
 * you need a pretty printer and a parser (even if you intend to have a better UI)
   * have found parser combinators the only viable lightweight solution
 * correctness
   * property based testing is as good as you can get in most current systems
     * kind of a pain to get the most out of these systems 
     * need to write custom generators and shrinkers
     * non termination/slow evluation is a big pain
     * something like tiny check would probly be ideal
   * I have not had good luck using the type system to garentee properties (this is possible in principle)
 
 
## Not covered
 * UI
 * type inference
 * Elaboration
 * tactics
 * proof search

## TODO
 * rename everything
 * correct spelling
 * add scala docs
 * clean up pom
 * parser more efficient
 * implicitly index everything by constructions
 * CI
 * TESTs
 * there is much more information that could be included in the quick check tests
   * lots of redundent checks with the defualt generator
   * can shrink in a binding aware way
   *could generate closed expressions
   * could even generate well typed expressions (sort of)
   * tinycheck would be better for this, but noones built it in scala yet
 * toScala?
 * possible to abstract away the details of variables?
 * expession problem 
 * a web UI would be cool
 * some automation would be cool