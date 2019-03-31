# calculus-of-constructions
[![Build Status](https://travis-ci.com/marklemay/calculus-of-constructions.svg?branch=master)](https://travis-ci.com/marklemay/calculus-of-constructions)

A simple reference implementation of the pure Calculus of Constructions in Scala.

You can find the basic rules for Calculus of Constructions on [nLab](https://ncatlab.org/nlab/show/pure+type+system) and some interesting constructions on [wikipedia](https://en.wikipedia.org/wiki/Calculus_of_constructions#Defining_logical_operators)

Start by reviewing the [example proofs](src/test/scala/cc/ExampleProofTest.scala).  Then review [Cc.scala](src/main/scala/cc/Cc.scala) to see a minimal implementation of the calculus of constructions.

There is a system to extend the base language with "constructions" so that common abbreviations can be used in the language interface.  They don't change the expressive power of the underlying theory at all, but make expressions much easier to read and write.  It's still a little half baked but you can see it implemented in [Constructions.scala](src/main/scala/cc_with_constructions/Constructions.scala)

## Build instructions

The build uses [sbt](https://www.scala-sbt.org/).

Running `sbt test` will download all dependencies, compile everything, and run all tests.

 
## Not covered
* UI
* type inference
* Elaboration
* tactics
* proof search

## References for implementing dependently typed languages
* everything [Stephanie Weirich](https://www.cis.upenn.edu/~sweirich/) has ever done
  * [Designing Dependently-Typed Programming Languages](https://www.cs.uoregon.edu/research/summerschool/summer13/curriculum.html)
* Andrej has a [zoo](http://plzoo.andrej.com/) of functional language implementations, and a good [presentation for dependent types](http://math.andrej.com/2018/08/25/how-to-implement-type-theory-in-an-hour/) with bidirectional typechecking 
* Bidirectional typechecking
  * [simplest presentation](http://davidchristiansen.dk/tutorials/bidirectional.pdf)
  * very intresting: http://semantic-domain.blogspot.com/2018/08/polarity-and-bidirectional-typechecking.html

## TODO (feel free to pull request any of these)
* rename everything
  * package and class names 
  * correct spelling/capitalization
* add Scala docs
* redo the pretty printing
* make it bidirectional
* implicitly index everything by constructions
* there is much more information that could be included in the quick check tests
  * lots of redundant checks with the default generator
  * can shrink in a binding aware way
  * could generate closed expressions
  * could even generate well typed expressions (sort of)
  * tinycheck would be better for this, but noones built it in scala yet?
* possible to abstract away the details of variables?
* expression problem (relevent to the half baked constructions interface)
* a web UI would be cool
* some proof automation would be cool
  * a minimal implementation of Doweks algoritm could be implemented with moderate effort.
* review the normalization proof in the off chance it comes up "A short and flexible proof of Strong Normalization for the Calculus of Constructions"
