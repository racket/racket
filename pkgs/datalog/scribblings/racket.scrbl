#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          (for-label racket/base
                     racket/contract
                     "../main.rkt")
          "utils.rkt")

@title[#:tag "interop"]{Racket Interoperability}

@defmodule[datalog]

The Datalog database can be directly used by Racket programs through this API.

@examples[#:eval the-eval
          (define family (make-theory))

          (datalog family
                   (! (parent joseph2 joseph1))
                   (! (parent joseph2 lucy))
                   (! (parent joseph3 joseph2)))

          (datalog family
                   (? (parent X joseph2)))

          (datalog family
                   (? (parent joseph2 X)))

          (datalog family
                   (? (parent joseph2 X))
                   (? (parent X joseph2)))

          (datalog family
                   (! (:- (ancestor A B)
                          (parent A B)))
                   (! (:- (ancestor A B)
                          (parent A C)
                          (= D C) 
                          (ancestor D B))))
 
          (datalog family
                   (? (ancestor A B)))
 
          (let ([x 'joseph2])
            (datalog family
                     (? (parent x X))))

          (datalog family
                   (? (add1 1 :- X)))]

@defthing[theory/c contract?]{ A contract for Datalog theories. }

@defproc[(make-theory) theory/c]{ Creates a theory for use with @racket[datalog]. }

@defproc[(write-theory [t theory/c]) void]{ Writes a theory to the current output port. Source location information is lost. }
@defproc[(read-theory) theory/c]{ Reads a theory from the current input port. }

@defform[(datalog thy-expr
                  stmt ...)
         #:contracts ([thy-expr theory/c])]{ Executes the statements on the theory given by @racket[thy-expr]. Returns the answers to the final query as a list of substitution dictionaries or returns @racket[empty]. }

@defform[(datalog! thy-expr
                  stmt ...)
         #:contracts ([thy-expr theory/c])]{ Executes the statements on the theory given by @racket[thy-expr]. Prints the answers to every query in the list of statements. Returns @racket[(void)]. }     

Statements are either assertions, retractions, or queries.

@defform[(! clause)]{ Asserts the clause. }
@defform[(~ clause)]{ Retracts the literal. }

@defform[(:- literal question ...)]{ A conditional clause. }

@defform[(? question)]{ Queries the literal and prints the result literals. }

Questions are either literals or external queries.
Literals are represented as @racket[identifier] or @racket[(identifier term ...)].
External queries are represented as @racket[(identifier term ... :- term ...)], where @racket[identifier] is bound to a procedure that when given the first set of terms as arguments returns the second set of terms as values.
A term is either a non-capitalized identifiers for a constant symbol, a Racket datum for a constant datum, or a capitalized identifier for a variable symbol. Bound identifiers in terms are treated as datums. 

External queries invalidate Datalog's guaranteed termination. For example, this program does not terminate:
@racketblock[
 (datalog (make-theory)
          (! (:- (loop X)
                 (add1 X :- Z)
                 (loop Z)))
          (? (loop 1)))
 ]
