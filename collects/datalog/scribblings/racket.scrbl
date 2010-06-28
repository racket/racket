#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/basic
          scribble/bnf
          (for-label racket/base
                     racket/contract
                     "../main.rkt")
          "utils.rkt")

@title{Racket Interoperability}

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
                     (? (parent x X))))]

@defthing[mutable-theory/c contract?]{ A contract for Datalog theories. }

@defproc[(make-theory) mutable-theory/c]{ Creates a theory for use with @racket[datalog]. }

@defform[(datalog thy-expr
                  stmt ...)
         #:contracts ([thy-expr mutable-theory/c])]{ Executes the statements on the theory given by @racket[thy-expr]. Returns the answers to the final query as a list of S-expressions or returns @racket[empty]. }
                                                   
@defform[(datalog! thy-expr
                  stmt ...)
         #:contracts ([thy-expr mutable-theory/c])]{ Executes the statements on the theory given by @racket[thy-expr]. Prints the answers to every query in the list of statements. Returns @racket[(void)]. }     
                                                   
Literals are represented as S-expressions with non-capitalized identifiers for constant symbols, strings for constant strings, and capitalized identifiers for variable symbols. Bound identifiers are treated as constants; they must evaluate to either a symbol or string. 

@defform[(! clause)]{ Asserts the clause. }
@defform[(~ clause)]{ Retracts the literal. }
@defform[(? literal)]{ Queries the literal and prints the result literals. }

@defform[(:- literal literal ...)]{ A conditional clause. }