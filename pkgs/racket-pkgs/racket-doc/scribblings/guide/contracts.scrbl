#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "contracts" #:style 'toc]{Contracts}

This chapter provides a gentle introduction to Racket's
contract system.

@refdetails["contracts"]{contracts}

@local-table-of-contents[]

@;{

Somewhere, discuss eq? and its impact on lists and
procedures. 

Also, discuss difference between contracts on
mutable datastructures & contracts on immutable ones.

Fill in question on optional arguments in general-function contracts.

->d and dependency (commented out section in general contracts).

update string-pad-center to show examples via REPL notation:

(string-pad-center "nba" 10)
(code:comment "=> \"   abc    \"")

(string-pad-center "nba" 10 #\-) 
(code:comment "=> \"---abc----\"")


}


@include-section["contracts-intro.scrbl"]
@include-section["contracts-simple-function.scrbl"]
@include-section["contracts-general-function.scrbl"]
@include-section["contracts-first-extended-example.scrbl"]
@include-section["contracts-structure.scrbl"]
@include-section["contracts-exists.scrbl"]
@include-section["contracts-examples.scrbl"]
@include-section["contracts-gotchas.scrbl"]
