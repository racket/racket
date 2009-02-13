#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          (for-label scheme/base
                     scheme/contract
                     stxclass 
                     stxclass/util))

@(define ellipses @scheme[...])
@(define (TODO . pre-flow)
   (make-splice
    (cons (bold "TODO: ")
          (decode-content pre-flow))))

@title{Parsing Syntax and Syntax Classes}

@bold{Warning: This library is still very volatile! Its interface and
behavior are subject to frequent change. I highly recommend that you
avoid creating PLaneT packages that depend on this library.}

The @schememodname[stxclass] library provides a framework for
describing and parsing syntax. Using @schememodname[stxclass], macro
writers can define new syntactic categories, specify their legal
syntax, and use them to write clear, concise, and robust macros.

To load the library:
@defmodule[stxclass]

@;{The first section is an overview with examples that illustrate
@schememodname[stxclass] features.}

The following sections are a reference for @schememodname[stxclass]
features.

@include-section["parsing-syntax.scrbl"]
@include-section["syntax-classes.scrbl"]
@include-section["library.scrbl"]
@include-section["util.scrbl"]

@local-table-of-contents[]

@;{


1 How to abstract over similar patterns:

(syntax-parse stx #:literals (blah bleh blaz kwA kwX)
  [(blah (bleh (kwX y z)) blaz)
   ___]
  [(blah (bleh (kwA (b c))) blaz)
   ___])

=>

(define-syntax-class common
  #:attributes (inner)
  #:literals (blah bleh blaz)
  (pattern (blah (bleh inner) blaz)))
(syntax-parse stx #:literals (kwA kwX)
  [c:common
   #:with (kwX y z) #'c.inner
   ___]
  [c:common
   #:with (kwA (b c)) #'c.inner
   ___])


OR =>

(define-syntax-class (common expected-kw)
  #:attributes (inner)
  #:literals (blah bleh blaz)
  (pattern (blah (bleh (kw . inner)) blaz)
           #:when (free-identifier=? #'kw expected-kw)))
(syntax-parse stx
  [c
   #:declare c (common #'kwX)
   #:with (y z) #'c.inner
   ___]
  [c
   #:declare c (common #'kwA)
   #:with ((b c)) #'c.inner
   ___])


}

