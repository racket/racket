#lang scribble/manual

@(require "utils.rkt" (for-label (only-meta-in 0 typed/scheme)))
@(provide typed-mod)

@title[#:tag "quick"]{Quick Start}

Given a module written in the @schememodname[scheme] language, using
Typed Scheme requires the following steps:

@itemize[#:style 
         'ordered
         @item{Change the language to @schememodname[typed/scheme].}
         @item{Change the uses of @scheme[(require mod)] to
           @scheme[(require typed/mod)].} 
         @item{Annotate structure definitions and top-level
           definitions with their types.} ]

Then, when the program is run, it will automatically be typechecked
before any execution, and any type errors will be reported.  If there
are any type errors, the program will not run.

Here is an example program, written in the @schememodname[scheme]
language:

@(define typed-mod
@schememod[
typed/scheme
(define-struct: pt ([x : Real] [y : Real]))

(: mag (pt -> Number))
(define (mag p)
  (sqrt (+ (sqr (pt-x p)) (sqr (pt-y p)))))
]
)

@schememod[
scheme
(define-struct pt (x y))

(code:contract mag : pt -> number)
(define (mag p)
  (sqrt (+ (sqr (pt-x p)) (sqr (pt-y p)))))
]

Here is the same program, in @schememodname[typed/scheme]:

@|typed-mod|
