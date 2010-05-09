#lang scribble/manual

@(require "utils.ss" (for-label (only-meta-in 0 typed/racket)))
@(provide typed-mod)

@title[#:tag "quick"]{Quick Start}

Given a module written in the @racketmodname[racket] language, using
Typed Racket requires the following steps:

@itemize[#:style 
         'ordered
         @item{Change the language to @racketmodname[typed/racket].}
         @item{Change the uses of @racket[(require mod)] to
           @racket[(require typed/mod)].} 
         @item{Annotate structure definitions and top-level
           definitions with their types.} ]

Then, when the program is run, it will automatically be typechecked
before any execution, and any type errors will be reported.  If there
are any type errors, the program will not run.

Here is an example program, written in the @racketmodname[racket]
language:

@(define typed-mod
@racketmod[
typed/racket
(define-struct: pt ([x : Real] [y : Real]))

(: mag (pt -> Number))
(define (mag p)
  (sqrt (+ (sqr (pt-x p)) (sqr (pt-y p)))))
]
)

@racketmod[
racket
(define-struct pt (x y))

(code:contract mag : pt -> number)
(define (mag p)
  (sqrt (+ (sqr (pt-x p)) (sqr (pt-y p)))))
]

Here is the same program, in @racketmodname[typed/racket]:

@|typed-mod|
