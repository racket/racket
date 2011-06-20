#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "booleans"]{Booleans}

Racket has two distinguished constants to represent boolean values:
@racket[#t] for true and @racket[#f] for false. Uppercase
@racketvalfont{#T} and @racketvalfont{#F} are parsed as the same
values, but the lowercase forms are preferred.

The @racket[boolean?] procedure recognizes the two boolean
constants. In the result of a test expression for @racket[if],
@racket[cond], @racket[and], @racket[or], etc., however, any value
other than @racket[#f] counts as true.

@examples[
(= 2 (+ 1 1))
(boolean? #t)
(boolean? #f)
(boolean? "no")
(if "no" 1 0)
]

