#lang scribble/doc
@require[scribble/manual]
@require[scribble/eval]
@require["guide-utils.ss"]

@title[#:tag "booleans"]{Booleans}

Scheme has two distinguished constants to represent boolean values:
@scheme[#t] for true and @scheme[#f] for false. Uppercase
@schemevalfont{#T} and @schemevalfont{#F} are parsed as the same
values, but the lowercase forms are preferred.

The @scheme[boolean?] procedure recognizes the two boolean
constants. In the result of a test expression for @scheme[if],
@scheme[cond], @scheme[and], @scheme[or], etc., however, any value
other than @scheme[#f] counts as true.

@examples[
(= 2 (+ 1 1))
(boolean? #t)
(boolean? #f)
(boolean? "no")
(if "no" 1 0)
]

