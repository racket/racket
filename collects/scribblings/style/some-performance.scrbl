#lang scribble/base

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@(define rurl "http://docs.racket-lang.org/reference/index.html?q=racket/base")
@(define (rkt) @hyperlink[rurl]{racket})
@(define (rkt/base) @hyperlink[rurl]{racket/base})

@title{Some Performance Hints}

Use @rkt/base[] instead of @rkt[] for any library that others may use
 eventually. For all other modules, use @rkt[].

The @rkt/base[] language loads significantly faster than the @rkt[]
language and is also significnatly smaller. Conversely, it is much more
convenient to program with @rkt[] than @rkt/base[].
