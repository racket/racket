#lang scribble/base

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@(define (rurl x) (format "http://docs.racket-lang.org/reference/index.html?q=racket/~a" x))
@(define (rkt) @hyperlink[(rurl "")]{racket})
@(define (rkt/base) @hyperlink[(rurl "base")]{racket/base})
@(define (rkt/gui) @hyperlink[(rurl "gui")]{racket/gui})

@title{Some Performance Hints}

When you write a module, you first pick a language. In Racket you can
 choose a lot of languages. The most important choice concerns @rkt/base[]
 vs @rkt[].

If you are writing a script, try to use @rkt/base[]. The @rkt/base[]
 language loads significantly faster than the @rkt[] language because it is
 significantly smaller than the @rkt[].

If your module is intended as a library, stick to @rkt/base[]. That way
 script writers can use it without incurring the overhead of loading all of
 @rkt[] unknowingly.

Conversely, you should use @rkt[] (or even @rkt/gui[]) when you just want a
 convenient language to write some program. The @rkt[] language comes with
 almost all the batteries, and @rkt/gui[] adds the rest of the GUI base.


