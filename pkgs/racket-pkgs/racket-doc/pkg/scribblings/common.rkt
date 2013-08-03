#lang at-exp racket/base
(require scribble/manual
         scribble/core)

(provide (all-defined-out))

(define (command s)
  @exec{raco pkg @|s|})

(define (command-ref s)
  @(seclink #:underline? #f (format "raco-pkg-~a" s) @command[s]))

(define (command/toc s)
  (list
   @subsection[#:tag (format "raco-pkg-~a" s)]{@command[s]}
   @command-ref[s]))

(define pkgname tt)
(define reponame elem)

(define (rtech . content)
  (apply tech #:doc '(lib "scribblings/reference/reference.scrbl") content))

@(define (inset . c)
   (apply nested #:style 'inset c))

