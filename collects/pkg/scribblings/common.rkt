#lang at-exp racket/base
(require scribble/manual
         scribble/core)

(provide (all-defined-out))

(define (command s)
  @exec{raco pkg @|s|})

(define (command-ref s)
  @(link-element "plainlink" @command[s] `(raco-pkg-cmd ,s)))

(define (command/toc s)
  @(toc-target-element #f @command[s] `(raco-pkg-cmd ,s)))

