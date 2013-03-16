#lang typed/racket/base

(define: (A ...) (lister args : A ... A) : (List A ... A)
   args)
