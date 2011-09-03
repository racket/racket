
#lang scheme/load

(module m typed-scheme
 (provide f)
 (: f Sexp)
 (define f 5))

(require 'm)

f
