#lang racket/load
(module re racket/base
  (require (for-syntax syntax/parse
                       racket/base
                       "dr-err-help.rkt"))
  
  (define-syntax (re stx)
    (syntax-parse
     stx
     [(_ the-re:sre)
      (attribute the-re.machine)]))
  
  (re _))

(require 're)
