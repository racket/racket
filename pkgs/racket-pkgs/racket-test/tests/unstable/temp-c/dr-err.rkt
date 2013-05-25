#lang racket/load
(module re racket/base
  (require (for-syntax syntax/parse
                       racket/base
                       tests/unstable/temp-c/dr-err-help))
  
  (define-syntax (re stx)
    (syntax-parse
     stx
     [(_ the-re:sre)
      (attribute the-re.machine)]))
  
  (re _))

(require 're)
