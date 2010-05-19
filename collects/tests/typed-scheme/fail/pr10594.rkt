#;
(exn-pred exn:fail:contract? #rx".*U broke the contract.*")
#lang scheme/load

(module T typed-scheme
 
  (define-struct: [a] thing ([get : a]))
 
  (: thing->string ((thing String) -> String))
  (define (thing->string x)
    (string-append "foo" (thing-get x)))
 
  (provide (all-defined-out)))

(module U scheme

  (require 'T)
 
  (thing->string (make-thing 5)))

(require 'U)