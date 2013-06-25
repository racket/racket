#lang racket/load

(module a racket
  (provide (struct-out foo))
  (define-struct foo (proc) #:property prop:procedure (struct-field-index proc)))

(module b typed/racket
  (require/typed 'a
                 (struct foo ((proc : (Number Number -> Number)))))
  (if (procedure? (foo +))
       #t
       (error 'wrong-branch))) 

(require 'b)
