#;
(exn-pred exn:fail:syntax? ".*unbound.*")


#lang typed-scheme

(define-typed-struct A ([x : Number] [y : Boolean]))

(define: (f [a : A]) : Number
  (set-A-x! a 4)
  (set-A-y! a #f)
  (+ 4 (A-x a)))

(display (f (make-A 11 #t)))
