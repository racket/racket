#;
(exn-pred exn:fail:contract?)
#lang racket/load

(module outer typed/racket  
  (: f : Integer -> Integer)
  (define (f x) (add1 x))
  
  (provide f)
  
  (module* m racket
    (require (submod ".."))
    (f "foo"))
  
  (module* main racket
    (require (submod ".." m))))

(require (submod 'outer main))
