#;
(exn-pred #rx"contract violation"
          #rx"higher-order value passed as `Any` in untyped code"
          #rx"blaming: top-level")
#lang racket/load

(module typed typed/racket
  (provide g)

  (define-type Foo (Rec a (U (List Any) (Boxof a))))


  (: f (Byte -> Natural))
  (define (f x) (add1 x))
  (: g (Foo -> Void))
  (define (g b) 
    (when (box? b)
      (set-box! b (list f)))))

(require 'typed)
(define b (box (list #f)))
(g b)
(displayln ((first (unbox b)) "foo"))
