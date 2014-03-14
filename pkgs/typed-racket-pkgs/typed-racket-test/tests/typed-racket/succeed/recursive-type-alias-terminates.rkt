#lang racket/load

;; Test that contracts for mutually recursive class types do not
;; cause infinite loops when they execute.

(module a typed/racket
  (define-type X%
    (Class [m (-> (Instance Y%) String)]))
  (define-type Y%
    (Class [m (-> (Instance X%) Any)]))
  (: x X%)
  (define x (class object%
              (super-new)
              (define/public (m y) "foo")))
  (: y Y%)
  (define y (class object%
              (super-new)
              (define/public (m x) 0)))
  (provide x y))

(require 'a)
(send (new x) m (new y))
