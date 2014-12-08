#lang racket/load

(module test1 mzscheme
  (require mzlib/unit)
  (provide s1)
  (define-signature s1 
    ((define-values (a) (+ 1 b))
     b)))

(module test2 mzscheme
  (require mzlib/unit 'test1)
  (provide s2)
  (define-signature s2 extends s1
    ((define-values (c) (list b a d))
     d)))

(module test3 mzscheme
  (require mzlib/unit 'test1 'test2)
  (provide (all-defined))
  (define-unit u1 (import s1) (export)
    (list a b))
  (define-unit u2 (import s2) (export)
    (list a b c d))
  (define-unit u3 (import) (export s1)
    (define b 100))
  (define-unit u4 (import) (export s2)
    (define b 1000)
    (define d 10000)))


(module test4 mzscheme
  (require mzlib/unit 'test1 'test2 'test3)
  (require "test-harness.rkt")
  (test '(101 100)
   (invoke-unit 
    (compound-unit/infer (import) (export s1) (link u3 u1))))

 (test '(1001 1000)
   (invoke-unit 
    (compound-unit/infer (import) (export s1) (link u4 u1))))

  (test '(1001 1000)
    (invoke-unit 
     (compound-unit/infer (import) (export s2) (link u4 u1))))

  (test '(1001 1000 (1000 1001 10000) 10000)
    (invoke-unit 
     (compound-unit/infer (import) (export s1) (link u4 u2))))

  (test '(1001 1000 (1000 1001 10000) 10000)
    (invoke-unit 
     (compound-unit/infer (import) (export s2) (link u4 u2))))

  
  )

(require 'test4)
