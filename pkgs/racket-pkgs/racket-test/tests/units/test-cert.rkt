#lang racket/load

(module test mzscheme
  (require mzlib/unit)
  
  (provide s)
  
  (define x add1)
  
  (define-signature s
    (a
     (define-values (y) (x a))
     (define-syntaxes (z)
       (syntax-rules () ((_) (x a)))))))

 (module test2 mzscheme
   (require mzlib/unit
            'test)
   (define-unit u1 (import) (export s)
     (define a 1))
   (define-unit u2 (import s) (export)
     (+ y (z)))
   (define-compound-unit u3 (import) (export)
     (link (((S : s)) u1)
           (() u2 S)))
   (printf "~a\n" (invoke-unit u3))
   )
 
 ;; 4
 (require 'test2)

 (module test3 mzscheme
   (require mzlib/unit
            'test)
   (define-unit u1 (import) (export s)
     (define a 1))
   (define-values/invoke-unit u1 (import) (export (rename s)))
   (printf "~a\n" (+ y (z)))
   )
 ;;4
 (require 'test3)
