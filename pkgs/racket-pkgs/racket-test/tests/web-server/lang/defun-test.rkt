#lang racket/base
(require rackunit
         web-server/lang/defun
         web-server/lang/util)
(provide defun-tests)

(define-syntax vwrap
  (syntax-rules ()
    [(_ e)
     (call-with-values
      (lambda () e)
      (lambda x x))]))

(define defun-tests
  (test-suite
   "Defunctionalization"
   
   ; XXX Doesn't work for non-exp values
   #;(test-not-exn "define-struct" (lambda () (vwrap (defun (expand (syntax (define-struct posn (x y))))))))
   (test-not-exn "quote-syntax" (lambda () (vwrap (defun (expand (syntax #'provide/contract-id-set-a-date-day!))))))
   #;(test-not-exn "provide/contract" (lambda () (vwrap (defun (expand (syntax (module t racket
                                                                                 (require racket/contract)
                                                                                 (define x 1)
                                                                                 (provide/contract
                                                                                  [x integer?]))))))))
   ))
