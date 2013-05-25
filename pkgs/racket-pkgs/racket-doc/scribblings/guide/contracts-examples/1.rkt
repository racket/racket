#lang racket
;; data definitions 

(define id? symbol?)
(define id-equal? eq?)
(define-struct basic-customer (id name address) #:mutable)

;; interface 
(provide
 (contract-out
  [id?                   (-> any/c boolean?)]
  [id-equal?             (-> id? id? boolean?)]
  [struct basic-customer ((id id?)
                          (name string?)
                          (address string?))]))
;; end of interface




