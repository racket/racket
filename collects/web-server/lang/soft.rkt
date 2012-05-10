#lang racket/base
(require racket/contract
         racket/match
         racket/local
         racket/serialize)

(define-serializable-struct soft-state-record (thnk))
(define-struct some (value))

(define *soft-state-cache*
  (make-weak-hash))

(define (make-soft-state thnk)
  (make-soft-state-record thnk))

(define (soft-state-ref ss)
  (match ss
    [(struct soft-state-record (thnk))
     (define the-weak-box
       (hash-ref! *soft-state-cache* ss (lambda () (make-weak-box (make-some (thnk))))))
     (define the-val
       (weak-box-value the-weak-box))
     (if (some? the-val)
         (some-value the-val)
         (local [(define real-val (thnk))]
           (hash-set! *soft-state-cache* ss (make-weak-box (make-some real-val)))
           real-val))]))

(define soft-state? soft-state-record?)

(define-syntax-rule (soft-state expr ...) 
  (make-soft-state (lambda () expr ...)))

(provide
 soft-state)
(provide/contract
 [soft-state? (any/c . -> . boolean?)]
 [make-soft-state ((-> any/c) . -> . soft-state?)]
 [soft-state-ref (soft-state? . -> . any/c)])
