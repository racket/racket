#lang scheme
(require scheme/serialize)

(define-serializable-struct soft-state-record (id thnk))

(define *soft-state-cache*
  (make-weak-hasheq))

(define next-record-id!
  (local [(define record-id 0)]
    (lambda ()
      (begin0 record-id
              (set! record-id (add1 record-id))))))

(define (make-soft-state thnk)
  (make-soft-state-record (next-record-id!) thnk))

(define soft-state-ref
  (match-lambda
    [(struct soft-state-record (id thnk))
     (hash-ref! *soft-state-cache* id thnk)]))

(define soft-state? soft-state-record?)

(define-syntax-rule (soft-state expr ...) 
  (make-soft-state (lambda () expr ...)))

(provide
 soft-state)
(provide/contract
 [soft-state? (any/c . -> . boolean?)]
 [make-soft-state ((-> any/c) . -> . soft-state?)]
 [soft-state-ref (soft-state? . -> . any/c)])