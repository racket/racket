#lang racket/base
(require "test-util.rkt")

(module A racket
  (define (f x) (if (positive? x) x 'wrong))
  (provide (contract-out [f (-> real? real?)])))

(module B racket
  (require racket/contract/base
           (submod ".." A))
  (provide (rename-out [f f-from-a])
           (recontract-out f)))

(module C racket
  (require (submod ".." B))
  (define (af x) (f-from-a x))
  (define (bf x) (f x))
  (provide af bf))

(require (submod "." C))

(define-syntax-rule (tcerr expr from blame)
  (do-tcerr (lambda () expr) 'expr from blame))

(define (do-tcerr thunk quoted-expr from blame)
  (define exn (with-handlers ([exn:fail? values]) (thunk)))
  (define msg (exn-message exn))
  
  (let ([from-m (regexp-match #rx"contract from:[ \n]*\\([^)]* ([A-Z])\\)" msg)])
    (test (and from-m (cadr from-m))
          (format "contract from ~s" quoted-expr)
          from))
  (let ([blame-m (regexp-match #rx"blaming:[ \n]*\\([^)]* ([A-Z])\\)" msg)])
    (test (and blame-m (cadr blame-m))
          (format "blaming ~s" quoted-expr)
          blame)))

;; Normally, A is the positive blame party
(test 1 "af ok" (af 1))
(tcerr (af -2) "A" "A")
(tcerr (af 'apple) "A" "C")

;; Check that recontract-out changes positive party to B
(test 1 "bf ok" (bf 1))
(tcerr (bf -2) "B" "B")
(tcerr (bf 'apple) "B" "C")
