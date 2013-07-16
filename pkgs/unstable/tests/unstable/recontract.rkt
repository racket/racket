#lang racket/base
(require rackunit)

(module A racket
  (define (f x) (if (positive? x) x 'wrong))
  (provide (contract-out [f (-> real? real?)])))

(module B racket
  (require unstable/recontract
           (submod ".." A))
  (provide (rename-out [f f-from-a])
           (recontract-out f)))

(module C racket
  (require (submod ".." B))
  (define (af x) (f-from-a x))
  (define (bf x) (f x))
  (provide af bf))

(require 'C)

(define-syntax-rule (tcerr expr from blame)
  (do-tcerr (lambda () expr) 'expr from blame))

(define (do-tcerr thunk quoted-expr from blame)
  (test-case (format "~s" quoted-expr)
    (check-exn (lambda (e)
                 (let ([msg (exn-message e)])
                   (let ([from-m (regexp-match #rx"contract from:[ \n]*\\([^)]* ([A-Z])\\)" msg)])
                     (check-equal? (and from-m (cadr from-m)) from "contract from"))
                   (let ([blame-m (regexp-match #rx"blaming:[ \n]*\\([^)]* ([A-Z])\\)" msg)])
                     (check-equal? (and blame-m (cadr blame-m)) blame "blaming"))))
               thunk)))

;; Normally, A is the positive blame party
(test-equal? "af ok" (af 1) 1)
(tcerr (af -2) "A" "A")
(tcerr (af 'apple) "A" "C")

;; Check that recontract-out changes positive party to B
(test-equal? "bf ok" (bf 1) 1)
(tcerr (bf -2) "B" "B")
(tcerr (bf 'apple) "B" "C")
