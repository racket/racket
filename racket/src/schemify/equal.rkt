#lang racket/base
(require "wrap.rkt"
         "match.rkt"
         "mutated-state.rkt")

;; Since a Racket `equal?` will shadow the host Scheme's `equal?`,
;; its optimizer won't be able to reduce `equal?` to `eq?` or `eqv?`
;; with obvious arguments. So, we perform that conversion in schemify.

(provide equal-implies-eq?
         equal-implies-eqv?
         always-eq/no-marks?)

(define (equal-implies-eq? e)
  (match e
    [`(quote ,val)
     (let ([val (unwrap val)])
       (or (symbol? val)
           (keyword? val)
           (null? val)
           (boolean-or-fixnum? val)))]
    [`,val
     (let ([val (unwrap val)])
       ;; Booleans and numbers don't have to be quoted
       (boolean-or-fixnum? val))]))

(define (boolean-or-fixnum? val)
  (or (boolean? val)
      (and (integer? val)
           (exact? val)
           ;; Always fixnum? conservatively...
           (<= (- (expt 2 24)) val (- (expt 2 24) 1)))))

(define (equal-implies-eqv? e)
  (match e
    [`(quote ,val)
     (let ([val (unwrap val)])
       (or (number? val)
           (char? val)))]
    [`,val
     (let ([val (unwrap val)])
       (number? val))]))

;; e1 and e2 have been simplified;
;; return #t only if e2 doesn't try to
;; consult continuation marks
(define (always-eq/no-marks? e1 e2 mutated)
  (match e1
    [`(quote ,v1)
     (match e2
       [`(quote ,v2)
        (eq? v1 v2)]
       [`,_ #f])]
    [`,_
     (define u-e1 (unwrap e1))
     (and (symbol? u-e1)
          (eq? u-e1 (unwrap e2))
          (simple-mutated-state? (hash-ref mutated u-e1 #f)))]))
