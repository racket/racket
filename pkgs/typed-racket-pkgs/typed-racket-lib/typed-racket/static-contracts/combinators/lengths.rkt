#lang racket/base

;; Static contracts for list and vector lengths.
;; These are used during optimizations as simplifications.
;; Ex: (list/sc any/sc) => (list-length/sc 1)

(require
  "../structures.rkt"
  "simple.rkt"
  racket/match
  (for-template racket/base))

(provide
  list-length/sc
  vector-length/sc
  empty-list/sc
  empty-vector/sc)


(define (length-contract-write-proc v port mode)
  (match-define (length-contract name length stx) v)
  (define-values (open close)
    (if (equal? mode 0)
        (values "(" ")")
        (values "#<" ">")))
  (display open port)
  (fprintf port "~a/sc" name)
  (display " " port)
  (write length port)
  (display close port))


(struct length-contract static-contract (name length syntax)
        #:methods gen:sc
         [(define (sc-map v f) v)
          (define (sc->contract v f) (length-contract-syntax v))
          (define (sc->constraints v f) 'flat)]
        #:methods gen:terminal-sc
         [(define (terminal-sc-kind v) 'flat)]
        #:methods gen:custom-write [(define write-proc length-contract-write-proc)])

(define (list-length/sc n)
  (if (equal? 0 n)
      empty-list/sc
      (length-contract 'length n
                       #`(λ (l) (and (list? l) (= #,n (length l)))))))
(define empty-list/sc (flat/sc #'null?))

(define (vector-length/sc n)
  (length-contract 'vector-length n
                   #`(λ (l) (and (vector? l) (= #,n (vector-length l))))))
(define empty-vector/sc (vector-length/sc 0))
