#lang racket/base

(require
  "../kinds.rkt"
  "../structures.rkt"
  "../constraints.rkt"
  racket/list
  racket/match
  (except-in racket/contract recursive-contract))

(provide
  (contract-out
    [flat/sc (syntax? . -> . static-contract?)]
    [chaperone/sc (syntax? . -> . static-contract?)]
    [impersonator/sc (syntax? . -> . static-contract?)]
    [flat/sc? predicate/c]))

(define (simple-contract-write-proc v port mode)
  (match-define (simple-contract _ syntax kind) v)
  (define-values (open close)
    (if (equal? mode 0)
        (values "(" ")")
        (values "#<" ">")))
  (display open port)
  (fprintf port "~a/sc" kind)
  (display " " port)
  (write (syntax->datum syntax) port)
  (display close port))



(struct simple-contract combinator (syntax kind)
        #:methods gen:sc
         [(define (sc-map v f) v)
          (define (sc->contract v f) (simple-contract-syntax v))
          (define (sc->constraints v f) (simple-contract-restrict (simple-contract-kind v)))]
        #:methods gen:custom-write [(define write-proc simple-contract-write-proc)])

(define (flat/sc ctc) (simple-contract null ctc 'flat))
(define (chaperone/sc ctc) (simple-contract null ctc 'chaperone))
(define (impersonator/sc ctc) (simple-contract null ctc 'impersonator))

(define (flat/sc? sc)
  (and (simple-contract? sc)
       (equal? 'flat (simple-contract-kind sc))))
