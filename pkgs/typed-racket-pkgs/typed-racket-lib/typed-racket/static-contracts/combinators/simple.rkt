#lang racket/base

;; Static contracts that are terminal and have no sub parts.
;; Unlike contracts defined with define-terminal-contract, equality of these contracts is based solely
;; on identity. Thus they are most useful for contracts which have no meaningful structure.
;; Ex: (flat/sc #'number?)

(require
  "../structures.rkt"
  "../constraints.rkt"
  racket/match
  racket/contract)

(provide
  (contract-out
    [flat/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)]
    [chaperone/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)]
    [impersonator/sc ((syntax?) ((or/c #f any/c)) . ->* . static-contract?)]))

(define (simple-contract-write-proc v port mode)
  (match-define (simple-contract syntax kind name) v)
  (define-values (open close)
    (if (equal? mode 0)
        (values "(" ")")
        (values "#<" ">")))
  (display open port)
  (fprintf port "~a/sc" kind)
  (display " " port)
  (write (or name (syntax->datum syntax)) port)
  (display close port))



(struct simple-contract static-contract (syntax kind name)
        #:transparent
        #:methods gen:sc
         [(define (sc-map v f) v)
          (define (sc-traverse v f) (void))
          (define (sc->contract v f) (simple-contract-syntax v))
          (define (sc->constraints v f) (simple-contract-restrict (simple-contract-kind v)))
          (define (sc-terminal-kind v) (simple-contract-kind v))]
        #:methods gen:custom-write [(define write-proc simple-contract-write-proc)])

(define (flat/sc ctc [name #f])
  (simple-contract ctc 'flat name))
(define (chaperone/sc ctc [name #f])
  (simple-contract ctc 'chaperone name))
(define (impersonator/sc ctc [name #f])
  (simple-contract ctc 'impersonator name))
