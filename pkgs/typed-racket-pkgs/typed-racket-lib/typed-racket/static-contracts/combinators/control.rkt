#lang racket/base

;; Static contracts for control contracts.
;; Currently only supports prompt tags.

(require "../structures.rkt" "../constraints.rkt"
         racket/list racket/match
         unstable/contract
         racket/contract
         (for-template racket/base racket/contract/base)
         (for-syntax racket/base syntax/parse))

(provide
  (contract-out
    [prompt-tag/sc ((listof static-contract?) (maybe/c (listof static-contract?)) . -> . static-contract?)])
  prompt-tag/sc:)

(struct prompt-tag-combinator combinator ()
  #:transparent
  #:property prop:combinator-name "prompt-tag/sc"
  #:methods gen:sc
    [(define (sc-map v f)
       (prompt-tag-combinator (pt-seq-map f (combinator-args v))))
     (define (sc-traverse v f)
       (pt-seq-map f (combinator-args v))
       (void))
     (define (sc->contract v f)
       (match v
        [(prompt-tag-combinator (pt-seq vals call-cc))
         (with-syntax ([(vals-stx ...) (map f vals)]
                       [(call-cc-stx ...)
                        (if call-cc
                            #`(#:call/cc (values #,@(map f call-cc)))
                            empty)])
           #'(prompt-tag/c vals-stx ... call-cc-stx ...))]))
     (define (sc->constraints v f)
       (merge-restricts* 'chaperone (map f (pt-seq->list (combinator-args v)))))])

(struct pt-seq (vals call-cc))

(define (prompt-tag/sc vals call-cc)
  (prompt-tag-combinator (pt-seq vals call-cc)))

(define-match-expander prompt-tag/sc:
  (syntax-parser
    [(_ vals call-cc)
     #'(prompt-tag-combinator (pt-seq vals call-cc))]))



(define (pt-seq-map f seq)
  (match seq
   [(pt-seq vals call-cc)
    (define (f* a) (f a 'invariant))
    (pt-seq
      (map f* vals)
      (and call-cc (map f* call-cc)))]))

(define (pt-seq->list seq)
  (match seq
   [(pt-seq vals call-cc)
    (append
      vals
      (or call-cc empty))]))
