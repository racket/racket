#lang racket/base
;; Allow evaluation at phase1
(require (for-syntax racket/base syntax/parse))
(provide phase1-eval phase1-phase0-eval)

(define-namespace-anchor anchor)
(define namespace (namespace-anchor->empty-namespace anchor))

;; See discussion of this function here:
;; http://www.mail-archive.com/dev@racket-lang.org/msg10759.html
;; http://lists.racket-lang.org/dev/archive/2014-May/014273.html
(define (prep! varref)
  (define p (variable-reference->resolved-module-path varref))
  (when p
    (parameterize ([current-namespace namespace])
      (dynamic-require p 0))))


(define-syntax phase1-phase0-run
  (syntax-parser
    [(_ form:expr ...)
     #'(let-syntax ([go (lambda (stx) form ...)]) (go))]))

 (define-syntax phase1-phase0-eval
   (syntax-parser
     [(_ form:expr ...)
      #'(begin
          (prep! (#%variable-reference))
          (eval-syntax (quote-syntax (phase1-phase0-run form ...)) namespace))]))

  (define-syntax phase1-eval
    (syntax-parser
      [(_ form:expr ...)
       #'(begin
           (prep! (#%variable-reference))
           (phase1-phase0-eval form ... #'(void)))]))
