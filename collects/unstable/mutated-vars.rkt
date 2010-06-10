#lang racket/base

(require (for-template racket/base) racket/dict
         racket/trace
         syntax/id-table syntax/kerncase)

;; samth : this should use sets, not dicts
;; but sets do not have extensible comparisons
;; shouldn't be promoted until this is fixed

;; find and add to mapping all the set!'ed variables in form
;; syntax -> table
(define (find-mutated-vars form)
  (let loop ([stx form] [tbl (make-immutable-free-id-table)])
    ;; syntax-list -> table
    (define (fmv/list lstx)
      (for/fold ([tbl tbl])
          ([stx (in-list (syntax->list lstx))])
        (loop stx tbl)))
    (kernel-syntax-case* stx #f (#%top-interaction)
      ;; what we care about: set!
      [(set! v e)
       (dict-set (loop #'e tbl) #'v #t)]
      ;; forms with expression subforms
      [(define-values (var ...) expr)
       (loop #'expr tbl)]
      [(#%expression e) (loop #'e tbl)]
      [(#%plain-app . rest) (fmv/list #'rest)]
      [(begin . rest) (fmv/list #'rest)]
      [(begin0 . rest) (fmv/list #'rest)]
      [(#%plain-lambda _ . rest) (fmv/list #'rest)]
      [(case-lambda (_  rest ...) ...)
       (fmv/list #'(rest ... ...))]
      [(if . es) (fmv/list #'es)]
      [(with-continuation-mark . es) (fmv/list #'es)]
      [(let-values ([_ e] ...) b ...) (fmv/list #'(b ... e ...))]
      [(letrec-values ([_ e] ...) b ...) (fmv/list #'(b ... e ...))]
      [(letrec-syntaxes+values _ ([_ e] ...) b ...) (fmv/list #'(b ... e ...))]
      [(#%plain-module-begin . forms) (fmv/list #'forms)]
      ;; all the other forms don't have any expression subforms (like #%top)
      [_ tbl])))

(provide find-mutated-vars)
