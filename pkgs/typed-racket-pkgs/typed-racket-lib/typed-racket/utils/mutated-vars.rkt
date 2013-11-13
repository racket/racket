#lang racket/base

(require (for-template racket/base) racket/dict
         syntax/parse syntax/id-table unstable/sequence)

;; find and add to mapping all the set!'ed variables in form
;; if the supplied mapping is mutable, mutates it
;; default is immutability
;; syntax [table] -> table
(define (find-mutated-vars form [tbl (make-immutable-free-id-table)])
  (define add (if (dict-mutable? tbl)
                  (lambda (t i) (dict-set! t i #t) t)
                  (lambda (t i) (dict-set t i #t))))
  (let loop ([stx form] [tbl tbl])
    ;; syntax-list -> table
    (define (fmv/list lstx)
      (for/fold ([tbl tbl]) ([stx (in-syntax lstx)])
        (loop stx tbl)))
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      ;; what we care about: set!
      [(set! v e)
       (add (loop #'e tbl) #'v)]
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
