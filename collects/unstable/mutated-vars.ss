#lang scheme/base

(require (for-template scheme/base)
         syntax/boundmap syntax/kerncase)

;; mapping telling whether an identifer is mutated
;; maps id -> boolean
(define table (make-module-identifier-mapping))

;; find and add to mapping all the set!'ed variables in form
;; syntax -> void
(define (find-mutated-vars form)
  ;; syntax -> void
  (define (fmv/list lstx)
    (for-each find-mutated-vars (syntax->list lstx)))
  ;(when (and (pair? (syntax->datum form))) (printf "called with ~a~n" (syntax->datum form)))
  (kernel-syntax-case* form #f (define-type-alias-internal define-typed-struct-internal require/typed-internal)     
    ;; what we care about: set!
    [(set! v e)
     (begin
       (module-identifier-mapping-put! table #'v #t))]
    [(define-values (var ...) expr)
     (find-mutated-vars #'expr)]
    [(#%plain-app . rest) (fmv/list #'rest)]
    [(begin . rest) (fmv/list #'rest)]
    [(begin0 . rest) (fmv/list #'rest)]
    [(#%plain-lambda _ . rest) (fmv/list #'rest)]
    [(case-lambda (_ . rest) ...) (for-each fmv/list (syntax->list #'(rest ...)))]
    [(if . es) (fmv/list #'es)]
    [(with-continuation-mark . es) (fmv/list #'es)]
    [(let-values ([_ e] ...) . b) (begin (fmv/list #'(e ...))
                                         (fmv/list #'b))]
    [(letrec-values ([_ e] ...) . b) (begin (fmv/list #'(e ...))
                                            (fmv/list #'b))]
    [(letrec-syntaxes+values _ ([_ e] ...) . b) (begin (fmv/list #'(e ...))
						       (fmv/list #'b))]
    [(#%expression e) (find-mutated-vars #'e)]
    ;; all the other forms don't have any expression subforms (like #%top)
    [_ (void)]))

;; checks to see if a particular variable is ever set!'d
;; is-var-mutated? : identifier -> boolean
(define (is-var-mutated? id) (module-identifier-mapping-get table id (lambda _ #f)))

(provide find-mutated-vars is-var-mutated?)

