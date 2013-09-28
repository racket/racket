#lang racket/base

(require syntax/parse syntax/stx unstable/sequence
         (for-template racket/base)
         "../utils/utils.rkt"
         (optimizer utils logging)
         (types abbrev type-table struct-table))

(provide hidden-cost-log-expr)

;; not an exhaustive list
(define-literal-syntax-class hidden-port-parameter-function
  (display displayln newline write write-byte print printf))

;; This syntax class does not perform optimization.
;; It only logs operations with hidden costs, for use by Optimization Coach.
(define-syntax-class hidden-cost-log-expr
  #:commit
  ;; Log functions that access parameters implicitly (e.g. `display', which
  ;; accesses `current-output-port').
  (pattern (#%plain-app op:hidden-port-parameter-function args:opt-expr ...)
    ;; The function is not getting its output port as argument.
    ;; Since the port is first arg for some functions, second for
    ;; others, we're conservative, and look for a port in any position.
    #:when (for/and ([arg (in-syntax #'(args ...))])
             (not (subtypeof? arg -Output-Port)))
    #:do [(log-optimization-info "hidden parameter" #'op)]
    #:with opt #'(op args.opt ...))
  ;; Log calls to struct constructors, so that OC can report those used in
  ;; hot loops.
  (pattern (#%plain-app op:id args:opt-expr ...)
    #:when (let ([constructor-for (syntax-property #'op 'constructor-for)])
             (or (and constructor-for (struct-constructor? constructor-for))
                 (struct-constructor? #'op)))
    #:do [(log-optimization-info "struct constructor" #'op)]
    #:with opt #'(op args.opt ...)))
