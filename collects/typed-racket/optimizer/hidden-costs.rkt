#lang racket/base

(require syntax/parse syntax/stx
         (for-template racket/base)
         "../utils/utils.rkt"
         (optimizer utils logging)
         (types abbrev))

(require (types type-table))

(provide hidden-cost-log-expr)

(define-syntax-class hidden-port-parameter-function
  #:commit
  ;; not an exhaustive list
  (pattern (~or (~literal display) (~literal displayln) (~literal newline)
                (~literal write) (~literal write-byte) (~literal print)
                (~literal printf))))

;; This syntax class does not perform optimization.
;; It only logs operations with hidden costs, for use by Optimization Coach.
(define-syntax-class hidden-cost-log-expr
  #:commit
  ;; Log functions that access parameters implicitly (e.g. `display', which
  ;; accesses `current-output-port').
  (pattern (#%plain-app op:hidden-port-parameter-function args ...)
           ;; The function is not getting its output port as argument.
           ;; Since the port is first arg for some functions, second for
           ;; others, we're conservative, and look for a port in any position.
           #:when (andmap (lambda (a) (not (subtypeof? a -Output-Port)))
                          (syntax->list #'(args ...)))
           #:with opt
           (begin (log-optimization-info "hidden parameter" #'op)
                  #`(op #,@(stx-map (optimize) #'(args ...)))))
  ;; Log calls to struct constructors, so that OC can report those used in
  ;; hot loops.
  (pattern (#%plain-app op:id args ...)
           #:when (struct-constructor? #'op)
           #:with opt
           (begin (log-optimization-info "struct constructor" #'op)
                  #`(op #,@(stx-map (optimize) #'(args ...))))))
