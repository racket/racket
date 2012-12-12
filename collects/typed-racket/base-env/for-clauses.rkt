#lang racket/base

(require syntax/parse
         "annotate-classes.rkt"
         (for-template racket/base))

(provide (all-defined-out))

;; intersperses "#:when #t" clauses to emulate the for* variants' semantics
(define-splicing-syntax-class for-clause
  ;; single-valued seq-expr
  (pattern (~and c (var:optionally-annotated-name seq-expr:expr))
           #:with (expand ...) #`(#,(syntax/loc #'c
                                      (var.ann-name seq-expr)))
           #:with (expand* ...) #'(expand ... #:when #t))
  ;; multi-valued seq-expr
  ;; currently disabled because it triggers an internal error in the typechecker
  ;; (pattern (~and c (((v:optionally-annotated-name) ...) seq-expr:expr))
  ;;          #:with (expand ...) (list (syntax/loc #'c
  ;;                                      ((v.ann-name ...) seq-expr)))
  ;;          #:with (expand* ...) (list (quasisyntax/loc #'c
  ;;                                       ((v.ann-name ...) seq-expr))
  ;;                                     #'#:when #'#t))
  ;; Note: #:break and #:final clauses don't ever typecheck
  (pattern (~seq (~and kw (~or #:when #:unless #:break #:final)) guard:expr)
           #:with (expand ...) (list #'kw #'guard)
           #:with (expand* ...) #'(expand ...)))
