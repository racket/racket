#lang racket/base

(require syntax/parse
         "annotate-classes.rkt"
         (for-template racket/base
                       "base-types-new.rkt"))

(provide (all-defined-out))

(define-splicing-syntax-class for-clause
    ;; single-valued seq-expr
    (pattern (var:annotated-name seq-expr:expr)
             #:with (expand ...) (list #`(var.ann-name
                                          #,(syntax-property #'seq-expr
                                                             'type-ascription
                                                             #'(Sequenceof var.ty)))))
    ;; multi-valued seq-expr
    ;; currently disabled because it triggers an internal error in the typechecker
    #;(pattern (((v:annotated-name) ...) seq-expr:expr)
             #:with (expand ...) (list #`((v.ann-name ...)
                                          #,(syntax-property #'seq-expr
                                                             'type-ascription
                                                             #'(Sequenceof (values v.ty ...))))))
    ;; when clause
    (pattern (~seq #:when guard:expr)
             #:with (expand ...) (list #'#:when #'guard)))

;; intersperses "#:when #t" clauses to emulate the for* variants' semantics
(define-splicing-syntax-class for*-clause
  ;; single-valued seq-expr
    (pattern (var:annotated-name seq-expr:expr)
             #:with (expand ...) (list #`(var.ann-name
                                          #,(syntax-property #'seq-expr
                                                             'type-ascription
                                                             #'(Sequenceof var.ty)))
                                       #'#:when #'#t))
    ;; multi-valued seq-expr
    ;; currently disabled because it triggers an internal error in the typechecker
    #;(pattern (((v:annotated-name) ...) seq-expr:expr)
             #:with (expand ...) (list #`((v.ann-name ...)
                                          #,(syntax-property #'seq-expr
                                                             'type-ascription
                                                             #'(Sequenceof (values v.ty ...))))
                                       #'#:when #'#t))
    ;; when clause
    (pattern (~seq #:when guard:expr)
             #:with (expand ...) (list #'#:when #'guard)))
