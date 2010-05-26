#lang scheme/base

(require syntax/parse
         "annotate-classes.rkt")

(provide for-clause)

(define-splicing-syntax-class for-clause
  ;; single-valued seq-expr
  (pattern (var:annotated-name seq-expr:expr)
           #:with (expand ...) (list #'(var.ann-name seq-expr)))
  ;; multi-valued seq-expr
  (pattern ((var:annotated-name ...) seq-expr:expr)
           #:with (expand ...) (list #'((var.ann-name ...) seq-expr)))
  ;; when clause
  (pattern (~seq #:when guard:expr)
           #:with (expand ...) (list #'#:when #'guard)))
