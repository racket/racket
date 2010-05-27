#lang racket/base

(require syntax/parse
         "annotate-classes.rkt"
         (for-template racket/base))

(provide convert-for-clauses)

;; we need handle #:when clauses manually because we need to annotate
;; the type of each nested for
(define (convert-for-clauses name clauses body ty)
  (let loop ((clauses clauses))
    (define-splicing-syntax-class for-clause
    ;; single-valued seq-expr
    (pattern (var:annotated-name seq-expr:expr)
             #:with expand #'(var.ann-name seq-expr))
    ;; multi-valued seq-expr
    (pattern ((v:annotated-name ...) seq-expr:expr)
             #:with expand #'((v.ann-name ...) seq-expr)))
    (syntax-parse clauses
      [(head:for-clause next:for-clause ... #:when rest ...)
       (syntax-property
        (quasisyntax/loc clauses
          (#,name
           (head.expand next.expand ...)
           #,(loop #'(#:when rest ...))))
        'type-ascription
        ty)]
      [(head:for-clause ...) ; we reached the end
       (syntax-property
        (quasisyntax/loc clauses
          (#,name
           (head.expand ...)
           #,@body))
        'type-ascription
        ty)]
      [(#:when guard) ; we end on a #:when clause
       (quasisyntax/loc clauses
         (when guard
           #,@body))]
      [(#:when guard rest ...)
       (quasisyntax/loc clauses
         (when guard
           #,(loop #'(rest ...))))])))
