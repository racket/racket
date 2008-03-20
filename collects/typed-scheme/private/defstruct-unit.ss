#lang scheme/base
(require (lib "struct.ss") (lib "unit.ss"))
(provide #;(all-defined))

(define-syntax defstructs/sig/unit
  (syntax-rules (define-struct/properties)
    [(_ signame unitname (imps ...)
        def
        (define-struct/properties nm1 (flds1 ...) props #f)
        (define-struct/properties (nm par) (flds ...) () #f) ...)
     (begin
       (define-signature signame
         ((struct nm1 (flds1 ...))
          (struct nm (flds ...)) ...))
       (define-unit unitname           
         (import imps ...)
         (export signame)
         def
         (define-struct/properties nm1 (flds1 ...) props #f)
         (define-struct (nm par) (flds ...) #f) ...))]))

