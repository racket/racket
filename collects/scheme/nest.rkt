#lang racket/base

(provide nest)

;; A form that makes it easy to combine many "right-drifting" nested
;; expressions into a single one, sort of like a generalized version of the `*'
;; from `let*' and similar forms.
(define-syntax nest
  (syntax-rules ()
    [(nest () body0 body ...)
     (let () body0 body ...)]
    ;; this allows putting definitions in the body
    [(nest ([form forms ...]) body0 body ...)
     (form forms ... (let () body0 body ...))]
    [(nest ([form forms ...] . more) body0 body ...)
     (form forms ... (nest more body0 body ...))]))

;; using this instead will allow splicing body expressions in the last form
;; whatever it happens to be, which can be (ab)used in strange ways
#;
(define-syntax nest
  (syntax-rules ()
    [(nest () body ...)
     (begin body ...)]
    [(nest ([form forms ...]) body ...)
     (form forms ... body ...)]
    [(nest ([form forms ...] . more) body ...)
     (form forms ... (nest more body ...))]))
