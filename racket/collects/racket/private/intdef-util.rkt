#lang racket/base

(provide add-decl-props)
(require syntax/intdef)

;; this function copies properties from the declarations expressions
;; that get dropped. (e.g. (public x) from the body of a class).
;; It doesn't use syntax-track-origin because there is
;; no residual code that it would make sense to be the result of expanding
;; those away. So, instead we only look at a few properties (as below).
;; Also, add 'disappeared-binding properties from `ctx`.
(define (add-decl-props def-ctx decls stx)
  (internal-definition-context-track
   def-ctx
   (for/fold ([stx stx]) ([decl (in-list decls)])
     (define (copy-prop src dest stx)
       (syntax-property
        stx
        dest
        (cons (syntax-property decl src)
              (syntax-property stx dest))))
     (copy-prop
      'origin 'disappeared-use
      (copy-prop
       'disappeared-use 'disappeared-use
       (copy-prop
        'disappeared-binding 'disappeared-binding
        stx))))))
