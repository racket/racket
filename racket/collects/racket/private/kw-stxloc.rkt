;; Like `syntax/loc` and `quasisyntax/loc`, but ensure that the
;; source location is copied even if there is no source location on
;; the input (as `(quasi)syntax/loc` doesn't copy a #f source
;; location); used by "kw.rkt"
(module kw-stxloc '#%kernel
  (#%declare #:unsafe-cross-phase-persistent)
  (#%require "core-syntax.rkt"
             "stxcase-scheme.rkt"
             "qqstx.rkt"
             (for-syntax '#%kernel
                         "core-syntax.rkt"
                         "stxcase-scheme.rkt"
                         "qqstx.rkt"))

  (#%provide syntax/loc/always quasisyntax/loc/always)

  (define-syntaxes (syntax/loc/always quasisyntax/loc/always)
    (let ([mk (lambda (syntax-id)
                (lambda (stx)
                  (syntax-case stx ()
                    [(_ src-expr template)
                     #`(let ([result (#,syntax-id template)])
                         (datum->syntax result (syntax-e result) src-expr result))])))])
      (values (mk #'syntax) (mk #'quasisyntax)))))
