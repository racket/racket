
;; A sane "core" for finishing up the "scheme/base" library

(module pre-base '#%kernel
  (#%require (for-syntax '#%kernel))
  (#%require "more-scheme.ss"
             "misc.ss"
             (all-except "define.ss" define)
             "letstx-scheme.ss"
             "kw.ss"
             "define-struct.ss"
             "reqprov.ss"
             "modbeg.ss"
             "for.ss"

             '#%builtin) ; so it's attached

  (define-syntaxes (#%top-interaction)
    (lambda (stx)
      (if (eq? 'top-level (syntax-local-context))
          'ok
          (raise-syntax-error
           #f
           "not at top level"
           stx))
      (datum->syntax stx (cdr (syntax-e stx)) stx stx)))

  (#%provide (all-from-except "more-scheme.ss" old-case)
             (all-from "misc.ss")
             (all-from "define.ss")
             (all-from-except "letstx-scheme.ss" -define -define-syntax -define-struct)
             (rename new-lambda lambda)
             (rename new-lambda λ)
             (rename new-define define)
             (rename new-app #%app)
             (rename #%app #%plain-app)
             (rename lambda #%plain-lambda)
             (rename #%module-begin #%plain-module-begin)
             (rename module-begin #%module-begin)
             (all-from-except '#%kernel lambda λ #%app #%module-begin)
             (all-from "reqprov.ss")
             (all-from "for.ss")
             #%top-interaction

             make-keyword-procedure
             keyword-apply
             procedure-keywords
             (rename define-struct* define-struct)
             define-struct/derived
             struct-field-index))
