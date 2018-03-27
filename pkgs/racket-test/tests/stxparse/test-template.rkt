#lang racket/base
(require (for-syntax racket/base)
         rackunit
         (only-in "setup.rkt" convert-syntax-error tcerr)
         racket/promise
         racket/syntax
         syntax/parse
         syntax/parse/experimental/template)

;; See test-syntax.rkt for main syntax tests (now same as template).
;; This file has tests for features not exported by racket/base
;; (metafunctions).

(define-syntax (tc stx)
  (syntax-case stx ()
    [(tc expr expected)
     #`(test-equal? (format "line ~s" #,(syntax-line stx))
                    (syntax->datum (convert-syntax-error expr))
                    expected)]))

(define-syntax (terx stx)
  (syntax-case stx ()
    [(terx expr err-rx ...)
     #`(tcerr (format "line ~s" #,(syntax-line stx)) expr err-rx ...)]))

;; ----------------------------------------

;; Common pattern variable definitions
;; (avoids having to have 'with-syntax' in every test case)

(define/with-syntax uu #'abc)
(define/with-syntax (aa ...) #'(a b c))
(define/with-syntax (xx ...) #'(x y z))
(define/with-syntax (nn ...) #'(1 2 3))
(define/with-syntax ((yy ...) ...) #'((1 2 3) (4 5 6) (7 8 9)))

(define/syntax-parse (~or* oo:nat _:id) #'x)
(define/syntax-parse ((~or* pp:nat _:id) ...) #'(a 1 b 2 3))

;; ----------------------------------------

(define-template-metafunction (join stx)
  (syntax-parse stx
    [(join a:id b:id ...)
     (datum->syntax #'a
                    (string->symbol
                     (apply string-append
                            (map symbol->string
                                 (syntax->datum #'(a b ...)))))
                    stx)]))

(tc (template (join a b c))
    'abc)
(tc (template ((xx (join tmp- xx)) ...))
    '((x tmp-x) (y tmp-y) (z tmp-z)))
(tc (template ((xx (join uu - xx)) ...))
    '((x abc-x) (y abc-y) (z abc-z)))
(tc (template ((xx (join aa xx)) ...))
    '((x ax) (y by) (z cz)))

;; ============================================================

;; Error tests

(define-template-metafunction (bad-mf stx) 123)

(terx (template (bad-mf))
      #rx"result of template metafunction was not syntax")
