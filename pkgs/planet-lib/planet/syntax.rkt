#lang racket/base

(provide make-planet-require-spec
         syntax-source-planet-package
         syntax-source-planet-package-owner
         syntax-source-planet-package-name
         syntax-source-planet-package-major
         syntax-source-planet-package-minor
         syntax-source-planet-package-symbol)

(require racket/match
         planet/util
         racket/syntax
         unstable/syntax
         (for-template racket/base)
         (for-label racket/base))

(define (syntax-source-planet-package stx)
  (let* ([dir (syntax-source-directory stx)])
    (and dir (path->package-version dir))))

(define (syntax-source-planet-package-owner stx)
  (match (syntax-source-planet-package stx)
    [(list owner name major minor) owner]
    [_ #f]))

(define (syntax-source-planet-package-name stx)
  (match (syntax-source-planet-package stx)
    [(list owner name major minor) name]
    [_ #f]))

(define (syntax-source-planet-package-major stx)
  (match (syntax-source-planet-package stx)
    [(list owner name major minor) major]
    [_ #f]))

(define (syntax-source-planet-package-minor stx)
  (match (syntax-source-planet-package stx)
    [(list owner name major minor) minor]
    [_ #f]))

(define (syntax-source-planet-package-symbol stx [suffix #f])
  (match (syntax-source-planet-package stx)
    [(list owner name major minor)
     (string->symbol
      (format "~a/~a:~a:=~a~a"
              owner
              (regexp-replace "\\.plt$" name "")
              major
              minor
              (if suffix (format-symbol "/~a" suffix) "")))]
    [#f #f]))

(define (make-planet-require-spec stx [id/f #f])
  (datum->syntax
   stx
   (list #'planet (syntax-source-planet-package-symbol stx id/f))
   (or id/f stx)))
