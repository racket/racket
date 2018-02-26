#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/scope.rkt"
         (only-in "reader-syntax.rkt"
                  [datum->syntax reader:datum->syntax]
                  [syntax-property reader:syntax-property]))

(provide syntax->reader-syntax
         srcloc->vector)

(define (syntax->reader-syntax v)
  (syntax-map v
              (lambda (tail? v) v)
              (lambda (orig-s d)
                (define s (reader:datum->syntax #f d (srcloc->vector (syntax-srcloc orig-s))))
                (define keys (syntax-property-symbol-keys orig-s))
                (for/fold ([s s]) ([key (in-list keys)])
                  (reader:syntax-property s key (syntax-property orig-s key))))
              syntax-e))

(define (srcloc->vector s)
  (and s
       (vector (srcloc-source s)
               (srcloc-line s)
               (srcloc-column s)
               (srcloc-position s)
               (srcloc-span s))))
