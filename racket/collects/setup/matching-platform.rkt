#lang racket/base

(provide platform-spec?
         matching-platform?)

(define (platform-spec? p)
  (or (symbol? p) (string? p) (regexp? p)))

(define (matching-platform? p
                            #:system-type [sys-type #f]
                            #:system-library-subpath [sys-lib-subpath #f])
  (unless (platform-spec? p)
    (raise-argument-error 'matching-platform? "platform-spec?" p))
  (unless (or (not sys-type) (symbol? sys-type))
    (raise-argument-error 'matching-platform? "(or/c symbol? #f)" sys-type))
  (unless (or (not sys-lib-subpath) (path? sys-lib-subpath))
    (raise-argument-error 'matching-platform? "(or/c path? #f)" sys-lib-subpath))
  (cond
   [(symbol? p)
    (eq? p (or sys-type (system-type)))]
   [else
    (define s (path->string (or sys-lib-subpath
                                (system-library-subpath #f))))
    (cond
     [(regexp? p)
      (regexp-match? p s)]
     [else
      (equal? p s)])]))
