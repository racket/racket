#lang racket/base
(require setup/cross-system)

(provide platform-spec?
         matching-platform?)

(define (platform-spec? p)
  (or (symbol? p) (string? p) (regexp? p)))

(define (matching-platform? p
                            #:cross? [cross? #f]
                            #:system-type [sys-type #f]
                            #:system-library-subpath [sys-lib-subpath #f])
  (unless (platform-spec? p)
    (raise-argument-error 'matching-platform? "platform-spec?" p))
  (unless (or (not sys-type) (symbol? sys-type))
    (raise-argument-error 'matching-platform? "(or/c symbol? #f)" sys-type))
  (unless (or (not sys-lib-subpath) (path-for-some-system? sys-lib-subpath))
    (raise-argument-error 'matching-platform? "(or/c path-for-some-system? #f)" sys-lib-subpath))
  (cond
   [(symbol? p)
    (eq? p (or sys-type (if cross?
                            (cross-system-type)
                            (system-type))))]
   [else
    (define s (bytes->string/utf-8
               (path->bytes
                (or sys-lib-subpath
                    (if cross?
                        (cross-system-library-subpath #f)
                        (system-library-subpath #f))))))
    (cond
     [(regexp? p)
      (regexp-match? p s)]
     [else
      (equal? p s)])]))
