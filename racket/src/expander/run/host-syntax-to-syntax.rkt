#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/datum-map.rkt"
         (prefix-in host: racket/base))

;; Just like `reader-syntax->syntax`, but for the host notion of
;; syntax (which can be different if `racket/base` provides a
;; different notion of syntax as its `read-syntax` than then runtime
;; system's reader)

(provide host-syntax->syntax)

(define (host-syntax->syntax v)
  (datum-map v
             (lambda (tail? v)
               (cond
                [(host:syntax? v)
                 (define e (host:syntax-e v))
                 (cond
                  [(syntax? e)
                   ;; Readtable, #lang, and #reader callbacks can lead to a
                   ;; reader syntax wrapper on our syntax
                   e]
                  [else
                   (define s
                     (struct-copy syntax empty-syntax
                                  [content (host-syntax->syntax (host:syntax-e v))]
                                  [srcloc (srcloc (host:syntax-source v)
                                                  (host:syntax-line v)
                                                  (host:syntax-column v)
                                                  (host:syntax-position v)
                                                  (host:syntax-span v))]))
                   (define keys (host:syntax-property-symbol-keys v))
                   (for/fold ([s s]) ([key (in-list keys)])
                     (syntax-property s key (host:syntax-property v key) #t))])]
                [else v]))))
