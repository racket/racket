#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/original.rkt"
         "../syntax/datum-map.rkt"
         (prefix-in reader:
                    (only-in "reader-syntax.rkt"
                             syntax? syntax-e syntax-property
                             syntax-property-symbol-keys
                             syntax-source syntax-line syntax-column
                             syntax-position syntax-span)))

(provide reader-syntax->syntax)

(define (reader-syntax->syntax v)
  (datum-map v
             (lambda (tail? v)
               (cond
                [(reader:syntax? v)
                 (define e (reader:syntax-e v))
                 (cond
                  [(syntax? e)
                   ;; Readtable, #lang, and #reader callbacks can lead to a
                   ;; reader syntax wrapper on our syntax
                   e]
                  [else
                   (define s
                     (struct-copy syntax empty-syntax
                                  [content (reader-syntax->syntax (reader:syntax-e v))]
                                  [srcloc (srcloc (reader:syntax-source v)
                                                  (reader:syntax-line v)
                                                  (reader:syntax-column v)
                                                  (reader:syntax-position v)
                                                  (reader:syntax-span v))]
                                  [props (case (reader:syntax-property v 'paren-shape)
                                           [(#\[) original-square-props]
                                           [(#\{) original-curly-props]
                                           [else original-props])]))
                   (define keys (reader:syntax-property-symbol-keys v))
                   (cond
                    [(null? keys) s]
                    [(and (null? (cdr keys)) (eq? (car keys) 'paren-shape)) s]
                    [else (for/fold ([s s]) ([key (in-list keys)])
                            (syntax-property s key (reader:syntax-property v key) #t))])])]
                [else v]))))

(define original-props
  (syntax-props (syntax-property empty-syntax original-property-sym #t)))
(define original-square-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\[)))
(define original-curly-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\{)))
