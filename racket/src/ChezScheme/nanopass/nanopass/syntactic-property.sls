;;; Copyright (c) 2000-2018 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

;; implements a global association list from bound-identifiers to property
;; lists property lists are themselves assogiation lists from free-identifiers
;; to values.
(library (nanopass syntactic-property)
  (export syntax-property-set! syntax-property-get)
  (import (rnrs))

  (define-record-type ($box box box?) (nongenerative) (fields (mutable v unbox box-set!)))

  (define props (box '()))

  (define syntax-property-set!
    (lambda (id key value)
      (box-set! props
        (let f ([props (unbox props)])
          (if (null? props)
              (list (cons id (list (cons key value))))
              (let ([as (car props)] [props (cdr props)])
                (if (bound-identifier=? (car as) id)
                    (cons (cons id (cons (cons key value) (cdr as))) props)
                    (cons as (f props)))))))))

  (define syntax-property-get
    (case-lambda
      [(id key)
       (let loop ([props (unbox props)])
         (if (null? props)
             (error 'syntax-property-get "no properties for ~s found" (syntax->datum id))
             (let ([as (car props)] [props (cdr props)])
               (if (bound-identifier=? (car as) id)
                   (let loop ([ls (cdr as)])
                     (if (null? ls)
                         (error 'syntax-propert-get "no property ~s for ~s found" (syntax->datum key) (syntax->datum id))
                         (let ([as (car ls)] [ls (cdr ls)])
                           (if (free-identifier=? (car as) key)
                               (cdr as)
                               (loop ls)))))
                   (loop props)))))]
      [(id key not-found)
       (let loop ([props (unbox props)])
         (if (null? props)
             not-found
             (let ([as (car props)] [props (cdr props)])
               (if (bound-identifier=? (car as) id)
                   (let loop ([ls (cdr as)])
                     (if (null? ls)
                         not-found
                         (let ([as (car ls)] [ls (cdr ls)])
                           (if (free-identifier=? (car as) key)
                               (cdr as)
                               (loop ls)))))
                   (loop props)))))])))
