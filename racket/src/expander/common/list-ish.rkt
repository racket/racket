#lang racket/base
(require (for-syntax racket/base))

;; A `list-ish` is like a `list`, but it can be an "improper list" that
;; doesn't end in null. Using `cons-ish` on an element and `null` returns
;; just the element. A `list-ish` makes sense when lists of length 1
;; would otherwise be common, but only when elements are never lists.

(provide cons-ish
         in-list-ish)

(define (cons-ish a b)
  (if (null? b)
      a
      (cons a b)))

(define-sequence-syntax in-list-ish
  (lambda (stx) (raise-syntax-error #f "only allowed in a `for` form" stx))
  (lambda (stx)
    (syntax-case stx ()
      [[(id) (_ lst-expr)]
       (for-clause-syntax-protect
        #'[(id)
           (:do-in
            ;;outer bindings
            ([(lst) lst-expr])
            ;; outer check
            (void)
            ;; loop bindings
            ([lst lst])
            ;; pos check
            (not (null? lst))
            ;; inner bindings
            ([(id) (if (pair? lst) (car lst) lst)]
             [(rest) (if (pair? lst) (cdr lst) null)])
            ;; pre guard
            #t
            ;; post guard
            #t
            ;; loop args
            (rest))])]
      [_ #f])))
