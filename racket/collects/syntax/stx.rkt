#lang racket/base

;; These utilities facilitate operations on syntax objects.
;; A syntax object that represents a parenthesized sequence
;; can contain a mixture of cons cells and syntax objects,
;; hence the need for `stx-null?', `stx-car', etc.

(require racket/private/stx)

(provide 
 ;; from racket/private/stx
 stx-null? stx-pair? stx-list?
 stx-car stx-cdr stx->list
 ;; defined here        
 stx-map module-or-top-identifier=?)

(define (stx-map f . stxls)
  (for ([stxl (in-list stxls)]
        [i (in-naturals)])
    (unless (stx-list? stxl)
      (apply raise-type-error 'stx-map "stx-list" i stxls)))
  (apply map f (map stx->list stxls)))

(define (module-or-top-identifier=? a b)
  (or (free-identifier=? a b)
      (and (eq? (syntax-e a) (syntax-e b))
           (free-identifier=? a
                              (datum->syntax
                               #f
                               (syntax-e b))))))
