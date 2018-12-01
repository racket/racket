#lang racket/base
(require (for-syntax racket/base))

;; All patterns for an interpreter matcher are vectors,
;; and each vector element is an unquote or a symbol
;; to match literally.

(provide interp-match)

(define-syntax interp-match
  (syntax-rules ()
    [(_ e)
     (let ([v e])
       (error 'interp-match "no matching clause"))]
    [(_ e [pat . rhs] . clauses)
     (let ([v e])
       (if (matches? v pat)
           (let-vars v pat . rhs)
           (interp-match v . clauses)))]))

(define-syntax (matches? stx)
  (syntax-case stx ()
    [(_ v #(elem ...))
     #`(and #,@(for/list ([e (in-list (syntax->list #'(elem ...)))]
                          [i (in-naturals)])
                 (syntax-case e (unquote)
                   [,id #'#t]
                   [s #`(eq? 's (vector*-ref v #,i))])))]))

(define-syntax (let-vars stx)
  (syntax-case stx ()
    [(_ v #(elem ...) . body)
     #`(let #,(for/list ([e (in-list (syntax->list #'(elem ...)))]
                         [i (in-naturals)]
                         #:when (syntax-case e (unquote)
                                  [,id #t]
                                  [_ #f]))
                (syntax-case e (unquote)
                  [,id #`[id (vector*-ref v #,i)]]))
         . body)]))
