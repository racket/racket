#lang racket/base

(require (for-syntax racket/base
                     racket/list))

(provide inline-sort)

(define-syntax (inline-sort stx)
  (syntax-case stx ()
    [(_ lt-expr v-exprs ...)
     (with-syntax ([(< vs ...)  (generate-temporaries #'(lt-expr v-exprs ...))])
       (quasisyntax/loc stx
         (let ([< lt-expr] [vs v-exprs] ...)
           #,(inline-sort/k #'(vs ...) #'< (λ (lst) #`(values #,@lst))))))]))

(define-for-syntax (inline-sort/k vs < k)
  (syntax-case vs ()
    [()  (k #'())]
    [(a)  (k #'(a))]
    [_  (let ([vs  (if (list? vs) vs (syntax->list vs))])
          (define-values (lvs rvs) (split-at vs (quotient (length vs) 2)))
          (inline-sort/k 
           lvs <
           (λ (lvs) (inline-sort/k
                     rvs <
                     (λ (rvs) (inline-merge/k lvs rvs < k))))))]))

(define-for-syntax (inline-merge/k as bs < k)
  (syntax-case #`(#,as #,bs) ()
    [(() bs)  (k #'bs)]
    [(as ())  (k #'as)]
    [((a . as) (b . bs))
     #`(if (#,< a b)
           #,(inline-merge/k #'as #'(b . bs) < (λ (vs) (k (cons #'a vs))))
           #,(inline-merge/k #'(a . as) #'bs < (λ (vs) (k (cons #'b vs)))))]))
