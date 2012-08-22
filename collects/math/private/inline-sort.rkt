#lang racket/base

(require (for-syntax racket/base))

(provide inline-sort)

(define-syntax (inline-sort stx)
  (syntax-case stx ()
    [(_ < vs ...)
     (with-syntax ([(as ...)  (generate-temporaries #'(vs ...))]
                   [(lt-binding ...)  (if (identifier? #'<) #'() #'([lt? <]))]
                   [lt?  (if (identifier? #'<) #'< #'lt?)])
       (quasisyntax/loc stx
         (let (lt-binding ... [as vs] ...)
           #,(inline-sort/k (λ (vs) #`(values #,@vs))
                            #'lt?
                            #'(as ...)))))]))

(begin-for-syntax
  (require racket/list)
  
  (define (inline-sort/k k < vs)
    (syntax-case vs ()
      [()  (k #'())]
      [(a)  (k #'(a))]
      [(a b)  #`(if (#,< a b) #,(k #'(a b)) #,(k #'(b a)))]
      [(vs ...)
       (let ([vs  (syntax->list #'(vs ...))])
         (define n (length vs))
         (define left-vs (take vs (quotient n 2)))
         (define right-vs (drop vs (quotient n 2)))
         (inline-sort/k (λ (lvs) (inline-sort/k (λ (rvs) (merge/k k < lvs rvs)) < right-vs))
                        <
                        left-vs))]))
  
  (define (merge/k k < as bs)
    (syntax-case #`(#,as #,bs) ()
      [(() bs)  (k #'bs)]
      [(as ())  (k #'as)]
      [((a . as) (b . bs))
       #`(if (#,< a b)
             #,(merge/k (λ (vs) (k (cons #'a vs))) < #'as #'(b . bs))
             #,(merge/k (λ (vs) (k (cons #'b vs))) < #'(a . as) #'bs))]))
  )
