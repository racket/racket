
(load-relative "loadtest.rktl")

(Section 'stxparam)

(require racket/stxparam
         racket/splicing)

(define-syntax-parameter tHIs (lambda (stx) #'(quote orig)))
(define-syntax-rule (inDIRECt) tHIs)

(test 'orig values tHIs)
(test 'orig values (inDIRECt))

(test 'sub values (syntax-parameterize ([tHIs (lambda (stx) #'(quote sub))])
                    tHIs))
(test 'sub values (syntax-parameterize ([tHIs (lambda (stx) #'(quote sub))])
                    (inDIRECt)))
(test 'sub values (splicing-syntax-parameterize ([tHIs (lambda (stx) #'(quote sub))])
                    (inDIRECt)))

(module check-splicing-stxparam-1 racket/base
  (require (for-syntax racket/base)
           racket/stxparam
           racket/splicing)
  (define-syntax-parameter sp 'orig)
  (define-syntax (m stx)
    (define v (syntax-parameter-value #'sp))
    (syntax-case stx ()
      [(_ id) #`(define id (quote #,v))]))
  (m x)
  (splicing-syntax-parameterize ([sp 'sub])
    (begin
     (define other 'other)
     (m y)))

  (begin-for-syntax (define sp-val-1 (syntax-parameter-value #'sp)))
  (define-syntax (m1 stx)
    (syntax-case stx ()
      [(_ id) #`(define id (quote #,sp-val-1))]))
  (m1 z)
  
  (splicing-syntax-parameterize ([sp 'sub2])
    (begin-for-syntax (define sp-val-2 (syntax-parameter-value #'sp))))
  (define-syntax (m2 stx)
    (syntax-case stx ()
      [(_ id) #`(define id (quote #,sp-val-2))]))
  (m2 w)

  (splicing-syntax-parameterize ([sp 'unused])
    ;; make sure that `splicing-syntax-parameterize' can
    ;; deal with a variety of compile-time forms
    (begin-for-syntax
     (require racket/base)
     (define x 11)
     (provide x)))

  (define (f)
    (splicing-syntax-parameterize ([sp 'nested])
      (define-syntax m (let ([v (syntax-parameter-value #'sp)])
                         (lambda (stx)
                           #`(quote #,v)))))
    (m))
   
  (define (g)
    (syntax-parameterize ([sp 'hidden])
      (splicing-syntax-parameterize ([sp 'also-nested])
        (define-syntax m (let ([v (syntax-parameter-value #'sp)])
                           (lambda (stx)
                             #`(quote #,v)))))
      (m)))
   
  (provide x y z w f g))

(test 'orig dynamic-require ''check-splicing-stxparam-1 'x)
(test 'sub dynamic-require ''check-splicing-stxparam-1 'y)
(test 'orig dynamic-require ''check-splicing-stxparam-1 'z)
(test 'sub2 dynamic-require ''check-splicing-stxparam-1 'w)
(test 'nested values ((dynamic-require ''check-splicing-stxparam-1 'f)))
(test 'also-nested values ((dynamic-require ''check-splicing-stxparam-1 'g)))

(module check-splicing-stxparam-et racket/base
  (require (for-syntax racket/base)
           'check-splicing-stxparam-1)
  (define-syntax (m stx) (datum->syntax stx x))
  (define q (m))
  (provide q))

(test 11 dynamic-require ''check-splicing-stxparam-et 'q)

;; ----------------------------------------

(report-errs)

