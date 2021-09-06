#lang racket/base

(require racket/private/stxcase-scheme
         racket/private/qqstx
         (for-syntax racket/base
                     racket/private/sc))

(provide datum datum-case with-datum define/with-datum
         quasidatum undatum undatum-splicing)

(define-syntax (define/with-datum stx)
  (syntax-case stx ()
    [(define/with-datum pattern rhs)
     (let ()
       (define pvar-env
         (get-match-vars #'define/with-datum
                         stx
                         #'pattern
                         '()))
       (define-values (pvars depths)
         (for/lists (pvars depths)
                    ([x (in-list pvar-env)])
           (let loop ([x x] [depth 0])
             (cond
               [(pair? x) (loop (car x) (add1 depth))]
               [else      (values x depth)]))))
       (with-syntax ([(pvar ...) pvars]
                     [(depth ...) depths]
                     [(valvar ...) (generate-temporaries pvars)])
         #'(begin (define-values (valvar ...)
                    (with-datum ([pattern rhs])
                      (values (pvar-value pvar) ...)))
                  (define-syntax pvar
                    (make-s-exp-mapping 'depth (quote-syntax valvar)))
                  ...)))]))

;; auxiliary macro
(define-syntax (pvar-value stx)
  (syntax-case stx ()
    [(_ pvar)
     (identifier? #'pvar)
     (let ([mapping (syntax-local-value #'pvar)])
       (unless (s-exp-pattern-variable? mapping)
         (raise-syntax-error #f "not a datum pattern variable" #'pvar))
       (s-exp-mapping-valvar mapping))]))
