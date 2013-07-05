#lang racket/base
(require (for-syntax racket/base
                     syntax/stx)
         "match-a-pattern.rkt"
         racket/match)

(provide extract-clauses
         extracted-clauses->fns)

(define-for-syntax extracted-conditions #f)
(define-syntax (extract-clauses stx)
  (syntax-case stx (match-a-pattern)
    [(_ (match-a-pattern #:allow-else p clauses ...))
     (begin
       (set! extracted-conditions
             (cons #'p
                   (filter
                    values
                    (map
                     (λ (clause)
                       (syntax-case clause (_)
                         [[cond #f] #f]
                         [[`(name id pat) if-part]
                          [eq? 'name (syntax-e #'name)]
                          #f] ;; skip this here, want bound variant for pat* - added in pat-unify
                         [[_ exp ...] #f] ;; skip the cstr test; that's added elsewhere
                         [[cond not-false ...] #'cond]))
                     (syntax->list #'(clauses ...))))))
       (stx-car (stx-cdr stx)))]))

(define-syntax (extracted-clauses->fns stx)
  (unless extracted-conditions
    (raise-syntax-error #f "no pats extracted"))
  (with-syntax ([(p condition ...) extracted-conditions]
                [(name ...) (map (λ (x) (string->symbol (format "~s" (syntax->datum x))))
                                 (cdr extracted-conditions))])
    #`(list 
       (let ([name (λ (p) (match p [condition #t] [else #f]))])
         name) ...)))
