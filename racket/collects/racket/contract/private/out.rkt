#lang racket/base
(require (for-syntax racket/base
                     racket/provide-transform)
         "provide.rkt")
(provide contract-out)

(define-syntax contract-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     ;; For now, only work in the base phase of the `contract-out'
     ;; binding. To generalize, we'll need to parameterize `true-provide'
     ;; over the phase where it should match, shift references, and
     ;; shift lifts (by wrapping them with `begin-for-syntax'es).
     (unless (or (null? modes)
                 (and (= 1 (length modes))
                      (zero? (car modes))))
       (raise-syntax-error #f
                           "allowed only in relative phase-level 0"
                           stx))
     
     ;; check for syntax errors
     (true-provide/contract stx #t 'contract-out)
     
     (with-syntax ([(contracted-vars-info) (generate-temporaries '(contracted-vars-info))])
       (syntax-local-lift-module-end-declaration
        #`(handle-contract-out contracted-vars-info #,stx))
       
       #`(provide-contracted-vars contracted-vars-info)))))

(define-syntax (handle-contract-out stx)
  (syntax-case stx ()
    [(_ contracted-vars-info orig-stx)
     (let ()
       (define provide-clauses '())
       (define without-provide-clauses
         (let loop ([stx (true-provide/contract #'orig-stx #f 'contract-out)])
           (syntax-case stx (begin provide)
             [(begin args ...)
              #`(begin #,@(map loop (syntax->list #'(args ...))))]
             [(provide clause ...)
              (identifier? #'x)
              (begin (set! provide-clauses (append (syntax->list #'(clause ...))
                                                   provide-clauses))
                     #'(begin))]
             [x stx])))
       #`(begin
           #,without-provide-clauses
           (define-syntax contracted-vars-info (quote-syntax #,provide-clauses))))]))
  
(define-syntax provide-contracted-vars 
  (make-provide-transformer
   (λ (stx modes)
     (define contracted-vars-info
       (syntax-case stx ()
         [(_ id) #'id]))
     (for*/list ([provide-clause (in-list (syntax->list (syntax-local-value contracted-vars-info)))]
                 [export (in-list (expand-export provide-clause modes))])
       export))))
