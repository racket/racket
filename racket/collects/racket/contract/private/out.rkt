#lang racket/base
(require (for-syntax racket/base
                     racket/provide-transform)
         "provide.rkt")
(provide contract-out
         reprovide/contract
         recontract-out)

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



(define-for-syntax (check-reprovide-stx-errs stx)
  (syntax-case stx ()
    [(_ id ...)
     (for ([id (in-list (syntax->list #'(id ...)))])
       (define info (and (identifier? id)
                         (syntax-local-value id (λ () #f))))
       (unless (provide/contract-info? info)
         (raise-syntax-error #f
                             "expected an identifier from contract-out"
                             stx id)))]))

(define-syntax (reprovide/contract stx)
  (syntax-case stx ()
    [(_ id ...)
     (begin
       (check-reprovide-stx-errs stx)
       #`(begin
           #,@(for/list ([id (in-list (syntax->list #'(id ...)))])
                (define info (syntax-local-value id))
                (with-syntax ([(uncontracted-id)
                               (generate-temporaries
                                (list (provide/contract-info-original-id info)))])
                  #`(begin
                      (define uncontracted-id #,(provide/contract-info-original-id info))
                      (provide
                       (contract-out [rename
                                      uncontracted-id
                                      #,id
                                      #,(provide/contract-info-contract-id info)])))))))]))

(define-syntax recontract-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     ;; For now, only work in the base phase ... (see contract-out)
     (unless (member modes '(() (0)))
       (raise-syntax-error #f "allowed only in relative phase-level 0" stx))

     (check-reprovide-stx-errs stx)
     (syntax-case stx ()
       [(_ . args)
        (syntax-local-lift-module-end-declaration 
         #`(reprovide/contract . args))])

     #`(combine-out))))
