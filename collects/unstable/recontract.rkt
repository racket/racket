#lang racket/base
(require (for-syntax racket/base
                     racket/provide-transform)
         racket/contract/base
         racket/contract/private/provide
         syntax/location)
(provide provide/recontract
         recontract-out)

;; TODO: support rename, struct (?), etc
;; TODO: check whether this works w/ define/contract, with-contract, etc

(begin-for-syntax
 ;; get-pctx : identifier syntax -> provide/contract-transformer
 (define (get-pctx id ctx)
   (unless (identifier? id)
     (raise-syntax-error #f "expected identifier" ctx id))
   (let ([pctx (syntax-local-value id (lambda () #f))])
     (unless (provide/contract-transformer? pctx)
       ;; FIXME: or should recontracting an uncontracted name be a no-op?
       (raise-syntax-error #f "expected name imported with contract" ctx id))
     pctx)))

(define-syntax (provide/recontract stx)
  (syntax-case stx ()
    [(provide/recontract id ...)
     (let* ([ids (syntax->list #'(id ...))])
       (for ([id (in-list ids)]) (get-pctx id stx)) ;; check for errors
       #'(begin (define pos-mod-src (quote-module-name))
                (provide/recontract1 id pos-mod-src) ...))]))

(define-syntax (provide/recontract1 stx)
  (syntax-case stx ()
    [(provide/recontract1 id pos-mod-src)
     (with-syntax ([(aux-id) (generate-temporaries #'(id))])
       #'(begin
           (define-syntax aux-id
             (let ([old-pctx (get-pctx (quote-syntax id) #f)])
               (replace-provide/contract-transformer-positive-blame
                old-pctx (quote-syntax pos-mod-src))))
           (provide (rename-out [aux-id id]))))]))

;; ----

(define-syntax recontract-out
  (make-provide-pre-transformer
   (lambda (stx modes)
     ;; Adapted from similar check in racket/contract/private/out:
     ;; For now, only work in the base phase ...
     (unless (member modes '(() (0)))
       (raise-syntax-error #f "allowed only in relative phase-level 0" stx))

     ;; FIXME: check for syntax errors

     (syntax-case stx ()
       [(_ . args)
        (syntax-local-lift-module-end-declaration 
         #`(provide/recontract . args))])

     #`(combine-out))))
