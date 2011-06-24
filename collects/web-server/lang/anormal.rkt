#lang racket/base
(require (for-template racket/base)
         syntax/kerncase
         racket/list
         racket/contract
         racket/match
         "util.rkt")
(provide/contract
 [make-anormal-term ((syntax? . -> . syntax?) . -> . (syntax? . -> . syntax?))])

; A-Normal Form
(define (id x) x)

;; a context is either
;;    frame
;;    (ccompose context frame)

;; a frame is either
;;    w -> target-redex
;;    (listof w) -> target-redex

;; ccompose: (w -> target-expr) (alpha -> target-redex) -> (alpha -> target-expr)
;; compose a context with a frame
(define (ccompose ctxt frame)
  (if (eq? ctxt id) 
      frame
      (lambda (val)
        (let-values ([(x ref-to-x) (generate-formal 'x)])
          #`(#%plain-app (#%plain-lambda (#,x) #,(ctxt ref-to-x)) #,(frame val))))))

(define (make-anormal-term elim-letrec-term)
  (define (anormal-term stx)
    (anormal id stx))
  
  (define (anormal ctxt stx)
    (rearm
     stx
     (kernel-syntax-case
         (disarm stx) (transformer?)
       [(begin)
        (anormal ctxt (syntax/loc stx (#%plain-app void)))]
       [(begin lbe)
        (anormal ctxt (syntax/loc stx lbe))]
       [(begin fbe be ...)
        (anormal ctxt 
                 (syntax/loc stx 
                   (#%plain-app call-with-values
                                (#%plain-lambda () fbe)
                                (#%plain-lambda throw-away
                                                (begin be ...)))))]
       [(begin0 lbe)
        (anormal ctxt (syntax/loc stx lbe))]
       [(begin0 fbe be ...)
        (let-values ([(save ref-to-save) (generate-formal 'save)])
          (anormal ctxt 
                   (quasisyntax/loc stx 
                     (#%plain-app call-with-values
                                  (#%plain-lambda () fbe)
                                  (#%plain-lambda #,save
                                                  (begin be ... 
                                                         (#%plain-app apply values #,ref-to-save)))))))]
       [(set! v ve)
        (anormal
         (ccompose ctxt
                   (lambda (val)
                     (quasisyntax/loc stx (set! v #,val))))
         #'ve)]
       [(let-values () be)
        (anormal ctxt (syntax/loc stx be))]
       [(let-values ([(v) ve]) be)
        (anormal ctxt
                 (syntax/loc stx 
                   (#%plain-app (#%plain-lambda (v) be)
                                ve)))]
       [(let-values ([(v ...) ve]) be)
        (anormal ctxt
                 (syntax/loc stx 
                   (#%plain-app call-with-values
                                (#%plain-lambda () ve)
                                (#%plain-lambda (v ...) be))))]
       [(let-values ([(fv ...) fve] [(v ...) ve] ...) be)
        (anormal ctxt
                 (syntax/loc stx 
                   (let-values ([(fv ...) fve])
                     (let-values ([(v ...) ve] ...)
                       be))))]
       [(let-values ([(v ...) ve] ...) be ...)
        (anormal ctxt
                 (syntax/loc stx 
                   (let-values ([(v ...) ve] ...)
                     (begin be ...))))]
       [(letrec-values ([(v ...) ve] ...) be ...)
        (anormal ctxt
                 (elim-letrec-term stx))]
       [(#%plain-lambda formals be ...)
        (with-syntax ([nbe (anormal-term (syntax/loc stx (begin be ...)))])
          (ctxt (syntax/loc stx (#%plain-lambda formals nbe))))]
       [(case-lambda [formals be] ...)
        (with-syntax ([(be ...) (map anormal-term (syntax->list #'(be ...)))])
          (ctxt (syntax/loc stx (case-lambda [formals be] ...))))]
       [(case-lambda [formals be ...] ...)
        (anormal ctxt
                 (syntax/loc stx (case-lambda [formals (begin be ...)] ...)))]
       [(if te ce ae)
        (anormal
         (ccompose ctxt
                   (lambda (val)
                     (quasisyntax/loc stx 
                       (if #,val
                           #,(anormal-term #'ce)
                           #,(anormal-term #'ae)))))
         #'te)]
       [(quote datum)
        (ctxt stx)]
       [(quote-syntax datum)
        (ctxt stx)]
       [(with-continuation-mark ke me be)
        (anormal
         (ccompose ctxt
                   (lambda (kev)
                     (anormal 
                      (lambda (mev)
                        (quasisyntax/loc stx 
                          (with-continuation-mark #,kev #,mev
                            #,(anormal-term #'be))))
                      #'me)))
         #'ke)]       
       [(#%plain-app fe e ...)
        (anormal
         (lambda (val0)
           (anormal*
            (ccompose ctxt
                      (lambda (rest-vals)
                        (quasisyntax/loc stx 
                          (#%plain-app #,val0 #,@rest-vals))))
            (syntax->list #'(e ...))))
         #'fe)]
       [(#%top . v)
        (ctxt stx)]
       [(#%variable-reference . v)
        (ctxt stx)]
       [id (identifier? #'id)
           (ctxt #'id)]
       [(letrec-syntaxes+values ([(sv ...) se] ...)
          ([(vv ...) ve] ...)
          be ...)
        (anormal ctxt (elim-letrec-term stx))]
       [(#%expression d)
        (anormal
         (ccompose ctxt
                   (lambda (d)
                     (quasisyntax/loc stx (#%expression #,d))))
         #'d)]
       [_
        (raise-syntax-error 'anormal "Dropped through:" stx)])))
  
  ;; anormal*: ((listof w) -> target-expr) (listof source-expr) -> target-expr
  ;; normalize an expression given as a context and list of sub-expressions
  (define (anormal* multi-ctxt exprs)
    (match exprs
      [(list) 
       (multi-ctxt '())]
      [(list-rest fe re)
       (anormal
        (lambda (val)
          (anormal*
           (lambda (rest-vals)
             (multi-ctxt (list* val rest-vals)))
           re))
        fe)]))
  
  anormal-term)
