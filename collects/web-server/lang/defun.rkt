#lang racket/base
(require (for-template racket/base)
         syntax/kerncase
         racket/contract
         web-server/lang/closure
         (for-template web-server/lang/serial-lambda)
         "util.rkt")
(provide/contract
 [defun (syntax? . -> . syntax?)])

; make-new-clouse-label : (syntax -> syntax) syntax -> syntax
(define (make-new-closure-label labeling stx)
  (labeling stx))

; defun : syntax[1] -> (values syntax?[2] (listof syntax?)[3])
; defunctionalizes the first syntax, returning the second and the lifted lambdas [3]
(define (defun stx)  
  (rearm
   stx
   (kernel-syntax-case
       (disarm stx) (transformer?)
     [(begin be ...)
      (let-values ([(nbes) (defun* (syntax->list #'(be ...)))])
        (quasisyntax/loc stx (begin #,@nbes)))]
     [(begin0 be ...)
      (let-values ([(nbes) (defun* (syntax->list #'(be ...)))])
        (quasisyntax/loc stx (begin0 #,@nbes)))]
     [(set! v ve)
      (let-values ([(nve) (defun #'ve)])
        (quasisyntax/loc stx (set! v #,nve)))]
     [(let-values ([(v ...) ve] ...) be ...)
      (let-values ([(nves) (defun* (syntax->list #'(ve ...)))]
                   [(nbes) (defun* (syntax->list #'(be ...)))])
        (with-syntax ([(nve ...) nves]
                      [(nbe ...) nbes])
          (syntax/loc stx (let-values ([(v ...) nve] ...) nbe ...))))]
     [(letrec-values ([(v ...) ve] ...) be ...)
      (let-values ([(nves) (defun* (syntax->list #'(ve ...)))]
                   [(nbes) (defun* (syntax->list #'(be ...)))])
        (with-syntax ([(nve ...) nves]
                      [(nbe ...) nbes])
          (syntax/loc stx (letrec-values ([(v ...) nve] ...) nbe ...))))]
     [(#%plain-lambda formals be ...)
      (let-values ([(nbes) (defun* (syntax->list #'(be ...)))])
        (with-syntax ([(nbe ...) nbes])
          (syntax/loc stx
            (serial-lambda formals nbe ...))
          #;
          (make-closure
           (quasisyntax/loc stx
             (_ #,(make-new-closure-label (current-code-labeling) stx) (#%plain-lambda formals nbe ...))))))]
     [(case-lambda [formals be ...] ...)       
      (let-values ([(nbes) (defun** (syntax->list #'((be ...) ...)))])
        (with-syntax ([((nbe ...) ...) nbes])
          (syntax/loc stx
            (serial-case-lambda
             [formals nbe ...]
             ...))
          #;
          (make-closure
           (quasisyntax/loc stx
             (_ #,(make-new-closure-label (current-code-labeling) stx) (case-lambda [formals nbe ...] ...))))))]
     [(if te ce ae)
      (let-values ([(es) (defun* (syntax->list #'(te ce ae)))])
        (quasisyntax/loc stx (if #,@es)))]
     [(quote datum)
      stx]
     [(quote-syntax datum)
      stx]
     [(with-continuation-mark ke me be)
      (let-values ([(es) (defun* (list #'ke #'me #'be))])
        (quasisyntax/loc stx (with-continuation-mark #,@es)))]
     [(#%plain-app e ...)
      (let-values ([(es) (defun* (syntax->list #'(e ...)))])
        (quasisyntax/loc stx (#%plain-app #,@es)))]
     [(#%top . v)
      stx]
     [(#%variable-reference . v)
      stx]
     [id (identifier? #'id)
         stx]       
     [(#%expression d)
      (let-values ([(nd) (defun #'d)])
        (quasisyntax/loc stx (#%expression #,nd)))]
     [_
      (raise-syntax-error 'defun "Dropped through:" stx)])))

; lift defun to list of syntaxes
(define (lift-defun defun)
  (lambda (stxs)
    (map defun stxs)))
(define defun* (lift-defun defun))
(define defun** (lift-defun (lambda (stx) (defun* (syntax->list stx)))))
