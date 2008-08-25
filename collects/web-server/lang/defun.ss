#lang scheme/base
(require (for-template scheme/base)
         syntax/kerncase
         syntax/free-vars
         scheme/contract
         mzlib/list
         mzlib/plt-match
         "util.ss"
         "../private/closure.ss")
(provide/contract
 [defun (syntax? . -> . (values syntax? (listof syntax?)))])

; make-new-clouse-label : (syntax -> syntax) syntax -> syntax
(define (make-new-closure-label labeling stx)
  (labeling stx))

; defun : syntax[1] -> (values syntax?[2] (listof syntax?)[3])
; defunctionalizes the first syntax, returning the second and the lifted lambdas [3]
(define (defun stx)  
  (recertify/new-defs
   stx
   (lambda ()
     (kernel-syntax-case
         stx (transformer?)
       [(begin be ...)
        (let-values ([(nbes defs) (defun* (syntax->list #'(be ...)))])
          (values (quasisyntax/loc stx (begin #,@nbes))
                  defs))]
       [(begin0 be ...)
        (let-values ([(nbes defs) (defun* (syntax->list #'(be ...)))])
          (values (quasisyntax/loc stx (begin0 #,@nbes))
                  defs))]
       [(set! v ve)
        (let-values ([(nve defs) (defun #'ve)])
          (values (quasisyntax/loc stx (set! v #,nve))
                  defs))]
       [(let-values ([(v ...) ve] ...) be ...)
        (let-values ([(nves ve-defs) (defun* (syntax->list #'(ve ...)))]
                     [(nbes be-defs) (defun* (syntax->list #'(be ...)))])
          (with-syntax ([(nve ...) nves]
                        [(nbe ...) nbes])
            (values (syntax/loc stx (let-values ([(v ...) nve] ...) nbe ...))
                    (append ve-defs be-defs))))]
       [(letrec-values ([(v ...) ve] ...) be ...)
        (let-values ([(nves ve-defs) (defun* (syntax->list #'(ve ...)))]
                     [(nbes be-defs) (defun* (syntax->list #'(be ...)))])
          (with-syntax ([(nve ...) nves]
                        [(nbe ...) nbes])
            (values (syntax/loc stx (letrec-values ([(v ...) nve] ...) nbe ...))
                    (append ve-defs be-defs))))]
       [(#%plain-lambda formals be ...)
        (let-values ([(nbes be-defs) (defun* (syntax->list #'(be ...)))])
          (with-syntax ([(nbe ...) nbes])
            (let ([fvars (free-vars stx)])
              (let-values ([(make-CLOSURE new-defs)
                            (make-closure-definition-syntax 
                             (make-new-closure-label (current-code-labeling) stx)
                             fvars 
                             (syntax/loc stx (#%plain-lambda formals nbe ...)))])
                (values (if (empty? fvars)
                            (quasisyntax/loc stx (#,make-CLOSURE))
                            (quasisyntax/loc stx (#,make-CLOSURE (#%plain-lambda () (values #,@fvars)))))
                        (append be-defs new-defs))))))]
       [(case-lambda [formals be ...] ...)       
        (let-values ([(nbes be-defs) (defun** (syntax->list #'((be ...) ...)))])
          (with-syntax ([((nbe ...) ...) nbes])
            (let ([fvars (free-vars stx)])
              (let-values ([(make-CLOSURE new-defs)
                            (make-closure-definition-syntax 
                             (make-new-closure-label (current-code-labeling) stx)
                             fvars 
                             (syntax/loc stx (case-lambda [formals nbe ...] ...)))])
                (values (if (empty? fvars)
                            (quasisyntax/loc stx (#,make-CLOSURE))
                            (quasisyntax/loc stx (#,make-CLOSURE (lambda () (values #,@fvars)))))
                        (append be-defs new-defs))))))]
       [(if te ce ae)
        (let-values ([(es defs) (defun* (syntax->list #'(te ce ae)))])
          (values (quasisyntax/loc stx (if #,@es))
                  defs))]
       [(quote datum)
        (values stx
                empty)]
       [(quote-syntax datum)
        (values stx
                empty)]
       [(with-continuation-mark ke me be)
        (let-values ([(es defs) (defun* (list #'ke #'me #'be))])
          (values (quasisyntax/loc stx (with-continuation-mark #,@es))
                  defs))]       
       [(#%plain-app e ...)
        (let-values ([(es defs) (defun* (syntax->list #'(e ...)))])
          (values (quasisyntax/loc stx (#%plain-app #,@es))
                  defs))]
       [(#%top . v)
        (values stx
                empty)]
       [(#%variable-reference . v)
        (values stx
                empty)]
       [id (identifier? #'id)
           (values stx
                   empty)]
       ; XXX Shouldn't
       [(letrec-syntaxes+values ([(sv ...) se] ...)
          ([(vv ...) ve] ...)
          be ...)
        (let-values ([(nses se-defs) (defun* (syntax->list #'(se ...)))]
                     [(nves ve-defs) (defun* (syntax->list #'(ve ...)))]
                     [(nbes be-defs) (defun* (syntax->list #'(be ...)))])
          (with-syntax ([(nse ...) nses]
                        [(nve ...) nves]
                        [(nbe ...) nbes])
            (values (syntax/loc stx 
                      (letrec-syntaxes+values ([(sv ...) nse] ...)
                        ([(vv ...) nve] ...)
                        nbe ...))
                    (append se-defs ve-defs be-defs))))]
       [(#%expression d)
        (let-values ([(nd d-defs) (defun #'d)])
          (values (quasisyntax/loc stx (#%expression #,nd))
                  d-defs))]
       [_
        (raise-syntax-error 'defun "Dropped through:" stx)]))))

; lift defun to list of syntaxes
(define (lift-defun defun)
  (lambda (stxs)
    (match 
        (foldl (lambda (stx acc)
                 (let-values ([(nstx stx-defs) (defun stx)])
                   (match acc
                     [(list-rest nstxs defs)
                      (cons (list* nstx nstxs)
                            (append stx-defs defs))])))
               (cons empty empty)
               stxs)
      [(list-rest nstxs defs)
       (values (reverse nstxs)
               defs)])))
(define defun* (lift-defun defun))
(define defun** (lift-defun (lambda (stx) (defun* (syntax->list stx)))))
