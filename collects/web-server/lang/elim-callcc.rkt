#lang racket/base
(require (for-template racket/base)
         syntax/kerncase
         racket/contract
         "abort-resume.rkt"
         (for-template "abort-resume.rkt")
         "util.rkt")
(provide/contract
 [elim-callcc (syntax? . -> . syntax?)])

(define (id x) x)

;; mark-lambda-as-safe: w -> w
;; If w is a lambda-expression then add #t to the safety mark, otherwise no mark
(define (mark-lambda-as-safe w)
  (rearm
   w
   (syntax-case (disarm w) (#%plain-lambda case-lambda)
     [(#%plain-lambda formals be ...)
      (syntax/loc w
        (#%plain-lambda formals
                        (with-continuation-mark safe-call? '(#t (lambda formals))
                          be ...)))]
     [(case-lambda [formals be ...] ...)
      (syntax/loc w
        (case-lambda [formals 
                      (with-continuation-mark safe-call? '(#t (case-lambda formals ...))
                        be ...)] ...))]
     [_else w])))

(define (elim-callcc stx)
  (elim-callcc/mark id stx))

(define (elim-callcc/mark markit stx)  
  (rearm
   stx
   (kernel-syntax-case*
    (disarm stx) (transformer?) (call/cc call-with-values)
    [(begin be ...)
     (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
    [(begin0 be ...)
     (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
    [(set! v ve)
     (with-syntax ([ve (elim-callcc #'ve)])
       (syntax/loc stx (set! v ve)))]
    [(let-values ([(v ...) ve] ...) be ...)
     (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
    [(letrec-values ([(v ...) ve] ...) be ...)
     (raise-syntax-error 'elim-callcc/mark "Not in ANF" stx)]
    [(#%plain-lambda formals be)
     (mark-lambda-as-safe
      (with-syntax ([be (elim-callcc #'be)])
        (syntax/loc stx
          (#%plain-lambda formals be))))]
    [(case-lambda [formals be] ...)
     (mark-lambda-as-safe
      (with-syntax ([(be ...) (map elim-callcc (syntax->list #'(be ...)))])
        (syntax/loc stx
          (case-lambda [formals be] ...))))]
    [(if te ce ae)
     (with-syntax ([te (elim-callcc #'te)]
                   [ce (elim-callcc #'ce)]
                   [ae (elim-callcc #'ae)])
       (markit (syntax/loc stx (if te ce ae))))]
    [(quote datum)
     stx]
    [(quote-syntax datum)
     stx]    
    [(with-continuation-mark ke me be)
     (let* ([ke-prime (elim-callcc #'ke)]
            [me-prime (elim-callcc #'me)]
            [be-prime (elim-callcc #'be)])
       ; Could be dangerous to evaluate ke-prime and me-prime twice (but remember, this is in ANF)
       (markit 
        (quasisyntax/loc stx
          (with-continuation-mark #,ke-prime #,me-prime
            (#%plain-app with-current-saved-continuation-marks-and #,ke-prime #,me-prime
                         (#%plain-lambda () #,be-prime))))))]
    [(#%plain-app call/cc w)
     (let-values ([(cm ref-to-cm) (generate-formal 'current-marks stx)]
                  [(x ref-to-x) (generate-formal 'x stx)])
       (markit 
        (quasisyntax/loc stx
          (#%plain-app 
           #,(elim-callcc #'w)
           (#%plain-app 
            (#%plain-lambda 
             (#,cm)
             (#%plain-lambda #,x
                             (#%plain-app abort
                                          ; XXX Do I need to rebuild the CMs?
                                          (#%plain-lambda () (#%plain-app resume #,ref-to-cm #,ref-to-x)))))
            (#%plain-app activation-record-list))))))]
    [(#%plain-app call-with-values (#%plain-lambda () prod) cons)
     (let-values ([(consumer ref-to-consumer) (generate-formal 'consumer stx)])
       (quasisyntax/loc stx
         (let-values ([(#,consumer) #,(mark-lambda-as-safe (elim-callcc #'cons))])
           #,(markit
              (quasisyntax/loc stx
                (#%plain-app 
                 call-with-values 
                 #,(mark-lambda-as-safe
                    (quasisyntax/loc stx
                      (#%plain-lambda ()
                                      #,(elim-callcc/mark
                                         (lambda (x)
                                           (quasisyntax/loc stx
                                             (with-continuation-mark the-cont-key #,ref-to-consumer #,x)))
                                         #'prod))))
                 #,ref-to-consumer))))))]
    [(#%plain-app w (#%plain-app . stuff))
     (with-syntax ([e #'(#%plain-app . stuff)])
       (syntax-case (disarm #'w) (#%plain-lambda case-lambda)
         [(#%plain-lambda formals body)
          (rearm
           #'w
           (let-values ([(w-prime ref-to-w-prime) (generate-formal 'l stx)])
             (quasisyntax/loc stx
               (let-values ([(#,w-prime) #,(elim-callcc #'w)])
                 #,(markit
                    (quasisyntax/loc stx
                      (#%plain-app #,ref-to-w-prime
                                   #,(elim-callcc/mark
                                      (lambda (x)
                                        (quasisyntax/loc stx
                                          (with-continuation-mark the-cont-key #,ref-to-w-prime #,x)))
                                      #'e))))))))]
         [(case-lambda [formals body] ...)
          (rearm
           #'w
           (let-values ([(w-prime ref-to-w-prime) (generate-formal 'cl stx)])
             (quasisyntax/loc stx
               (let-values ([(#,w-prime) #,(elim-callcc #'w)])
                 #,(markit
                    (quasisyntax/loc stx
                      (#%plain-app #,ref-to-w-prime
                                   #,(elim-callcc/mark
                                      (lambda (x)
                                        (quasisyntax/loc stx
                                          (with-continuation-mark the-cont-key #,ref-to-w-prime #,x)))
                                      #'e))))))))]
         [_else
          (let-values ([(w-prime ref-to-w-prime) (generate-formal 'other stx)])
            (quasisyntax/loc stx
              (let ([#,w-prime #,(elim-callcc #'w)])
                (markit
                 (quasisyntax/loc stx
                   (#%plain-app #,ref-to-w-prime
                                #,(elim-callcc/mark
                                   (lambda (x)
                                     #`(with-continuation-mark the-cont-key #,ref-to-w-prime #,x))
                                   #'e)))))))]))]
    [(#%plain-app w rest ...)
     (markit
      (quasisyntax/loc stx
        (with-continuation-mark safe-call? '(#f stx)
          (#%plain-app #,(mark-lambda-as-safe (elim-callcc #'w))
                       #,@(map 
                           (lambda (an-expr)
                             (mark-lambda-as-safe
                              (elim-callcc
                               an-expr)))
                           (syntax->list #'(rest ...)))))))]
    [(#%top . v)
     stx]
    [(#%variable-reference . v)
     stx]       
    [id (identifier? #'id)
        stx]    
    [(#%expression d)
     (markit (quasisyntax/loc stx (#%expression #,(elim-callcc #'d))))]
    [_
     (raise-syntax-error 'elim-callcc "Dropped through:" stx)])))
