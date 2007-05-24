(module elim-call-cc mzscheme
  (require "syntax-utils.ss")
  (require-for-template "abort-resume.ss" mzscheme)
  (provide elim-call/cc-from-definition
           elim-call/cc)
  
  ;; **************************************************
  ;; LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var) expr)
  ;;
  ;; expr ::= w
  ;;       |  (if w expr)
  ;;       |  (if w expr expr)
  ;;       |  (#%app w expr) ;where expr != w
  ;;       |  (#%app w w ...)
  ;;       |  (#%app call/cc w)
  ;;
  ;;   w  ::= var | (#%top . var) | value
  ;;   value ::=  (#%datum . datum)
  ;;          | (lambda (var ...) expr)
  
  ;; id: alpha -> alpha
  (define (id x) x)
  
  ;; elim-call/cc: expr -> expr
  ;; eliminate call/cc from an expression
  (define (elim-call/cc expr)
     (elim-call/cc/mark expr id))
   
  ;; elim-call/cc/mark: expr (expr -> expr) -> expr
  ;; eliminate call/cc from an expression given a mark frame function
  (define (elim-call/cc/mark expr markit)
    (syntax-case expr (if #%app call/cc #%top #%datum lambda quote)
      [(if w e)
       (with-syntax ([(w e) (recertify* (list #'w #'e) expr)])
         (markit #`(if #,(elim-call/cc #'w) #,(elim-call/cc #'e))))]
      [(if w e0 e1)
       (with-syntax ([(w e0 e1) (recertify* (list #'w #'e0 #'e1) expr)])
         (markit #`(if #,(elim-call/cc #'w)
                       #,(elim-call/cc #'e0)
                       #,(elim-call/cc #'e1))))]
      [(#%app call/cc w)
       (with-syntax ([w (recertify #'w expr)])
         (let-values ([(cm ref-to-cm) (generate-formal 'current-marks)]
                      [(x ref-to-x) (generate-formal 'x)])
           (markit #`(#%app #,(elim-call/cc #'w)
                            (#%app (lambda (#,cm)
                                     (lambda (#,x)
                                       (#%app abort
                                              (lambda () (#%app resume #,ref-to-cm (#%app list #,ref-to-x))))))
                                   (#%app activation-record-list))))))]
      ;; this is (w e) where e is not a w. (w w) handled in next case.
      ;; m00.4 in persistent-interaction-tests.ss tests this distinction
      [(#%app w (#%app . stuff))
       (with-syntax ([e #'(#%app . stuff)])
         (with-syntax ([(w e) (recertify* (list #'w #'e) expr)])
           (syntax-case #'w (lambda)
             [(lambda (formals ...) body)
              (let ([w-prime (datum->syntax-object #f (gensym 'f))])
                #`(let-values ([(#,w-prime) #,(elim-call/cc #'w)])
                    #,(markit
                       #`(#%app #,w-prime
                                #,(elim-call/cc/mark
                                   #'e
                                   (lambda (x)
                                     #`(with-continuation-mark the-cont-key #,w-prime #,x)))))))]
             [_else
              (let ([w-prime (elim-call/cc #'w)])
                (markit
                 #`(#%app #,w-prime
                          #,(elim-call/cc/mark
                             #'e 
                             (lambda (x)
                               #`(with-continuation-mark the-cont-key #,w-prime #,x))))))])))]
      [(#%app w rest ...)
       (with-syntax ([(w rest ...) (recertify* (syntax->list #'(w rest ...)) expr)])
        (markit
         #`(with-continuation-mark safe-call? #f
             (#%app #,(mark-lambda-as-safe (elim-call/cc #'w))
                    #,@(map 
                        (lambda (an-expr)
                          (mark-lambda-as-safe
                           (elim-call/cc
                            an-expr)))
                        (syntax->list #'(rest ...)))))))]
      [(#%top . var) expr]
      [(#%datum . d) expr]
      [(lambda (formals ...) body)
       (with-syntax ([body (recertify #'body expr)])
         #`(lambda (formals ...) #,(elim-call/cc #'body)))]
      [(quote datum) expr]
      [x (symbol? (syntax-object->datum #'x)) expr]
      [_else
       (raise-syntax-error #f "elim-call/cc/mark dropped through" expr)]))
  
  ;; elim-call/cc-from-definition: definition -> definition
  ;; produce a transformed defintion
  (define (elim-call/cc-from-definition def)
    (syntax-case def ()
      [(define-values (var ...) expr)
       #`(define-values (var ...) #,(mark-lambda-as-safe (elim-call/cc #'expr)))]
      [else
       (raise-syntax-error #f "elim-call/cc-from-definition dropped through" def)]))
  
  ;; mark-lambda-as-safe: w -> w
  ;; If w is a lambda-expression then add #t to the safety mark, otherwise no mark
  (define (mark-lambda-as-safe w)
    (syntax-case w (lambda)
      [(lambda (formals ...) body)
       #`(lambda (formals ...)
           (with-continuation-mark safe-call? #t
             body))]
      [_else w])))