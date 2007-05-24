(module normalizer mzscheme
  (require "syntax-utils.ss")
  (require-for-template mzscheme)
  (provide normalize-term
           normalize-definition
           )
  ;; **************************************************
  ;; SOURCE LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var ...) expr)
  ;;
  ;; expr ::= var
  ;;       |  (lambda (var ...) expr)
  ;;       |  (if expr expr)
  ;;       |  (if expr expr expr)
  ;;       |  (let-values ([(var)] expr) expr)
  ;;       |  (let-values ([(var ...)] expr) expr)
  ;;       |  (let-values ([(var ...)] expr) expr ...)
  ;;       |  (#%app expr ...)
  ;;       |  (#%datum . datum)
  ;;       |  (#%top . var)
  ;;       |  (begin expr ...)
  ;;       |  (values expr ...)
  
  ;; **************************************************
  ;; TARGET LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var ...) expr)
  ;;
  ;; expr ::= w | r | (#%app (lambda (var) expr) r)
  ;;
  ;; r ::= (if w expr)
  ;;    |  (if w expr expr)
  ;;    |  (#%app w w ...)
  ;;
  ;;   w  ::= var | (#%top . var) | value
  ;;   value ::=  (#%datum . datum)
  ;;          | (lambda (var ...) expr)
  
  ;; **************************************************
  ;; **************************************************
  
  ;; id: alpha -> alpha
  ;; the identity function
  (define (id x) x)
  
  ;; normalize-definition: definition -> expr
  (define (normalize-definition def)
    (syntax-case def (define-values)
      [(define-values (ids ...) body-expr)
       (with-syntax ([body-expr (recertify #'body-expr def)])
         #`(define-values (ids ...) #,(normalize-term #'body-expr)))]
      [_else
       (raise-syntax-error #f "normalize-definition: dropped through" def)]))
  
  ;; normalize-term: source-expr -> target-expr
  ;; transform a term into an application chain
  (define (normalize-term src-expr)
    (normalize id src-expr))
  
  ;; normalize: (w -> target-expr) source-expr -> target-expr
  ;; normalize an expression given as a context and sub-expression
  (define (normalize ctxt expr)
    (syntax-case expr (lambda if let-values #%app #%datum #%top quote begin)
      [(lambda (formals ...) body)
       (with-syntax ([body (recertify #'body expr)])
         (ctxt #`(lambda (formals ...) #,(normalize-term #'body))))]
      [(lambda . anything)
       (raise-syntax-error #f "Not all lambda-expressions supported" expr)]
      [(if tst-expr csq-expr)
       (with-syntax ([(tst-expr csq-expr) (recertify* (list #'tst-expr #'csq-expr) expr)])
         (normalize
          (compose ctxt
                   (lambda (val)
                     #`(if #,val #,(normalize-term #'csq-expr))))
          #'tst-expr))]
      [(if tst-expr csq-expr alt-expr)
       (with-syntax ([(tst-expr csq-expr alt-expr) (recertify* (list #'tst-expr #'csq-expr #'alt-expr) expr)])
         (normalize
          (compose ctxt
                   (lambda (val)
                     #`(if #,val
                           #,(normalize-term #'csq-expr)
                           #,(normalize-term #'alt-expr))))
          #'tst-expr))]
      [(let-values ([(var) rhs-expr]) body)
       (with-syntax ([(rhs-expr body) (recertify* (list #'rhs-expr #'body) expr)])
         (normalize ctxt #'(#%app (lambda (var) body) rhs-expr)))]
      [(let-values ([(vars ...) rhs-expr]) body)
       (with-syntax ([(rhs-expr body) (recertify* (list #'rhs-expr #'body) expr)])
         (normalize ctxt #'(#%app call-with-values
                                  (lambda () rhs-expr)
                                  (lambda (vars ...) body))))]
      [(let-values ([(vars ...) rhs-expr]) body-expr rest-body-exprs ...)
       (with-syntax ([(rhs-expr body-expr rest-body-exprs ...)
                      (recertify* (syntax->list #'(rhs-expr body-expr rest-body-exprs ...)) expr)])
         (normalize ctxt #'(let-values ([(vars ...) rhs-expr])
                             (let-values ([(throw-away) body-expr]) rest-body-exprs ...))))]
      [(#%app expr-rator expr-rands ...)
       (with-syntax ([(expr-rator expr-rands ...)
                      (recertify* (syntax->list #'(expr-rator expr-rands ...)) expr)])
         (normalize
          (lambda (val0)
            (normalize*
             (compose ctxt
                      (lambda (rest-vals)
                        #`(#%app #,val0 #,@rest-vals)))
             (syntax->list #'(expr-rands ...))))
          #'expr-rator))]
      [(#%datum . datum) (ctxt expr)]
      [(#%top . var) (ctxt expr)]
      [(begin) (normalize ctxt #'(#%app (#%top . void)))]
      [(begin last-expr)
       (with-syntax ([last-expr (recertify #'last-expr expr)])
         (normalize ctxt #'last-expr))]
      [(begin first-expr rest-exprs ...)
       (with-syntax ([(first-expr rest-exprs ...)
                      (recertify* (syntax->list #'(first-expr rest-exprs ...)) expr)])
         (normalize ctxt #'(let-values ([(throw-away) first-expr])
                             (begin rest-exprs ...))))]
      [(quote datum) (ctxt expr)]
      [x (identifier? #'x)
         (ctxt expr)]
      [_else
       (raise-syntax-error #f "normalize: unsupported form" expr)]))
  
  ;; normalize*: ((listof w) -> target-expr) (listof source-expr) -> target-expr
  ;; normalize an expression given as a context and list of sub-expressions
  (define (normalize* multi-ctxt exprs)
    (cond
      [(null? exprs) (multi-ctxt '())]
      [else
       (normalize
        (lambda (val)
          (normalize*
           (lambda (rest-vals)
             (multi-ctxt (cons val rest-vals)))
           (cdr exprs)))
        (car exprs))]))
    
  ;; a context is either
  ;;    frame
  ;;    (compose context frame)
  
  ;; a frame is either
  ;;    w -> target-redex
  ;;    (listof w) -> target-redex
  
  ;; compose: (w -> target-expr) (alpha -> target-redex) -> (alpha -> target-expr)
  ;; compose a context with a frame
  (define (compose ctxt frame)
    (if (eq? ctxt id) frame
        (lambda (val)
          (let-values ([(x ref-to-x) (generate-formal 'x)])
            #`(#%app (lambda (#,x) #,(ctxt ref-to-x)) #,(frame val))))))
  )

