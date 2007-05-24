(module elim-letrec mzscheme
  (require "syntax-utils.ss")
  (require-for-template "abort-resume.ss" mzscheme)
  (provide elim-letrec
           elim-letrec-from-definition)
  
  
  ;; **************************************************
  ;; SOURCE LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var ...) expr)
  ;;
  ;; expr ::= var
  ;;       |  (letrec-values ([(var) expr] ...) expr)
  ;;       |  (lambda (var ...) expr)
  ;;       |  (if expr expr)
  ;;       |  (if expr expr expr)
  ;;       |  (let-values ([(var ...)] expr) expr)
  ;;       |  (#%app expr ...)
  ;;       |  (#%datum . datum)
  ;;       |  (#%top . var)
  ;;       |  (begin expr ...)
  ;;
  ;; NOTES: (1) Assumes fully expanded code.
  ;;        (2) For now just single vars on the RHS of a letrec values.

  ;; **************************************************
  ;; TARGET LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var ...) expr)
  ;;
  ;; expr ::= var
  ;;       |  (lambda (var ...) expr)
  ;;       |  (if expr expr)
  ;;       |  (if expr expr expr)
  ;;       |  (let-values ([(var ...)] expr) expr)
  ;;       |  (#%app expr ...)
  ;;       |  (#%datum . datum)
  ;;       |  (#%top . var)
  ;;       |  (begin expr ...)
  
  ;; elim-letrec-from-definition: definition -> expr
  (define (elim-letrec-from-definition def)
    (syntax-case def (define-values)
      [(define-values (ids ...) body-expr)
       #`(define-values (ids ...) #,(elim-letrec #'body-expr))]
      [_else
       (raise-syntax-error #f "elim-letrec-from-definition: dropped through" def)]))
  
  ;; elim-letrec: source-expr -> target-expr
  ;; eliminate all occurences of letrec-values from the source expression
  (define (elim-letrec src-expr)
    (elim-letrec/ids src-expr '()))
  
  ;; elim-letrec/ids: source-expr (listof identifier) -> target-expr
  ;; eliminate letrec-values and make substitutions for the indicated ids
  ;; substitute x ---> (unbox x), (set! x expr) (set-box! x expr)
  (define (elim-letrec/ids expr ids)
    (syntax-case expr (lambda letrec-values if let-values #%app #%datum #%top quote begin set!)
      [(letrec-values ([(vars) rhss] ...) body-expr)
       (with-syntax ([(body-expr rhss ...) (recertify* (syntax->list #'(body-expr rhss ...)) expr)])
         (let ([ids (append (syntax->list #'(vars ...)) ids)])
           (with-syntax ([(new-rhss ...)
                          (map
                           (lambda (rhs)
                             (elim-letrec/ids rhs ids))
                           (syntax->list #'(rhss ...)))]
                         [new-body (elim-letrec/ids #'body-expr ids)])
             #`(let-values ([(vars ...) (#%app values
                                               #,@(map
                                                   (lambda (x) #'(#%app box the-undef))
                                                   (syntax->list #'(vars ...))))])
                 (begin
                   (#%app set-box! vars new-rhss) ...
                   new-body)))))]
      [(letrec-values . anything)
       (raise-syntax-error #f "Not all letrec-values-expressions supported" expr)]
      [(lambda (formals ...) body)
       (with-syntax ([body (recertify #'body expr)])
         #`(lambda (formals ...) #,(elim-letrec/ids #'body ids)))]
      [(lambda . anything)
       (raise-syntax-error #f "Not all lambda-expressions supported" expr)]
      [(if tst-expr csq-expr)
       (with-syntax ([(tst-expr csq-expr) (recertify* (list #'tst-expr #'csq-expr) expr)])
         #`(if #,(elim-letrec/ids #'tst-expr ids)
               #,(elim-letrec/ids #'csq-expr ids)))]
      [(if tst-expr csq-expr alt-expr)
       (with-syntax ([(tst-expr csq-expr alt-expr) (recertify* (list #'tst-expr #'csq-expr #'alt-expr) expr)])
         #`(if #,(elim-letrec/ids #'tst-expr ids)
               #,(elim-letrec/ids #'csq-expr ids)
               #,(elim-letrec/ids #'alt-expr ids)))]
      [(let-values ([(varss ...) rhs-exprs] ...) body-exprs ...)
       (with-syntax ([(rhs-exprs ...) (recertify* (syntax->list #'(rhs-exprs ...)) expr)]
                     [(body-exprs ...) (recertify* (syntax->list #'(body-exprs ...)) expr)])
         (let ([elim-letrec* (lambda (an-expr) (elim-letrec/ids an-expr ids))])
           (with-syntax ([(new-rhs-exprs ...)
                          (map elim-letrec* (syntax->list #'(rhs-exprs ...)))]
                         [(new-body-exprs ...)
                          (map elim-letrec* (syntax->list #'(body-exprs ...)))])
             #`(let-values ([(varss ...) new-rhs-exprs] ...) new-body-exprs ...))))]
      [(#%app expr-rator expr-rands ...)
       (with-syntax ([(expr-rator expr-rands ...) (recertify* (syntax->list #'(expr-rator expr-rands ...)) expr)])
         #`(#%app #,(elim-letrec/ids #'expr-rator ids)
                  #,@(map
                      (lambda (expr-rand)
                        (elim-letrec/ids expr-rand ids))
                      (syntax->list #'(expr-rands ...)))))]
      [(set! id rhs-expr)
       (with-syntax ([id (recertify #'id expr)])
         (if (bound-identifier-member? #'id ids)
             #`(#%app set-box! id #,(elim-letrec/ids #'rhs-expr ids))
             #`(set! id #,(elim-letrec/ids #'rhs-expr ids))))]
      [(#%datum . datum) expr]
      [(#%top . var) expr]
      [(begin rest-expr ...)
       (with-syntax ([(rest-expr ...) (recertify* (syntax->list #'(rest-expr ...)) expr)])
         #`(begin
             #,@(map
                 (lambda (an-expr)
                   (elim-letrec/ids an-expr ids))
                 (syntax->list #'(rest-expr ...)))))]
      [(quote datum) expr]
      [id
       (if (bound-identifier-member? #'id ids)
           #'(#%app unbox id)
           #'id)]
      [_else
       (raise-syntax-error #f "eliminate-letrec: unsupported form" expr)]))
  
  (define myprint printf)
  
  ;; bound-identifier-member?: identifier (listof identifier) -> boolean
  ;; is the given identifier in the list according to bound-identifier=?
  (define (bound-identifier-member? id ids)
    (ormap
     (lambda (an-id)
       (bound-identifier=? id an-id))
     ids))
  )

