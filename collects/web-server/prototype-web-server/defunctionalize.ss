(module defunctionalize mzscheme
  (require (lib "list.ss")
           "closure.ss"
           "syntax-utils.ss")
  (require-for-template mzscheme)
  (provide defunctionalize-definition
           defunctionalize)  

  ;; **************************************************
  ;; LANGUAGE
  ;;
  ;; program ::= definition* expr
  ;;
  ;; definition ::= (define-values (var) expr)
  ;;
  ;; expr ::= w
  ;;       |  E[redex]
  ;;
  ;; redex ::= (if w expr)
  ;;        |  (if w expr expr)
  ;;        |  (#%app w w...)
  ;;
  ;; E ::= [] 
  ;;    |  (let-values ([(f) (lambda (var) expr)])
  ;;         (#%app f (w-c-m f E)))
  ;;
  ;;   w  ::= var | (#%top . var) | value
  ;;
  ;;   value ::=  (#%datum . datum)
  ;;          | (lambda (var ...) expr)

  ;; defunctionalize-definition: definition (-> symbol) -> (listof definition)
  ;; remove lambdas from a definition
  (define (defunctionalize-definition def labeling)
    (syntax-case def ()
      [(define-values (var ...) expr)
       (with-syntax ([expr (recertify #'expr def)])
         (let-values ([(new-expr defs) (defunctionalize #'expr labeling)])
           (append defs (list #`(define-values (var ...) #,new-expr)))))]
      [else
       (raise-syntax-error #f "defunctionalize-definition dropped through" def)]))
    
  ;; defunctionalize: expr (-> symbol) -> (values expr (listof definition))
  ;; remove lambdas from an expression
  (define (defunctionalize expr labeling)
    (syntax-case expr (if #%app lambda let-values #%top #%datum with-continuation-mark quote)
      [(if test-expr csq-expr)
       (with-syntax ([(tst-expr csq-expr) (recertify* (list #'tst-expr #'csq-expr) expr)])
         (let-values ([(new-test-expr test-defs) (defunctionalize #'test-expr labeling)]
                      [(new-csq-expr csq-defs) (defunctionalize #'csq-expr labeling)])
           (values 
            #`(if #,new-test-expr #,new-csq-expr)
            (append test-defs csq-defs))))]
      [(if test-expr csq-expr alt-expr)
       (with-syntax ([(tst-expr csq-expr alt-expr) (recertify* (list #'tst-expr #'csq-expr #'alt-expr) expr)])
         (let-values ([(new-test-expr test-defs) (defunctionalize #'test-expr labeling)]
                      [(new-csq-expr csq-defs) (defunctionalize #'csq-expr labeling)]
                      [(new-alt-expr alt-defs) (defunctionalize #'alt-expr labeling)])
           (values #`(if #,new-test-expr #,new-csq-expr #,new-alt-expr)
                   (append test-defs csq-defs alt-defs))))]
      [(#%app exprs ...)
       (with-syntax ([(exprs ...) (recertify* (syntax->list #'(exprs ...)) expr)])
         (let-values ([(new-exprs defs) (defunctionalize* (syntax->list #'(exprs ...)) labeling)])
           (values
            #`(#%app #,@new-exprs)
            defs)))]
      [(let-values ([(f) rhs])
         (#%app f-apply (with-continuation-mark ignore-key f-mark body-expr)))
       (with-syntax ([(rhs f-apply ignore-key f-mark body-expr)
                      (recertify* (syntax->list #'(rhs f-apply ignore-key f-mark body-expr)) expr)])
         (let-values ([(new-rhs rhs-defs) (defunctionalize #'rhs labeling)]
                      [(new-body-expr body-defs) (defunctionalize #'body-expr labeling)])
           (values
            #`(let ([f #,new-rhs])
                (f-apply (with-continuation-mark ignore-key f-mark #,new-body-expr)))
            (append rhs-defs body-defs))))]
      [(let-values ([(f) rhs]) (#%app f-apply body-expr))
       (with-syntax ([(rhs f-apply body-expr) (recertify* (syntax->list #'(rhs f-apply body-expr) expr))])
         (let-values ([(new-rhs rhs-defs) (defunctionalize #'rhs labeling)]
                      [(new-body-expr body-defs) (defunctionalize #'body-expr labeling)])
           (values
            #`(let ([f #,new-rhs])
                (f-apply #,new-body-expr))
            (append rhs-defs body-defs))))]
      [(lambda (formals ...) body-expr)
       (with-syntax ([body-expr (recertify #'body-expr expr)])
         (let-values ([(new-body-expr body-defs) (defunctionalize #'body-expr labeling)])
           (let ([fvars (free-vars expr)]
                 [tag (labeling)])
             (let-values ([(make-CLOSURE closure-definitions)
                           (make-closure-definition-syntax tag fvars
                                                           #`(lambda (formals ...) #,new-body-expr))])
               (values
                (if (null? fvars)
                    #`(#,make-CLOSURE)
                    #`(#,make-CLOSURE (lambda () (values #,@fvars))))
                (append body-defs
                        closure-definitions))))))]
      [(with-continuation-mark safe-call? b-val body-expr)
       (with-syntax ([body-expr (recertify #'body-expr expr)])
         (let-values ([(new-body-expr body-defs) (defunctionalize #'body-expr labeling)])
           (values
            #`(with-continuation-mark safe-call? b-val #,new-body-expr)
            body-defs)))]
      [(#%top . var) (values expr '())]
      [(#%datum . var) (values expr '())]
      [(quote datum) (values expr '())]
      [var (identifier? #'var) (values expr '())]
      [_else
       (raise-syntax-error #f "defunctionalize: dropped through" expr)]))
  
  ;; defunctionalize*: (listof expr) (-> symbol) -> (values (listof expr) (listof definition))
  ;; remove lambdas from a whole list of expressions
  (define (defunctionalize* exprs labeling)
    (cond
      [(null? exprs) (values '() '())]
      [else
       (let-values ([(first-new-expr first-defs) (defunctionalize (car exprs) labeling)]
                    [(rest-new-exprs rest-defs) (defunctionalize* (cdr exprs) labeling)])
         (values
          (cons first-new-expr rest-new-exprs)
          (append first-defs rest-defs)))]))
  
  ;; free-vars: expr -> (listof identifier)
  ;; Find the free variables in an expression
  (define (free-vars expr)
    (syntax-case expr (if #%app lambda let #%top #%datum with-continuation-mark quote)
      [(if test-expr csq-expr)
       (union (free-vars #'test-expr)
              (free-vars #'csq-expr))]
      [(if test-expr csq-expr alt-expr)
       (union (free-vars #'test-expr)
              (union (free-vars #'csq-expr)
                     (free-vars #'alt-expr)))]
      [(#%app exprs ...)
       (free-vars* (syntax->list #'(exprs ...)))]
      [(let-values ([(f) rhs])
         (#%app f-apply (with-continuation-mark ignore-key f-mark body-expr)))
       ;; (and (bound-identifier=? #'f #'f-apply) (bound-identifier=? #'f #'f-mark))
       (union (free-vars #'rhs)
              (set-diff (free-vars #'body-expr) (list #'f)))]
      
      [(let-values ([(f) rhs]) (#%app f-apply body-expr))
       (union (free-vars #'rhs)
              (set-diff (free-vars #'body-expr) (list #'f)))]
      
      [(lambda (formals ...) body-expr)
       (set-diff (free-vars #'body-expr) (syntax->list #'(formals ...)))]
      [(with-continuation-mark safe-call? b-val body-expr)
       (free-vars #'body-expr)]
      [(#%top . var) '()]
      [(#%datum . var) '()]
      [(quote datum) '()]
      [var (identifier? #'var)
           (let ([i-bdg (identifier-binding #'var)])
             (cond
               [(eqv? 'lexical (identifier-binding #'var))
                (list #'var)]
               [else '()]))]
      [_else
       (raise-syntax-error #f "free-vars: dropped through" expr)]))
  
  ;; free-vars*: (listof expr) -> (listof identifier)
  ;; union the free variables that occur in several expressions
  (define (free-vars* exprs)
    (foldl
     (lambda (expr acc) (union (free-vars expr) acc))
     '() exprs))
  
  ;; union: (listof identifier) (listof identifier) -> (listof identifier)
  ;; produce the set-theoretic union of two lists
  (define (union l1 l2)
    (cond
      [(null? l1) l2]
      [else (insert (car l1) (union (cdr l1) l2))]))
  
  ;; insert: symbol (listof identifier) -> (listof symbol)
  ;; insert a symbol into a list without creating a duplicate
  (define (insert sym into)
    (cond
      [(null? into) (list sym)]
      [(bound-identifier=? sym (car into)) into]
      [else (cons (car into) (insert sym (cdr into)))]))
  
  ;; set-diff: (listof identifier) (listof identifier) -> (listof identifier)
  ;; produce the set-theoretic difference of two lists
  (define (set-diff s1 s2)
    (cond
      [(null? s2) s1]
      [else (set-diff (sans s1 (car s2)) (cdr s2))]))
  
  ;; sans: (listof identifier) symbol -> (listof identifier)
  ;; produce the list sans the symbol
  (define (sans s elt)
    (cond
      [(null? s) '()]
      [(bound-identifier=? (car s) elt)
       (cdr s)] ;; if we maintain the no-dupe invariant then we don't need to recur
      [else (cons (car s)
                  (sans (cdr s) elt))]))
  )

