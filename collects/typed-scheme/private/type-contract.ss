(module type-contract mzscheme
  
  (provide type->contract define/fixup-contract? generate-contract-def change-contract-fixups)
  
  (require
   "type-rep.ss"   
   "parse-type.ss"
   "utils.ss"      
   "type-name-env.ss"
   "require-contract.ss"
   "internal-forms.ss"
   "tc-utils.ss"
   "resolve-type.ss"
   "type-utils.ss"
   mzlib/list   
   (only "type-effect-convenience.ss" Any-Syntax))
  
  (require
   (lib "plt-match.ss")
   (lib "struct.ss" "syntax")
   (lib "stx.ss" "syntax")
   (lib "trace.ss")
   (only (lib "contract.ss") -> ->* case-> cons/c flat-rec-contract)
   (lib "etc.ss")           
   (lib "struct.ss")
   #;(lib "syntax-browser.ss" "macro-debugger"))
  
  (require-for-template mzscheme (lib "contract.ss") (only scheme/class object%))
  
  (define (define/fixup-contract? stx)
    (syntax-property stx 'typechecker:contract-def))
  
  (define (generate-contract-def stx)
    (define prop (syntax-property stx 'typechecker:contract-def))
    (define typ (parse-type prop))
    (syntax-case stx (define-values)
      [(_ (n) __)
       (with-syntax ([cnt (type->contract typ (lambda () (tc-error/stx prop "Type ~a could not be converted to a contract." typ)))])
         (syntax/loc stx (define-values (n) cnt)))]
      [_ (int-err "should never happen - not a define-values: ~a" (syntax-object->datum stx))]))
  
  (define (change-contract-fixups forms)
    (map (lambda (e)
           (if (not (define/fixup-contract? e))
               e
               (generate-contract-def e)))
         (syntax->list forms)))
  
  
  (define (type->contract ty fail)
    (define vars (make-parameter '()))
    (let/cc exit
      (let t->c ([ty ty])
        (match ty
          [(or (App: _ _ _) (Name: _)) (t->c (resolve-once ty))]
          [(Univ:) #'any/c]
          ;; we special-case lists:
          [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
           #`(listof #,(t->c elem-ty))]
          [(? (lambda (e) (eq? Any-Syntax e))) #'syntax?]
          [(Base: sym)
           (case sym
             [(Number) #'number?]
             [(Boolean) #'boolean?]
             [(Keyword) #'keyword?]
             [(Port) #'port?]
             [(Path) #'path?]
             [(String) #'string?]
             [(Symbol) #'symbol?]
             [(Bytes) #'bytes?]
             [(Void) #'void?]
             [(Syntax) #'syntax?]
             [(Output-Port) #'output-port?]
             [(Input-Port) #'input-port?]
             [(Char) #'char?]
             [(Namespace) #'namespace?]
             [else (int-err "Base type ~a cannot be converted to contract" sym)])]
          [(Union: elems) 
           (with-syntax 
               ([cnts (map t->c elems)])
             #;(printf "~a~n" (syntax-object->datum #'cnts))
             #'(or/c . cnts))]
          [(Function: arrs)
           (let ()
             (define (f a)
               (define-values (dom* rngs* rst)
                 (match a
                   [(arr: dom (Values: rngs) #f _ _)
                    (values (map t->c dom) (map t->c rngs) #f)]
                   [(arr: dom rng #f _ _)
                    (values (map t->c dom) (list (t->c rng)) #f)]
                   [(arr: dom (Values: rngs) rst _ _)
                    (values (map t->c dom) (map t->c rngs) (t->c rst))]
                   [(arr: dom rng rst _ _)
                    (values (map t->c dom) (list (t->c rng)) (t->c rst))]))
               (with-syntax 
                   ([(dom* ...) dom*]
                    [(rng* ...) rngs*]
                    [rst* rst])
                 (if rst
                     #'((dom* ...) (listof rst*) . ->* . (rng* ...))
                     #'((dom* ...) . ->* . (rng* ...)))))
             (let ([l (map f arrs)])
               (if (and (pair? l) (null? (cdr l)))
                   (car l)
                   #`(case-> #,@l))))]
          [(Vector: t)
           #`(vectorof #,(t->c t))]
          [(Pair: t1 t2)
           #`(cons/c #,(t->c t1) #,(t->c t2))]
          [(Opaque: p? cert)
           #`(flat-contract #,(cert p?))]
          [(F: v) (cond [(assoc v (vars)) => cadr]
                        [else (int-err "unknown var: ~a" v)])]
          [(Mu: n b)
           (with-syntax ([(n*) (generate-temporaries (list n))])
             (parameterize ([vars (cons (list n #'n*) (vars))])
               #`(flat-rec-contract n* #,(t->c b))))]
          [(Value: #f) #'false/c]    
          [(Instance: _) #'(is-a?/c object%)]
          [(Class: _ _ _) #'(subclass?/c object%)]
          [(Value: '()) #'null?]
          [(Syntax: (Base: 'Symbol)) #'identifier?]
          [(Syntax: t)
           (if (equal? ty Any-Syntax)
               #`syntax?
               #`(syntax/c #,(t->c t)))]
          [(Value: v) #`(flat-named-contract #,(format "~a" v) (lambda (x) (equal? x #,v)))]
          [else (exit (fail))]))))
  
  )
