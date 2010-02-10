#lang scheme/base

(provide type->contract define/fixup-contract? generate-contract-def change-contract-fixups)

(require
 "../utils/utils.ss"
 (rep type-rep filter-rep object-rep)
 (typecheck internal-forms)
 (utils tc-utils require-contract)
 (env type-name-env)
 (types resolve utils)
 (prefix-in t: (types convenience))
 (private parse-type)
 scheme/match syntax/struct syntax/stx mzlib/trace unstable/syntax scheme/list
 (only-in scheme/contract -> ->* case-> cons/c flat-rec-contract provide/contract any/c)
 (for-template scheme/base scheme/contract unstable/poly-c (utils any-wrap) (only-in scheme/class object% is-a?/c subclass?/c)))

(define (define/fixup-contract? stx)
  (or (syntax-property stx 'typechecker:contract-def)
      (syntax-property stx 'typechecker:contract-def/maker)))

(define (generate-contract-def stx)
  (define prop (or (syntax-property stx 'typechecker:contract-def)
                   (syntax-property stx 'typechecker:contract-def/maker)))
  (define maker? (syntax-property stx 'typechecker:contract-def/maker))
  (define typ (parse-type prop))
  (syntax-case stx (define-values)
    [(_ (n) __)
     (let ([typ (if maker?
                    ((Struct-flds (lookup-type-name (Name-id typ))) #f . t:->* . typ)
                    typ)])
       (with-syntax ([cnt (type->contract 
                           typ 
                           ;; this is for a `require/typed', so the value is not from the typed side
                           #:typed-side #f 
                           (lambda () (tc-error/stx prop "Type ~a could not be converted to a contract." typ)))])
         (syntax/loc stx (define-values (n) cnt))))]
    [_ (int-err "should never happen - not a define-values: ~a" (syntax->datum stx))]))

(define (change-contract-fixups forms)
  (map (lambda (e)
         (if (not (define/fixup-contract? e))
             e
             (generate-contract-def e)))
       (syntax->list forms)))

(define (no-duplicates l)
  (= (length l) (length (remove-duplicates l))))


(define (type->contract ty fail #:out [out? #f] #:typed-side [from-typed? #t])
  (define vars (make-parameter '()))
  (let/ec exit
    (let loop ([ty ty] [pos? #t] [from-typed? from-typed?])
      (define (t->c t) (loop t pos? from-typed?))
      (define (t->c/neg t) (loop t (not pos?) (not from-typed?)))
      (match ty
        [(or (App: _ _ _) (Name: _)) (t->c (resolve-once ty))]
        ;; any/c doesn't provide protection in positive position
        [(Univ:) (if from-typed? 
                     #'any-wrap/c
                     #'any/c)]
        ;; we special-case lists:
        [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
         #`(listof #,(t->c elem-ty))]
        [(? (lambda (e) (eq? t:Any-Syntax e))) #'syntax?]
        [(Base: sym cnt) #`(flat-named-contract '#,sym (flat-contract-predicate #,cnt))]
        [(Refinement: par p? cert)
         #`(and/c #,(t->c par) (flat-contract #,(cert p?)))]
        [(Union: elems)         
         (let-values ([(vars notvars)
                       (partition F? elems)])
           (unless (>= 1 (length vars)) (exit (fail)))
           (with-syntax 
               ([cnts (append (map t->c vars) (map t->c notvars))])
             #'(or/c . cnts)))]
        [(Function: arrs)
         (let ()           
           (define (f a)
             (define-values (dom* opt-dom* rngs* rst)
               (match a
                 [(arr: dom (Values: (list (Result: rngs (LFilterSet: '() '()) (LEmpty:)) ...)) rst #f kws)
                  (let-values ([(mand-kws opt-kws) (partition (match-lambda [(Keyword: _ _ mand?) mand?]) kws)]
                               [(conv) (match-lambda [(Keyword: kw kty _) (list kw (t->c/neg kty))])])
                    (values (append (map t->c/neg dom) (append-map conv mand-kws))
                            (append-map conv opt-kws)
                            (map t->c rngs) 
                            (and rst (t->c/neg rst))))]
                 [(arr: dom (Values: (list (Result: rngs _ _) ...)) rst #f '())
                  (if (and out? pos?)
                      (values (map t->c/neg dom)
                              null
                              (map t->c rngs)
                              (and rst (t->c/neg rst)))
                      (exit (fail)))]
                 [_ (exit (fail))]))
             (trace f)
             (with-syntax 
                 ([(dom* ...) dom*]
                  [(opt-dom* ...) opt-dom*]
                  [rng* (match rngs*
                          [(list r) r]
                          [_ #`(values #,@rngs*)])]
                  [rst* rst])
               (if (or rst (pair? (syntax-e #'(opt-dom* ...))))
                   #'((dom* ...) (opt-dom* ...) #:rest (listof rst*) . ->* . rng*)
                   #'(dom* ... . -> . rng*))))
           (unless (no-duplicates (for/list ([t arrs])
                                    (match t [(arr: dom _ _ _ _) (length dom)])))
             (exit (fail)))
	   (match (map f arrs)
	     [(list e) e]
	     [l #`(case-> #,@l)]))]
        [(Vector: t)
         #`(vectorof #,(t->c t))]
        [(Box: t)
         #`(box/c #,(t->c t))]
        [(Pair: t1 t2)
         #`(cons/c #,(t->c t1) #,(t->c t2))]
        [(Opaque: p? cert)
         #`(flat-named-contract (quote #,(syntax-e p?)) #,(cert p?))]
        [(F: v) (cond [(assoc v (vars)) => (if pos? second third)]
                      [else (int-err "unknown var: ~a" v)])]
	[(Poly: vs (and b (Function: _)))
         (match-let ([(Poly-names: vs-nm _) ty])
           (with-syntax ([(vs+ ...) (generate-temporaries (for/list ([v vs-nm]) (format-symbol "~a+" v)))]
			 [(vs- ...) (generate-temporaries (for/list ([v vs-nm]) (format-symbol "~a-" v)))])
             (parameterize ([vars (append (map list
					       vs
					       (syntax->list #'(vs+ ...))
					       (syntax->list #'(vs- ...)))
					  (vars))])
               #`(poly/c ([vs- vs+] ...) #,(t->c b)))))]
        [(Mu: n b)
         (match-let ([(Mu-name: n-nm _) ty])
           (with-syntax ([(n*) (generate-temporaries (list n-nm))])
             (parameterize ([vars (cons (list n #'n* #'n*) (vars))])
               #`(flat-rec-contract n* #,(t->c b)))))]
        [(Value: #f) #'false/c]    
        [(Instance: _) #'(is-a?/c object%)]
        [(Class: _ _ _) #'(subclass?/c object%)]
        [(Value: '()) #'null?]
        [(Struct: _ _ _ _ #f pred? cert) 
         #`(flat-named-contract '#,(syntax-e pred?) #,(cert pred?))]
        [(Syntax: (Base: 'Symbol _)) #'identifier?]
        [(Syntax: t) #`(syntax/c #,(t->c t))]
        [(Value: v) #`(flat-named-contract #,(format "~a" v) (lambda (x) (equal? x '#,v)))]
        [(Param: in out) #`(parameter/c #,(t->c out))]
	[(Hashtable: k v) #`(hash/c #,(t->c k) #,(t->c v) #:immutable 'dont-care)]
        [else          
         (exit (fail))]))))


