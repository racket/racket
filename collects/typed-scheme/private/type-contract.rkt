#lang scheme/base

(provide type->contract define/fixup-contract? change-contract-fixups)

(require
 "../utils/utils.rkt"
 syntax/parse
 (rep type-rep filter-rep object-rep)
 (typecheck internal-forms)
 (utils tc-utils require-contract)
 (env type-name-env)
 (types resolve utils)
 (prefix-in t: (types convenience))
 (private parse-type)
 scheme/match syntax/struct syntax/stx mzlib/trace unstable/syntax scheme/list 
 (only-in scheme/contract -> ->* case-> cons/c flat-rec-contract provide/contract any/c)
 (for-template scheme/base scheme/contract unstable/poly-c (utils any-wrap)
	       (only-in scheme/class object% is-a?/c subclass?/c object-contract class/c init object/c class?)))

(define (define/fixup-contract? stx)
  (or (syntax-property stx 'typechecker:contract-def)
      (syntax-property stx 'typechecker:flat-contract-def)
      (syntax-property stx 'typechecker:contract-def/maker)))

(define (generate-contract-def stx)
  (define prop (define/fixup-contract? stx))
  (define maker? (syntax-property stx 'typechecker:contract-def/maker))
  (define flat? (syntax-property stx 'typechecker:flat-contract-def))
  (define typ (parse-type prop))
  (syntax-parse stx #:literals (define-values)
    [(define-values (n) _)
     (let ([typ (if maker?
                    ((map fld-t (Struct-flds (lookup-type-name (Name-id typ)))) #f . t:->* . typ)
                    typ)])
       (with-syntax ([cnt (type->contract 
                           typ 
                           ;; this is for a `require/typed', so the value is not from the typed side
                           #:typed-side #f 
                           #:flat flat?
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


(define (type->contract ty fail #:out [out? #f] #:typed-side [from-typed? #t] #:flat [flat? #f])
  (define vars (make-parameter '()))
  (let/ec exit
    (let loop ([ty ty] [pos? #t] [from-typed? from-typed?] [structs-seen null] [flat? flat?])
      (define (t->c t #:seen [structs-seen structs-seen] #:flat [flat? flat?])
        (loop t pos? from-typed? structs-seen flat?))
      (define (t->c/neg t #:seen [structs-seen structs-seen] #:flat [flat? flat?])
        (loop t (not pos?) (not from-typed?) structs-seen flat?))
      (define (t->c/fun f #:method [method? #f])
        (match f
          [(Function: (list (top-arr:))) #'procedure?]
          [(Function: arrs)
           (when flat? (exit (fail)))
           (let ()           
             (define ((f [case-> #f]) a)
               (define-values (dom* opt-dom* rngs* rst)
                 (match a
                   ;; functions with no filters or objects
                   [(arr: dom (Values: (list (Result: rngs (FilterSet: (Top:) (Top:)) (Empty:)) ...)) rst #f kws)
                    (let-values ([(mand-kws opt-kws) (partition (match-lambda [(Keyword: _ _ mand?) mand?]) kws)]
                                 [(conv) (match-lambda [(Keyword: kw kty _) (list kw (t->c/neg kty))])])
                      (values (append (map t->c/neg dom) (append-map conv mand-kws))
                              (append-map conv opt-kws)
                              (map t->c rngs) 
                              (and rst (t->c/neg rst))))]
                   ;; functions with filters or objects
                   [(arr: dom (Values: (list (Result: rngs _ _) ...)) rst #f '())
                    (if (and out? pos?)
                        (values (map t->c/neg dom)
                                null
                                (map t->c rngs)
                                (and rst (t->c/neg rst)))
                        (exit (fail)))]
                   [_ (exit (fail))]))
               (with-syntax 
                   ([(dom* ...) (if method? (cons #'any/c dom*) dom*)]
                    [(opt-dom* ...) opt-dom*]
                    [rng* (match rngs*
                            [(list r) r]
                            [_ #`(values #,@rngs*)])]
                    [rst* rst])
		 ;; Garr, I hate case->!
		 (if (and (pair? (syntax-e #'(opt-dom* ...))) case->)
		     (exit (fail))
		     (if (or rst (pair? (syntax-e #'(opt-dom* ...))))
			 (if case->
			     #'(dom* ... #:rest (listof rst*) . -> . rng*)
			     #'((dom* ...) (opt-dom* ...) #:rest (listof rst*) . ->* . rng*))
			 #'(dom* ... . -> . rng*)))))
             (unless (no-duplicates (for/list ([t arrs])
                                      (match t
                                        [(arr: dom _ _ _ _) (length dom)]
                                        ;; is there something more sensible here?
                                        [(top-arr:) (int-err "got top-arr")])))
               (exit (fail)))
             (match (map (f (not (= 1 (length arrs)))) arrs)
               [(list e) e]
               [l #`(case-> #,@l)]))]
          [_ (int-err "not a function" f)]))
      (match ty
        [(or (App: _ _ _) (Name: _)) (t->c (resolve-once ty))]
        ;; any/c doesn't provide protection in positive position
        [(Univ:) (if from-typed? #'any-wrap/c #'any/c)]
        ;; we special-case lists:
        [(Mu: var (Union: (list (Value: '()) (Pair: elem-ty (F: var)))))
         #`(listof #,(t->c elem-ty))]
        [(? (lambda (e) (eq? t:Any-Syntax e))) #'syntax?]
        [(Base: sym cnt) #`(flat-named-contract '#,sym (flat-contract-predicate #,cnt))]
        [(Refinement: par p? cert)
         #`(and/c #,(t->c par) (flat-contract #,(cert p?)))]
        [(Union: elems)         
         (let-values ([(vars notvars) (partition F? elems)])
           (unless (>= 1 (length vars)) (exit (fail)))
           (with-syntax 
               ([cnts (append (map t->c vars) (map t->c notvars))])
             #'(or/c . cnts)))]
        [(and t (Function: _)) (t->c/fun t)]
        [(Vector: t)
         #`(vectorof #,(t->c t #:flat #t))]
        [(Box: t)
         #`(box/c #,(t->c t #:flat #t))]
        [(Pair: t1 t2)
         #`(cons/c #,(t->c t1) #,(t->c t2))]
        [(Opaque: p? cert)
         #`(flat-named-contract (quote #,(syntax-e p?)) #,(cert p?))]
        [(F: v) (cond [(assoc v (vars)) => second]
                      [else (int-err "unknown var: ~a" v)])]
	[(Poly: vs (and b (Function: _)))
         (when flat? (exit (fail)))
         (match-let ([(Poly-names: vs-nm _) ty])
           (with-syntax ([(v ...) (generate-temporaries vs-nm)])
             (parameterize ([vars (append (map list vs (syntax->list #'(v ...)))
					  (vars))])
               #`(parametric/c (v ...) #,(t->c b)))))]
        [(Mu: n b)
         (match-let ([(Mu-name: n-nm _) ty])
           (with-syntax ([(n*) (generate-temporaries (list n-nm))])
             (parameterize ([vars (cons (list n #'n* #'n*) (vars))])
               #`(flat-rec-contract n* #,(t->c b)))))]
        [(Value: #f) #'false/c]
        [(Instance: (Class: _ _ (list (list name fcn) ...)))
         (when flat? (exit (fail)))
         (with-syntax ([(fcn-cnts ...) (for/list ([f fcn]) (t->c/fun f #:method #t))]
                       [(names ...) name])
           #'(object/c (names fcn-cnts) ...))]
        ;; init args not currently handled by class/c
        [(Class: _ (list (list by-name-init by-name-init-ty _) ...) (list (list name fcn) ...))
         (when flat? (exit (fail)))
         (with-syntax ([(fcn-cnt ...) (for/list ([f fcn]) (t->c/fun f #:method #t))]
                       [(name ...) name]
                       [(by-name-cnt ...) (for/list ([t by-name-init-ty]) (t->c/neg t))]
                       [(by-name-init ...) by-name-init])
           #;#'class?           
           #'(class/c (name fcn-cnt) ... (init [by-name-init by-name-cnt] ...)))]
        [(Value: '()) #'null?]
        [(Struct: nm par (list (fld: flds acc-ids mut?) ...) proc poly? pred? cert maker-id)
         (cond 
           [(assf (λ (t) (type-equal? t ty)) structs-seen)
            =>
            cdr]
           [proc (exit (fail))]
           [poly? 
            (with-syntax* ([(rec blame val) (generate-temporaries '(rec blame val))]
                           [maker maker-id]
                           [cnt-name nm]
                           [(fld-cnts ...)
                            (for/list ([fty flds]
                                       [f-acc acc-ids]
                                       [m? mut?])
                              #`(((contract-projection
                                   #,(t->c fty #:seen (cons (cons ty #'(recursive-contract rec)) structs-seen)))
                                  blame)
                                 (#,f-acc val)))])
              #`(letrec ([rec 
                          (make-contract 
                           #:name 'cnt-name
                           #:first-order #,pred?
                           #:projection 
                           (lambda (blame)
                             (lambda (val)
                               (maker fld-cnts ...))))])
                  rec))]
           [else #`(flat-named-contract '#,(syntax-e pred?) #,(cert pred?))])]
        [(Syntax: (Base: 'Symbol _)) #'identifier?]
        [(Syntax: t) #`(syntax/c #,(t->c t))]
        [(Value: v) #`(flat-named-contract #,(format "~a" v) (lambda (x) (equal? x '#,v)))]
        [(Param: in out) #`(parameter/c #,(t->c out))]
	[(Hashtable: k v) #`(hash/c #,(t->c k #:flat #t) #,(t->c v #:flat #t) #:immutable 'dont-care)]
        [else          
         (exit (fail))]))))


