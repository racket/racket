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
 (prefix-in t: (types convenience abbrev))
 (private parse-type)
 racket/match unstable/match syntax/struct syntax/stx mzlib/trace racket/syntax scheme/list
 (only-in scheme/contract -> ->* case-> cons/c flat-rec-contract provide/contract any/c)
 (for-template scheme/base racket/contract racket/set (utils any-wrap)
               (prefix-in t: (types numeric-predicates))
               (only-in unstable/contract sequence/c)
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
         (quasisyntax/loc stx (define-values (n) (recursive-contract cnt #,(if flat? #'#:flat #'#:impersonator))))))]
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
         (if (and (not from-typed?) (type-equal? elem-ty t:Univ))
             #'list?
             #`(listof #,(t->c elem-ty)))]
        [(? (lambda (e) (eq? t:Any-Syntax e))) #'syntax?]

        ;; numeric special cases
        ;; since often-used types like Integer are big unions, this would
        ;; generate large contracts.
        [(== t:-PosByte type-equal?) #'(flat-named-contract 'Positive-Byte (and/c byte? positive?))]
        [(== t:-Byte type-equal?) #'(flat-named-contract 'Byte byte?)]
        [(== t:-PosIndex type-equal?) #'(flat-named-contract 'Positive-Index (and/c t:index? positive?))]
        [(== t:-Index type-equal?) #'(flat-named-contract 'Index t:index?)]
        [(== t:-PosFixnum type-equal?) #'(flat-named-contract 'Positive-Fixnum (and/c fixnum? positive?))]
        [(== t:-NonNegFixnum type-equal?) #'(flat-named-contract 'Nonnegative-Fixnum (and/c fixnum? (lambda (x) (>= x 0))))]
        ;; -NegFixnum is a base type
        [(== t:-NonPosFixnum type-equal?) #'(flat-named-contract 'Nonpositive-Fixnum (and/c fixnum? (lambda (x) (<= x 0))))]
        [(== t:-Fixnum type-equal?) #'(flat-named-contract 'Fixnum fixnum?)]
        [(== t:-PosInt type-equal?) #'(flat-named-contract 'Positive-Integer (and/c exact-integer? positive?))]
        [(== t:-Nat type-equal?) #'(flat-named-contract 'Natural (and/c exact-integer? (lambda (x) (>= x 0))))]
        [(== t:-NegInt type-equal?) #'(flat-named-contract 'Negative-Integer (and/c exact-integer? negative?))]
        [(== t:-NonPosInt type-equal?) #'(flat-named-contract 'Nonpositive-Integer (and/c exact-integer? (lambda (x) (<= x 0))))]
        [(== t:-Int type-equal?) #'(flat-named-contract 'Integer exact-integer?)]
        [(== t:-PosRat type-equal?) #'(flat-named-contract 'Positive-Rational (and/c t:exact-rational? positive?))]
        [(== t:-NonNegRat type-equal?) #'(flat-named-contract 'Nonnegative-Rational (and/c t:exact-rational? (lambda (x) (>= x 0))))]
        [(== t:-NegRat type-equal?) #'(flat-named-contract 'Negative-Rational (and/c t:exact-rational? negative?))]
        [(== t:-NonPosRat type-equal?) #'(flat-named-contract 'Nonpositive-Rational (and/c t:exact-rational? (lambda (x) (<= x 0))))]
        [(== t:-Rat type-equal?) #'(flat-named-contract 'Rational t:exact-rational?)]
        [(== t:-FlonumZero type-equal?) #'(flat-named-contract 'Float-Zero (and/c flonum? zero?))]
        [(== t:-NonNegFlonum type-equal?) #'(flat-named-contract 'Nonnegative-Float (and/c flonum? (lambda (x) (>= x 0))))]
        [(== t:-NonPosFlonum type-equal?) #'(flat-named-contract 'Nonpositive-Float (and/c flonum? (lambda (x) (<= x 0))))]
        [(== t:-Flonum type-equal?) #'(flat-named-contract 'Float flonum?)]
        [(== t:-SingleFlonumZero type-equal?) #'(flat-named-contract 'Single-Flonum-Zero (and/c t:single-flonum? zero?))]
        [(== t:-InexactRealZero type-equal?) #'(flat-named-contract 'Inexact-Real-Zero (and/c inexact-real? zero?))]
        [(== t:-PosInexactReal type-equal?) #'(flat-named-contract 'Positive-Inexact-Real (and/c inexact-real? positive?))]
        [(== t:-NonNegSingleFlonum type-equal?) #'(flat-named-contract 'Nonnegative-Single-Flonum (and/c t:single-flonum? (lambda (x) (>= x 0))))]
        [(== t:-NonNegInexactReal type-equal?) #'(flat-named-contract 'Nonnegative-Inexact-Real (and/c inexact-real? (lambda (x) (>= x 0))))]
        [(== t:-NegInexactReal type-equal?) #'(flat-named-contract 'Negative-Inexact-Real (and/c inexact-real? negative?))]
        [(== t:-NonPosSingleFlonum type-equal?) #'(flat-named-contract 'Nonpositive-Single-Flonum (and/c t:single-flonum? (lambda (x) (<= x 0))))]
        [(== t:-NonPosInexactReal type-equal?) #'(flat-named-contract 'Nonpositive-Inexact-Real (and/c inexact-real? (lambda (x) (<= x 0))))]
        [(== t:-SingleFlonum type-equal?) #'(flat-named-contract 'Single-Flonum t:single-flonum?)]
        [(== t:-InexactReal type-equal?) #'(flat-named-contract 'Inexact-Real inexact-real?)]
        [(== t:-RealZero type-equal?) #'(flat-named-contract 'Real-Zero (and/c real? zero?))]
        [(== t:-PosReal type-equal?) #'(flat-named-contract 'Positive-Real (and/c real? positive?))]
        [(== t:-NonNegReal type-equal?) #'(flat-named-contract 'Nonnegative-Real (and/c real? (lambda (x) (>= x 0))))]
        [(== t:-NegReal type-equal?) #'(flat-named-contract 'Negative-Real (and/c real? negative?))]
        [(== t:-NonPosReal type-equal?) #'(flat-named-contract 'Nonpositive-Real (and/c real? (lambda (x) (<= x 0))))]
        [(== t:-Real type-equal?) #'(flat-named-contract 'Real real?)]
        [(== t:-ExactNumber type-equal?) #'(flat-named-contract 'Exact-Number (and/c number? exact?))]
        [(== t:-InexactComplex type-equal?)
         #'(flat-named-contract 'Inexact-Complex
                                (and/c number?
                                       (lambda (x)
                                         (and (inexact-real? (imag-part x))
                                              (inexact-real? (real-part x))))))]
        [(== t:-Number type-equal?) #'(flat-named-contract 'Number number?)]

        [(Base: sym cnt _ _ _) #`(flat-named-contract '#,sym (flat-contract-predicate #,cnt))]
        [(Refinement: par p? cert)
         #`(and/c #,(t->c par) (flat-contract #,(cert p?)))]
        [(Union: elems)
         (let-values ([(vars notvars) (partition F? elems)])
           (unless (>= 1 (length vars)) (exit (fail)))
           (with-syntax
               ([cnts (append (map t->c vars) (map t->c notvars))])
             #'(or/c . cnts)))]
        [(and t (Function: _)) (t->c/fun t)]
	[(Set: t) #`(set/c #,(t->c t))]
        [(Sequence: ts) #`(sequence/c #,@(map t->c ts))]
        [(Vector: t)
         (if flat?
             #`(vectorof #,(t->c t #:flat #t) #:flat? #t)
             #`(vectorof #,(t->c t)))]
        [(Box: t)
         (if flat?
             #`(box/c #,(t->c t #:flat #t) #:flat? #t)
             #`(box/c #,(t->c t)))]
        [(Pair: t1 t2)
         #`(cons/c #,(t->c t1) #,(t->c t2))]
        [(Opaque: p? cert)
         #`(flat-named-contract (quote #,(syntax-e p?)) #,(cert p?))]
        [(F: v) (cond [(assoc v (vars)) => second]
                      [else (int-err "unknown var: ~a" v)])]
	[(Poly: vs b)
         (if from-typed?
             ;; in positive position, no checking needed for the variables
             (parameterize ([vars (append (for/list ([v vs]) (list v #'any/c)) (vars))])
               (t->c b))
             ;; in negative position, use `parameteric/c'
             (match-let ([(Poly-names: vs-nm _) ty])
               (with-syntax ([(v ...) (generate-temporaries vs-nm)])
                 (parameterize ([vars (append (map list vs (syntax->list #'(v ...)))
                                              (vars))])
                   #`(parametric->/c (v ...) #,(t->c b))))))]
        [(Mu: n b)
         (match-let ([(Mu-name: n-nm _) ty])
           (with-syntax ([(n*) (generate-temporaries (list n-nm))])
             (parameterize ([vars (cons (list n #'n* #'n*) (vars))])
               #`(flat-rec-contract n* #,(t->c b #:flat #t)))))]
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
                           [cnt-name nm])
              ;If it should be a flat contract, we make flat contracts for the type of each field,
              ;extract the predicates, and apply the predicates to the corresponding field value
              (if flat?
                #`(letrec ([rec
                            (make-flat-contract
                             #:name 'cnt-name
                             #:first-order
                              (lambda (val)
                               (and
                                (#,pred? val)
                                #,@(for/list ([fty flds] [f-acc acc-ids])
                                    #`((flat-contract-predicate
                                       #,(t->c fty #:seen (cons (cons ty #'(recursive-contract rec)) structs-seen)))
                                       (#,f-acc val))))))])
                    rec)
                ;Should make this case a chaperone/impersonator contract
                (with-syntax ([(fld-cnts ...)
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
                                   (unless (#,pred? val)
                                     (raise-blame-error blame val "expected ~a value, got ~v" 'cnt-name val))
                                   (maker fld-cnts ...))))])
                      rec))))]
           [else #`(flat-named-contract '#,(syntax-e pred?) #,(cert pred?))])]
        [(Syntax: (Base: 'Symbol _ _ _ _)) #'identifier?]
        [(Syntax: t) #`(syntax/c #,(t->c t))]
        [(Value: v) #`(flat-named-contract #,(format "~a" v) (lambda (x) (equal? x '#,v)))]
        [(Param: in out) #`(parameter/c #,(t->c out))]
	[(Hashtable: k v)
         (if flat?
             #`(hash/c #,(t->c k #:flat #t) #,(t->c v #:flat #t) #:flat? #t #:immutable 'dont-care)
             #`(hash/c #,(t->c k) #,(t->c v) #:immutable 'dont-care))]
        [else
         (exit (fail))]))))


