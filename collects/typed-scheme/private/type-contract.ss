#lang scheme/base

(provide type->contract define/fixup-contract? generate-contract-def change-contract-fixups)

(require (except-in "../utils/utils.ss" extend))
(require
 (rep type-rep)
 (typecheck internal-forms)
 (utils tc-utils)
 (env type-name-env)
 "parse-type.ss"
 "require-contract.ss"
 "resolve-type.ss"
 "type-utils.ss"   
 (only-in "type-effect-convenience.ss" Any-Syntax)
 (prefix-in t: "type-effect-convenience.ss")
 scheme/match
 syntax/struct
 syntax/stx
 mzlib/trace
 scheme/list
 (only-in scheme/contract -> ->* case-> cons/c flat-rec-contract provide/contract any/c)
 (for-template scheme/base scheme/contract (only-in scheme/class object% is-a?/c subclass?/c)))

(define (define/fixup-contract? stx)
  (syntax-property stx 'typechecker:contract-def))

(define (generate-contract-def stx)
  (define prop (syntax-property stx 'typechecker:contract-def))
  (define typ (parse-type prop))
  (syntax-case stx (define-values)
    [(_ (n) __)
     (with-syntax ([cnt (type->contract typ (lambda () (tc-error/stx prop "Type ~a could not be converted to a contract." typ)))])
       (syntax/loc stx (define-values (n) cnt)))]
    [_ (int-err "should never happen - not a define-values: ~a" (syntax->datum stx))]))

(define (change-contract-fixups forms)
  (map (lambda (e)
         (if (not (define/fixup-contract? e))
             e
             (generate-contract-def e)))
       (syntax->list forms)))

(define (no-duplicates l)
  (= (length l) (length (remove-duplicates l))))


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
        [(Base: sym cnt) cnt]
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
                 [(arr: dom (Values: rngs) #f #f '() _ _)
                  (values (map t->c dom) (map t->c rngs) #f)]
                 [(arr: dom rng #f #f '() _ _)
                  (values (map t->c dom) (list (t->c rng)) #f)]
                 [(arr: dom (Values: rngs) rst #f '() _ _)
                  (values (map t->c dom) (map t->c rngs) (t->c rst))]
                 [(arr: dom rng rst #f '() _ _)
                  (values (map t->c dom) (list (t->c rng)) (t->c rst))]))
             (with-syntax 
                 ([(dom* ...) dom*]
                  [rng* (match rngs*
                          [(list r) r]
                          [_ #`(values #,@rngs*)])]
                  [rst* rst])
               (if rst
                   #'((dom* ...) () #:rest (listof rst*) . ->* . rng*)
                   #'(dom* ...  . -> . rng*))))
           (unless (no-duplicates (for/list ([t arrs])
                                    (match t [(arr: dom _ _ _ _ _ _) (length dom)])))
             (exit (fail)))
	   (match (map f arrs)
	     [(list e) e]
	     [l #`(case-> #,@l)]))]
        [(Vector: t)
         #`(vectorof #,(t->c t))]
        [(Pair: t1 t2)
         #`(cons/c #,(t->c t1) #,(t->c t2))]
        [(Opaque: p? cert)
         #`(flat-contract #,(cert p?))]
        [(F: v) (cond [(assoc v (vars)) => cadr]
                      [else (int-err "unknown var: ~a" v)])]
        [(Mu: n b)
         (match-let ([(Mu-name: n-nm _) ty])
           (with-syntax ([(n*) (generate-temporaries (list n-nm))])
             (parameterize ([vars (cons (list n #'n*) (vars))])
               #`(flat-rec-contract n* #,(t->c b)))))]
        [(Value: #f) #'false/c]    
        [(Instance: _) #'(is-a?/c object%)]
        [(Class: _ _ _) #'(subclass?/c object%)]
        [(Value: '()) #'null?]
        [(Struct: _ _ _ _ #f pred? cert) (cert pred?)]
        [(Syntax: (Base: 'Symbol _)) #'identifier?]
        [(Syntax: t)
         (if (equal? ty Any-Syntax)
             #`syntax?
             #`(syntax/c #,(t->c t)))]
        [(Value: v) #`(flat-named-contract #,(format "~a" v) (lambda (x) (equal? x '#,v)))]
        [(Param: in out) #`(parameter/c #,(t->c out))]
        [else          
         (exit (fail))]))))

