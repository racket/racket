#lang scheme/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer]))
(require "signatures.rkt" "tc-metafunctions.rkt" "tc-subst.rkt"
         (types utils convenience)
         (private type-annotation parse-type)
	 (env lexical-env type-alias-env global-env type-env-structs)
         (rep type-rep)
         syntax/free-vars
         mzlib/trace unstable/debug
         scheme/match (prefix-in c: scheme/contract)
         (except-in scheme/contract -> ->* one-of/c)
         syntax/kerncase syntax/parse
         (for-template 
          scheme/base
          "internal-forms.rkt"))

(require (only-in srfi/1/list s:member))


(import tc-expr^)
(export tc-let^)

(define (erase-filter tc)
  (match tc
    [(tc-results: ts _ _)
     (ret ts (for/list ([f ts]) (make-NoFilter)) (for/list ([f ts]) (make-NoObject)))]))

(d/c (do-check expr->type namess results form exprs body clauses expected #:abstract [abstract null])
     (((syntax? syntax? tc-results? . c:-> . any/c)
       (listof (listof identifier?)) (listof tc-results?)
       syntax? (listof syntax?) syntax? (listof syntax?) (or/c #f tc-results?))
      (#:abstract any/c)
      . c:->* . 
      tc-results?)
     (w/c t/p ([types (listof (listof Type/c))]
               [props (listof (listof Filter?))])
          (define-values (types props)
            (for/lists (t p) 
              ([r (in-list results)]
               [names (in-list namess)])
              (match r 
                [(tc-results: ts (FilterSet: fs+ fs-) os)
                 ;(printf "f+: ~a~n" fs+)
                 ;(printf "f-: ~a~n" fs-)
                 (values ts
                         (apply append
                                (for/list ([n names]
                                           [f+ fs+]
                                           [f- fs-])
                                  (list (make-ImpFilter (-not-filter (-val #f) n) f+)
                                        (make-ImpFilter (-filter (-val #f) n) f-)))))]
                [(tc-results: ts (NoFilter:) _) (values ts null)]))))
     ;; extend the lexical environment for checking the body
  (with-lexical-env/extend/props
   ;; the list of lists of name
   namess
   ;; the types
   types
   (w/c append-region
    #:result (listof Filter?)
    (define-values (p1 p2)
      (combine-props (apply append props) (env-props (lexical-env)) (box #t)))
    (append p1 p2))
   (for-each expr->type
             clauses
             exprs 
             results)
   (let ([subber (lambda (proc lst)
                   (for/list ([i (in-list lst)])
                     (for/fold ([s i])
                       ([nm (in-list (apply append abstract namess))])
                       (proc s nm (make-Empty) #t))))])
     (define (run res)
       (match res
         [(tc-results: ts fs os)
          (ret (subber subst-type ts) (subber subst-filter-set fs) (subber subst-object os))]
         [(tc-results: ts fs os dt db)
          (ret (subber subst-type ts) (subber subst-filter-set fs) (subber subst-object os) dt db)]))
     (if expected 
         (check-below 
          (run (tc-exprs/check (syntax->list body) (erase-filter expected)))
          expected)
         (run (tc-exprs (syntax->list body)))))))

(define (tc/letrec-values/check namess exprs body form expected)
  (tc/letrec-values/internal namess exprs body form expected))

(define (tc/letrec-values namess exprs body form)
  (tc/letrec-values/internal namess exprs body form #f))

(define (tc-expr/maybe-expected/t e name)
  (define expecteds
    (map (lambda (stx) (lookup-type stx (lambda () #f))) name))
  (define mk (if (and (pair? expecteds) (null? (cdr expecteds)))
                 car
                 -values))
  (define tcr
    (if
     (andmap values expecteds)
     (tc-expr/check e (mk expecteds))
     (tc-expr e)))
  tcr)

(define (tc/letrec-values/internal namess exprs body form expected)
  (let* ([names (map syntax->list (syntax->list namess))]
         [orig-flat-names (apply append names)]
         [exprs (syntax->list exprs)]
         ;; the clauses for error reporting
         [clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)])])
    (for-each (lambda (names body)
                (kernel-syntax-case* body #f (values :-internal define-type-alias-internal)
                  [(begin (quote-syntax (define-type-alias-internal nm ty)) (#%plain-app values))
                   (register-resolved-type-alias #'nm (parse-type #'ty))]
                  [(begin (quote-syntax (:-internal nm ty)) (#%plain-app values))
                   (register-type-if-undefined #'nm (parse-type #'ty))]
                  [_ (void)]))
              names
              exprs)
    (let loop ([names names] [exprs exprs] [flat-names orig-flat-names] [clauses clauses])
      (cond 
        ;; after everything, check the body expressions
        [(null? names) 
         (do-check void null null form null body null expected #:abstract orig-flat-names)
         #;
         (if expected (tc-exprs/check (syntax->list body) expected) (tc-exprs (syntax->list body)))]
        ;; if none of the names bound in the letrec are free vars of this rhs
        [(not (ormap (lambda (n) (s:member n flat-names bound-identifier=?)) (free-vars (car exprs))))
         ;; then check this expression separately
         (with-lexical-env/extend
          (list (car names))
          (list (match (get-type/infer (car names) (car exprs) (lambda (e) (tc-expr/maybe-expected/t e (car names))) 
                                       tc-expr/check)
                  [(tc-results: ts) ts]))
          (loop (cdr names) (cdr exprs) (apply append (cdr names)) (cdr clauses)))]
        [else
         ;(for-each (lambda (vs) (for-each (lambda (v) (printf/log "Letrec Var: ~a~n" (syntax-e v))) vs)) names)
         (do-check (lambda (stx e t) (tc-expr/check e t))
                   names (map (λ (l) (ret (map get-type l))) names) form exprs body clauses expected)]))))

;; this is so match can provide us with a syntax property to
;; say that this binding is only called in tail position
(define ((tc-expr-t/maybe-expected expected) e)
  (syntax-parse e #:literals (#%plain-lambda)
    [(#%plain-lambda () _)
     #:fail-unless (and expected (syntax-property e 'typechecker:called-in-tail-position)) #f
     (tc-expr/check e (ret (-> (tc-results->values expected))))]
    [_ 
     #:fail-unless (and expected (syntax-property e 'typechecker:called-in-tail-position)) #f
     (tc-expr/check e expected)]
    [_ (tc-expr e)]))

(define (tc/let-values namess exprs body form [expected #f])
  (let* (;; a list of each name clause
         [names (map syntax->list (syntax->list namess))]
         ;; all the trailing expressions - the ones actually bound to the names
         [exprs (syntax->list exprs)]
         ;; the types of the exprs
         #;[inferred-types (map (tc-expr-t/maybe-expected expected) exprs)]
         ;; the annotated types of the name (possibly using the inferred types)
         [types (for/list ([name names] [e exprs]) 
                  (get-type/infer name e (tc-expr-t/maybe-expected expected) 
                                         tc-expr/check))]
         ;; the clauses for error reporting
         [clauses (syntax-case form () [(lv cl . b) (syntax->list #'cl)])])
    (do-check void names types form exprs body clauses expected)))


