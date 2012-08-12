#lang racket/unit

(require (rename-in "../utils/utils.rkt" [infer r:infer])
         "signatures.rkt" "tc-metafunctions.rkt" "tc-subst.rkt"
         "check-below.rkt"
         (types utils abbrev)
         (private type-annotation parse-type)
         (env lexical-env type-alias-env global-env type-env-structs)
         (rep type-rep filter-rep object-rep)
         syntax/free-vars
         racket/match (prefix-in c: racket/contract)
         (except-in racket/contract -> ->* one-of/c)
         syntax/kerncase syntax/parse unstable/syntax

         (for-template
          racket/base
          "internal-forms.rkt"))

(require (only-in srfi/1/list s:member))

(import tc-expr^)
(export tc-let^)

(define (erase-filter tc)
  (match tc
    [(tc-results: ts _ _)
     (ret ts (for/list ([f ts]) (make-NoFilter)) (for/list ([f ts]) (make-NoObject)))]))

(define/cond-contract (do-check expr->type namess results expected-results form exprs body clauses expected #:abstract [abstract null])
     (((syntax? syntax? tc-results? . c:-> . any/c)
       (listof (listof identifier?)) (listof tc-results?) (listof tc-results?)
       syntax? (listof syntax?) syntax? (listof syntax?) (or/c #f tc-results?))
      (#:abstract any/c)
      . c:->* .
      tc-results?)
     (with-cond-contract t/p ([types          (listof (listof Type/c))] ; types that may contain undefined (letrec)
                              [expected-types (listof (listof Type/c))] ; types that may not contain undefined (what we got from the user)
                              [props          (listof (listof Filter?))])
          (define-values (types expected-types props)
            (for/lists (t e p)
              ([r     (in-list results)]
               [e-r   (in-list expected-results)]
               [names (in-list namess)])
              (match* (r e-r)
                [((tc-results: ts (FilterSet: fs+ fs-) os) (tc-results: e-ts _ _)) ; rest should be the same
                 ;(printf "f+: ~a\n" fs+)
                 ;(printf "f-: ~a\n" fs-)
                 (values ts
                         e-ts
                         (apply append
                                (for/list ([n names]
                                           [f+ fs+]
                                           [f- fs-])
                                  (list (make-ImpFilter (-not-filter (-val #f) n) f+)
                                        (make-ImpFilter (-filter (-val #f) n) f-)))))]
                [((tc-results: ts (NoFilter:) _) (tc-results: e-ts (NoFilter:) _))
                 (values ts e-ts null)]))))
     (with-cond-contract append-region ([p1 (listof Filter?)]
                                        [p2 (listof Filter?)])
          (define-values (p1 p2)
            (combine-props (apply append props) (env-props (lexical-env)) (box #t))))
     ;; extend the lexical environment for checking the body
  (with-lexical-env/extend/props
   ;; the list of lists of name
   namess
   ;; the types
   types
   (append p1 p2)
   (for-each expr->type
             clauses
             exprs
             expected-results)
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
     (with-lexical-env/extend/props
      ;; we typechecked the rhss with the lhss having types that potentially contain undefined
      ;; if undefined can actually show up, a type error will have been triggered
      ;; it is therefore safe to typecheck the body with the original types the user gave us
      ;; any undefined-related problems have been caught already
      namess
      expected-types ; types w/o undefined
      (append p1 p2)
      ;; typecheck the body
      (if expected
          (check-below
           (run (tc-exprs/check (syntax->list body) (erase-filter expected)))
           expected)
          (run (tc-exprs (syntax->list body))))))))

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

(define (tc/letrec-values namess exprs body form [expected #f])
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
         ;(if expected (tc-exprs/check (syntax->list body) expected) (tc-exprs (syntax->list body)))
         (do-check void null null null form null body null expected #:abstract orig-flat-names)]
        ;; if none of the names bound in the letrec are free vars of this rhs
        [(not (ormap (lambda (n) (s:member n flat-names bound-identifier=?))
                     (free-vars (car exprs))))
         ;; then check this expression separately
         (with-lexical-env/extend
          (list (car names))
          (list (match (get-type/infer (car names) (car exprs) (lambda (e) (tc-expr/maybe-expected/t e (car names)))
                                       tc-expr/check)
                  [(tc-results: ts) ts]))
          (loop (cdr names) (cdr exprs) (apply append (cdr names)) (cdr clauses)))]
        [else
         (do-check (lambda (stx e t) (tc-expr/check e t))
                   names
                   ;; compute set of variables that can't be undefined. see below.
                   (let-values
                    ([(safe-bindings _)
                      (for/fold ([safe-bindings              '()] ; includes transitively-safe
                                 [transitively-safe-bindings '()])
                          ([names  names]
                           [clause clauses])
                        (case (safe-letrec-values-clause? clause transitively-safe-bindings flat-names)
                          ;; transitively safe -> safe to mention in a subsequent rhs
                          [(transitively-safe) (values (append names safe-bindings)
                                                       (append names transitively-safe-bindings))]
                          ;; safe -> safe by itself, but may not be safe to use in a subsequent rhs
                          [(safe)              (values (append names safe-bindings)
                                                       transitively-safe-bindings)]
                          ;; unsafe -> could be undefined
                          [(unsafe)            (values safe-bindings transitively-safe-bindings)]))])
                    (map (λ (l) (let ([types-from-user (map get-type l)])
                                  (ret (if (andmap (λ (x) ; are all the lhs vars safe?
                                                      (s:member x safe-bindings bound-identifier=?))
                                                   l)
                                           types-from-user
                                           (map (λ (x) (make-Union (if (type<? x -Undefined)
                                                                       (list x -Undefined)
                                                                       (list -Undefined x))))
                                                types-from-user)))))
                         names))
                   ;; types the user gave. check against that to error if we could get undefined
                   (map (λ (l) (ret (map get-type l))) names)
                   form exprs body clauses expected)]))))

;; determines whether any of the variables bound in the given clause can have an undefined value
;; in this case, we cannot trust the type the user gave us and must union it with undefined
;; for example, the following code:
;;  (letrec: ([x : Float x]) x)
;; should not typecheck with type Float, even though the user said so, because the actual value
;; is undefined.
;; this implements a conservative analysis.
;; we identify 3 kinds of bindings:
;;  - safe bindings cannot be undefined
;;  - transitively safe bindings are safe and can safely be used in subsequent rhss
;;  - unsafe bindings may be undefined
;; x is transitively safe if for all its free variables, they are either transitively safe and
;; earlier in the letrec or they are bound outside the letrec
;; x is safe if it is transitively safe or if its rhs is a lambda
;; otherwise, x is unsafe
;; this function returns either 'transitively-safe, 'safe or 'unsafe
;; Note: In some cases (such as the example on the bottom of page 6 of Ghuloum and Dybvig's
;;  Fixing Letrec (reloaded) paper), we are more conservative than a fully-connected component
;;  based approach. On the other hand, our algorithm should cover most interesting cases and
;;  is much simpler than Tarjan's.
(define (safe-letrec-values-clause? clause transitively-safe-bindings letrec-bound-ids)
  (define clause-rhs
    (syntax-parse clause
      [(bindings . rhs) #'rhs]))
  (cond [(andmap (lambda (fv)
                   (or (not (s:member fv letrec-bound-ids bound-identifier=?)) ; from outside
                       (s:member fv transitively-safe-bindings bound-identifier=?)))
                 (apply append
                        (syntax-map (lambda (x) (free-vars x))
                                    clause-rhs)))
         'transitively-safe]
        [else
         (syntax-parse clause-rhs #:literal-sets (kernel-literals)
           [((#%plain-lambda _ ...)) 'safe]
           [else                     'unsafe])]))

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
    (do-check void names types types form exprs body clauses expected)))
