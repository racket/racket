#lang racket/unit

(require "../utils/utils.rkt"
         (except-in (types utils abbrev union filter-ops) -> ->* one-of/c)
         (only-in (types abbrev) (-> t:->))
         (private type-annotation parse-type syntax-properties)
         (env lexical-env type-alias-env type-alias-helper mvar-env
              global-env type-env-structs scoped-tvar-env)
         (rep type-rep filter-rep)
         syntax/free-vars
         (typecheck signatures tc-metafunctions internal-forms)
         (utils tarjan)
         racket/match (contract-req)
         racket/list
         syntax/parse syntax/stx
         syntax/id-table
         ;; For internal type forms
         (for-template (only-in racket/base define-values)))


(import tc-expr^)
(export tc-let^)

;; get-names+objects (listof (listof identifier?)) (listof tc-results?) -> (listof (list/c identifier Object?))
;; Given a list of bindings and the tc-results for the corresponding expressions, return a list of
;; tuples of the binding-name and corresponding objects from the results.
;; This is used to replace the names with the objects after the names go out of scope.
(define (get-names+objects namess results)
  (append*
    (for/list ([names namess] [results results])
      (match results
        [(tc-results: _ _ os)
         (map list names os)]))))

;; Checks that the body has the expected type when names are bound to the types spcified by results.
;; The exprs are also typechecked by using expr->type.
;; TODO: make this function sane.
(define/cond-contract (do-check expr->type namess results expected-results exprs body expected)
     ((syntax? tc-results/c . -> . any/c)
      (listof (listof identifier?)) (listof tc-results/c) (listof tc-results/c)
      (listof syntax?) syntax? (or/c #f tc-results/c)
      . -> .
      tc-results/c)
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
                                (for/list ([n (in-list names)]
                                           [f+ (in-list fs+)]
                                           [f- (in-list fs-)])
                                  (if (is-var-mutated? n)
                                      (list)
                                      (list (-imp (-not-filter (-val #f) n) f+)
                                            (-imp (-filter (-val #f) n) f-))))))]
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
             exprs
             expected-results)
   (with-lexical-env/extend/props
    ;; we typechecked the rhss with the lhss having types that potentially contain undefined
    ;; if undefined can actually show up, a type error will have been triggered
    ;; it is therefore safe to typecheck the body with the original types the user gave us
    ;; any undefined-related problems have been caught already
    namess
    expected-types ; types w/o undefined
    (append p1 p2)
    ;; typecheck the body
    (replace-names (get-names+objects namess expected-results)
      (if expected
        (tc-body/check body (erase-filter expected))
        (tc-body body))))))

(define (tc-expr/maybe-expected/t e names)
  (syntax-parse names
    [(i:typed-id^ ...)
     (tc-expr/check e (-values (attribute i.type)))]
    [_ (tc-expr e)]))

(define (tc/letrec-values namess exprs body [expected #f])
  (let* ([names (stx-map syntax->list namess)]
         [orig-flat-names (apply append names)]
         [exprs (syntax->list exprs)])
    ;; Collect the declarations, which are represented as expression.
    ;; We put them back into definitions to reuse the existing machinery
    (define-values (type-aliases declarations)
      (for/fold ([aliases '()] [declarations '()])
                ([body (in-list exprs)])
        (syntax-parse #`(define-values () #,body)
          [t:type-alias
           (values (cons #'t aliases) declarations)]
          [t:type-declaration
           (values aliases (cons (list #'t.id #'t.type) declarations))]
          [_ (values aliases declarations)])))

    (define-values (alias-names alias-map) (get-type-alias-info type-aliases))
    (register-all-type-aliases alias-names alias-map)

    (for ([declaration declarations])
      (match-define (list id type) declaration)
      (register-type-if-undefined id (parse-type type))
      (register-scoped-tvars id (parse-literal-alls type)))

    ;; add scoped type variables, before we get to typechecking
    ;; FIXME: can this pass be fused with the one immediately above?
    (for ([n (in-list names)] [b (in-list exprs)])
      (syntax-case n ()
        [(var) (add-scoped-tvars b (lookup-scoped-tvars #'var))]
        [_ (void)]))

    ;; First look at the clauses that do not bind the letrec names
    (define all-clauses
      (for/list ([name-lst names] [expr exprs])
        (lr-clause name-lst expr)))

    (define-values (ordered-clauses remaining)
      (get-non-recursive-clauses all-clauses orig-flat-names))

    (define-values (remaining-names remaining-exprs)
      (for/lists (_1 _2) ([remaining-clause remaining])
        (match-define (lr-clause name expr) remaining-clause)
        (values name expr)))

    ;; Check those and gather an environment for use below
    (define-values (env-names env-results)
      (check-non-recursive-clauses ordered-clauses))

    (replace-names (get-names+objects env-names env-results)
      (with-lexical-env/extend env-names (map tc-results-ts env-results)
        (cond
          ;; after everything, check the body expressions
          [(null? remaining-names)
             (if expected
               (tc-body/check body (erase-filter expected))
               (tc-body body))]
          [else
           (define flat-names (apply append remaining-names))
           (do-check tc-expr/check
                     remaining-names
                     ;; compute set of variables that can't be undefined. see below.
                     (let-values
                      ([(safe-bindings _)
                        (for/fold ([safe-bindings              '()] ; includes transitively-safe
                                   [transitively-safe-bindings '()])
                            ([names  (in-list remaining-names)]
                             [expr (in-list remaining-exprs)])
                          (case (safe-letrec-values-clause? expr transitively-safe-bindings flat-names)
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
                                                        (member x safe-bindings bound-identifier=?))
                                                     l)
                                             types-from-user
                                             (map (λ (x) (Un x -Undefined)) types-from-user)))))
                           remaining-names))
                     ;; types the user gave. check against that to error if we could get undefined
                     (map (λ (l) (ret (map get-type l))) remaining-names)
                     remaining-exprs body expected)])))))

;; An lr-clause is a
;;   (lr-clause (Listof Identifier) Syntax)
;;
;; interp. represents a letrec binding
(struct lr-clause (names expr) #:transparent)

;; get-non-recursive-clauses : (Listof lr-clause) (Listof Identifier) ->
;;                             (Listof lr-clause) (Listof lr-clause)
;; Find letrec-values clauses that do not create variable cycles. Return
;; both the non-recursive clauses and the remaining recursive ones.
(define (get-non-recursive-clauses clauses flat-names)

  ;; First, filter out clauses with no names. Don't do cycle checking on
  ;; these because they trivially don't form any.
  (define-values (*non-binding *other-clauses)
    (for/fold ([non-binding '()] [other-clauses '()])
              ([clause clauses])
      (match-define (lr-clause names _) clause)
      (if (null? names)
          (values (cons clause non-binding) other-clauses)
          (values non-binding (cons clause other-clauses)))))
  (define-values (non-binding other-clauses)
    (values (reverse *non-binding) (reverse *other-clauses)))

  ;; Set up vertices for Tarjan's algorithm, where each letrec-values
  ;; clause is a vertex but mapped in the table for each of the clause names
  (define vertices (make-bound-id-table))
  (for ([clause other-clauses])
    (match-define (lr-clause names expr) clause)
    (define relevant-free-vars
      (for/list ([var (in-list (free-vars expr))]
                 #:when (member var flat-names bound-identifier=?))
        var))
    (define vertex (make-vertex clause relevant-free-vars))
    (for ([name (in-list names)])
      (bound-id-table-set! vertices name vertex)))

  (define components (tarjan vertices))

  ;; no-self-cycle? : (Vertex Id (Listof Id)) -> Boolean
  (define (no-self-cycle? vertex)
    (match-define (lr-clause names _) (vertex-data vertex))
    (for/and ([id (in-list names)])
      (andmap (λ (id2) (not (bound-identifier=? id id2)))
              (vertex-adjacent vertex))))

  ;; The components with only one entry are non-recursive if they also
  ;; contain no self-cycles.
  (define-values (non-recursive remaining)
    (for/fold ([non-recursive '()]
               [remaining '()])
              ([component components])
      (cond [(and (= (length component) 1)
                  (no-self-cycle? (car component)))
             (values (cons (vertex-data (car component)) non-recursive)
                     remaining)]
            [else
             (values non-recursive
                     (append (map vertex-data component)
                             remaining))])))
  (values (append non-recursive non-binding)
          remaining))

;; check-non-recursive-clauses : (Listof lr-clause) ->
;;                               (Listof (Listof Identifier)) (Listof tc-results)
;; Given a list of non-recursive clauses, check the clauses in order and
;; build up a type environment for use in the second pass.
(define (check-non-recursive-clauses clauses)
  (let loop ([clauses clauses] [env-ids '()] [env-types '()])
    (cond [(null? clauses) (values env-ids env-types)]
          [else
           (match-define (lr-clause names expr) (car clauses))
           (define results
             (get-type/infer names expr
                             (lambda (e) (tc-expr/maybe-expected/t e names))
                             tc-expr/check))
           (with-lexical-env/extend names (tc-results-ts results)
             (loop (cdr clauses)
                   (cons names env-ids)
                   (cons results env-types)))])))

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
(define (safe-letrec-values-clause? expr transitively-safe-bindings letrec-bound-ids)
  (cond [(andmap (lambda (fv)
                   (or (not (member fv letrec-bound-ids bound-identifier=?)) ; from outside
                       (member fv transitively-safe-bindings bound-identifier=?)))
                 (free-vars expr))
         'transitively-safe]
        [else
         (syntax-parse expr #:literal-sets (kernel-literals)
           [(#%plain-lambda _ ...) 'safe]
           [else                   'unsafe])]))

;; this is so match can provide us with a syntax property to
;; say that this binding is only called in tail position
(define ((tc-expr-t/maybe-expected expected) e)
  (syntax-parse e #:literal-sets (kernel-literals)
    [(~and (#%plain-lambda () _) _:tail-position^)
     #:when expected
     (tc-expr/check e (ret (t:-> (tc-results->values expected))))]
    [_:tail-position^
     #:when expected
     (tc-expr/check e expected)]
    [_ (tc-expr e)]))

(define (tc/let-values namess exprs body [expected #f])
  (let* (;; a list of each name clause
         [names (stx-map syntax->list namess)]
         ;; all the trailing expressions - the ones actually bound to the names
         [exprs (syntax->list exprs)]
         ;; the types of the exprs
         #;[inferred-types (map (tc-expr-t/maybe-expected expected) exprs)]
         ;; the annotated types of the name (possibly using the inferred types)
         [types (for/list ([name (in-list names)] [e (in-list exprs)])
                  (get-type/infer name e (tc-expr-t/maybe-expected expected)
                                         tc-expr/check))])
    (do-check void names types types exprs body expected)))
