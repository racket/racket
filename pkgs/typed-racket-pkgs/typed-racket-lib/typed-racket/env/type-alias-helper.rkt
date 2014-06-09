#lang racket/base

;; This module provides helper functions for type aliases

(require "../utils/utils.rkt"
         (utils tarjan tc-utils)
         (env type-alias-env type-name-env)
         (rep type-rep)
         (private parse-type)
         (typecheck internal-forms)
         (types resolve base-abbrev)
	 data/queue
         racket/dict
         racket/format
         racket/list
         racket/match
         syntax/id-table
         syntax/kerncase
         (for-template
          (typecheck internal-forms)
          racket/base))

(provide find-strongly-connected-type-aliases
         check-type-alias-contractive
         get-type-alias-info
         register-all-type-aliases
         parse-type-alias)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Data definitions for aliases
;;
;; A TypeAliasInfo is a (list Syntax (Listof Identifier))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dict<Id, (List Type Listof<Id>)> -> Listof<Listof<Id>>
;; Find strongly connected type aliases in order to
;; find mutually recursive aliases
;;
;; Returns the components in topologically sorted order
(define (find-strongly-connected-type-aliases dep-map)
  (define vertex-map (make-free-id-table))
  (for ([(id adjacent) (in-dict dep-map)])
    (free-id-table-set! vertex-map id (make-vertex id adjacent)))
  (define components (tarjan vertex-map))
  ;; extract the identifiers out of the results since we
  ;; don't need the whole vertex
  (for/list ([component components])
    (map vertex-data component)))

;; check-type-alias-contractive : Id Type -> Void
;;
;; This function checks if the given type alias is
;; "contractive" or "productive"
;;   i.e., that you can unfold a good type like μx.int->x to
;;         μx.int->int->int->...x but a type like
;;         μx.x only unfolds to itself
;;
(define (check-type-alias-contractive id type)
  (define/match (check type)
    [((Union: elems)) (andmap check elems)]
    [((Name: name-id _ _ _))
     (and (not (free-identifier=? name-id id))
          (check (resolve-once type)))]
    [((App: rator rands stx))
     (and (check rator) (check rands))]
    [((Mu: _ body)) (check body)]
    [((Poly: names body)) (check body)]
    [((PolyDots: names body)) (check body)]
    [((PolyRow: _ _ body)) (check body)]
    [(_) #t])
  (unless (check type)
    (tc-error/fields
     "parse error in type"
     #:stx id
     #:more "recursive types are not allowed directly inside their definition")))

;; get-type-alias-info : Listof<Syntax> -> Listof<Id> Dict<Id, TypeAliasInfo>
;;
;; Given the syntaxes representing type alias definitions, return
;; the information needed to register them later
(define (get-type-alias-info type-aliases)
  (for/lists (_1 _2) ([type-alias (in-list type-aliases)])
    (define-values (id type-stx args) (parse-type-alias type-alias))
    ;; Register type alias names with a dummy value so that it's in
    ;; scope for the registration later.
    ;;
    ;; The `(make-Value (gensym))` expression is used to make sure
    ;; that unions don't collapse the aliases too soon.
    (register-resolved-type-alias id Err)
    (register-type-name
     id
     (if args
         (make-Poly (map syntax-e args) (make-Value (gensym)))
         (make-Value (gensym))))
    (values id (list id type-stx args))))

;; register-all-type-aliases : Listof<Id> Dict<Id, TypeAliasInfo> -> Void
;;
;; Given parsed type aliases and a type alias map, do the work
;; of actually registering the type aliases. If struct names or
;; other definitions need to be registered, do that before calling
;; this function.
(define (register-all-type-aliases type-alias-names type-alias-map)
  ;; Find type alias dependencies
  ;; The two maps defined here contains the dependency structure
  ;; of type aliases in two senses:
  ;;   (1) other type aliases referenced in a type alias
  ;;   (2) other type aliases referenced by some class in a
  ;;       type alias in a #:implements clause
  ;;
  ;; The second is necessary in order to prevent recursive
  ;; #:implements clauses and to determine the order in which
  ;; recursive type aliases should be initialized.
  (define-values (type-alias-dependency-map type-alias-class-map)
    (for/lists (_1 _2)
      ([(name alias-info) (in-dict type-alias-map)])
      (define links-box (box null))
      (define class-box (box null))
      (define type
        (parameterize ([current-type-alias-name name]
                       [current-referenced-aliases links-box]
                       [current-referenced-class-parents class-box])
          (parse-type (car alias-info))))
      (define pre-dependencies
        (remove-duplicates (unbox links-box) free-identifier=?))
      (define (filter-by-type-alias-names names)
        (for/list ([id (in-list names)]
                   #:when (memf (λ (id2) (free-identifier=? id id2))
                                type-alias-names))
          id))
      (define alias-dependencies
        (filter-by-type-alias-names pre-dependencies))
      (define class-dependencies
        (filter-by-type-alias-names (unbox class-box)))
      (values (cons name alias-dependencies)
              (cons name class-dependencies))))

  (define components
    (find-strongly-connected-type-aliases type-alias-dependency-map))

  (define class-components
    (find-strongly-connected-type-aliases type-alias-class-map))

  ;; helper function for defining singletons
  (define (has-self-cycle? component [map type-alias-dependency-map])
    (define id (car component))
    (memf (λ (id2) (free-identifier=? id id2))
          (dict-ref map id)))

  ;; A singleton component can be either a self-cycle or a node that
  ;; that does not participate in cycles, so we disambiguate
  (define-values (acyclic-singletons recursive-aliases)
    (for/fold ([singletons '()] [other '()])
              ([component (in-list components)])
      (if (and (= (length component) 1)
               (not (has-self-cycle? component)))
          (values (cons (car component) singletons) other)
          (values singletons (append component other)))))

  ;; Check that no #:implements clauses are recursive
  (define counterexample
    (for/or ([component class-components])
      (and (or (not (= (length component) 1))
               (has-self-cycle? component type-alias-class-map))
           component)))
  (when counterexample
    (tc-error/stx
     (car counterexample)
     "Recursive #:implements clause not allowed"))

  ;; Split recursive aliases into those involving classes
  ;; (in reverse topological order) and the rest of the aliases
  (define class-aliases
    (for/list ([component (in-list (reverse class-components))]
               #:when (member (car component)
                              recursive-aliases
                              free-identifier=?))
      (car component)))
  (define other-recursive-aliases
    (for/list ([alias (in-list recursive-aliases)]
               #:unless (member alias
                                class-aliases
                                free-identifier=?))
      alias))

  ;; Reconstruct type alias dependency map based on class parent
  ;; information. This ensures that the `deps` field is precise
  ;; in all type aliases involving class types
  (define (get-all-parent-deps id)
    (define (get-deps parent)
      (cdr (assoc parent type-alias-dependency-map free-identifier=?)))
    (define parents (cdr (assoc id type-alias-class-map free-identifier=?)))
    (cond [(null? parents) null]
          [else
           (define all-deps
             (for/list ([parent parents])
               (append (get-deps parent)
                       (get-all-parent-deps parent))))
           (apply append all-deps)]))

  (define new-dependency-map/classes
    (for/list ([(id deps) (in-dict type-alias-dependency-map)])
      (cond [(dict-has-key? type-alias-class-map id)
             (define new-deps
               (remove-duplicates (append (get-all-parent-deps id) deps)
                                  free-identifier=?))
             (cons id new-deps)]
            [else (cons id deps)])))

  ;; Do another pass on dependency map, using the connected
  ;; components analysis data to determine which dependencies are
  ;; actually needed for mutual recursion. Drop all others.
  (define new-dependency-map
    (for/list ([(id deps) (in-dict new-dependency-map/classes)])
      ;; find the component this `id` participated in so
      ;; that we can drop `deps` that aren't in that component
      (define component
        (findf (λ (component) (member id component free-identifier=?))
               components))
      (define new-deps
        (filter (λ (dep) (member dep component free-identifier=?)) deps))
      (cons id new-deps)))

  ;; Actually register recursive type aliases
  (define name-types
    (for/list ([id (in-list recursive-aliases)])
      (define record (dict-ref type-alias-map id))
      (match-define (list _ args) record)
      (define deps (dict-ref new-dependency-map id))
      (define name-type (make-Name id deps args #f))
      (register-resolved-type-alias id name-type)
      name-type))

  ;; Register non-recursive type aliases
  ;;
  ;; Note that the connected component algorithm returns results
  ;; in topologically sorted order, so we want to go through in the
  ;; reverse order of that to avoid unbound type aliases.
  (for ([id (in-list acyclic-singletons)])
    (define type-stx (car (dict-ref type-alias-map id)))
    (register-resolved-type-alias id (parse-type type-stx)))

  ;; Clear the resolver cache of Name types from this block
  (define (reset-resolver-cache!) (resolver-cache-remove! name-types))
  (reset-resolver-cache!)

  ;; Finish registering recursive aliases
  ;; names-to-refine : Listof<Id>
  ;; types-to-refine : Listof<Type>
  ;; tvarss          : Listof<Listof<Symbol>>
  (define-values (names-to-refine types-to-refine tvarss)
    (for/lists (_1 _2 _3)
      ([id (in-list (append other-recursive-aliases class-aliases))])
      (define record (dict-ref type-alias-map id))
      (match-define (list type-stx args) record)
      (define type
        ;; make sure to reject the type if it uses polymorphic
        ;; recursion (see resolve.rkt)
        (parameterize ([current-check-polymorphic-recursion args])
          (parse-type type-stx)))
      (reset-resolver-cache!)
      (register-type-name id type)
      (add-constant-variance! id args)
      (check-type-alias-contractive id type)
      (values id type args)))

  ;; Finally, do a last pass to refine the variance
  (refine-variance! names-to-refine types-to-refine tvarss))

;; Syntax -> Syntax Syntax Syntax Option<Integer>
;; Parse a type alias internal declaration
(define (parse-type-alias form)
  (kernel-syntax-case* form #f
    (define-type-alias-internal values)
    [(define-values ()
       (begin (quote-syntax (define-type-alias-internal nm ty args))
              (#%plain-app values)))
     (values #'nm #'ty (syntax-e #'args))]
    ;; this version is for `let`-like bodies
    [(begin (quote-syntax (define-type-alias-internal nm ty args))
            (#%plain-app values))
     (values #'nm #'ty (syntax-e #'args))]
    [_ (int-err "not define-type-alias")]))

