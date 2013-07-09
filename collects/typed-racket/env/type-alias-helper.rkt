#lang racket/base

;; This module provides helper functions for type aliases

(require "../utils/utils.rkt"
         (utils tc-utils)
         (env type-alias-env type-name-env)
         (rep type-rep)
         (private parse-type)
         (typecheck internal-forms)
         (types resolve base-abbrev)
	 data/queue
         racket/dict
         racket/format
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

(module+ test (require rackunit))

;; A Vertex is a
;;   (vertex Identifier Boolean Option<Integer> Option<Integer> Listof<Id>)
;;
;; interp. a vertex in a graph, we only use this for Tarjan's algorithm
;;   id       - identifier (labels vertices)
;;   stack?   - whether this vertex is on the stack (for speed)
;;   index    - index tracked in Tarjan's algorithm
;;   lowlink  - see index
;;   adjacent - list of adjacent vertices
(struct vertex (id [stack? #:mutable] [index #:mutable]
                   [lowlink #:mutable] adjacent)
        #:transparent)

;; Dict<Id, (List Type Listof<Id>)> -> Listof<Listof<Id>>
;; Find strongly connected type aliases in order to
;; find mutually recursive aliases
;;
;; Returns the components in topologically sorted order
(define (find-strongly-connected-type-aliases dep-map)
  (define vertex-map (make-free-id-table))
  (for ([(id adjacent) (in-dict dep-map)])
    (free-id-table-set! vertex-map id (vertex id #f #f #f adjacent)))
  ;; Implements Tarjan's algorithm. See Wikipedia
  ;; http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
  (define (tarjan vertices)
    (define (strongly-connected vtx)
      (set-vertex-index! vtx index)
      (set-vertex-lowlink! vtx index)
      (set! index (add1 index))
      (enqueue-front! stack vtx)
      (set-vertex-stack?! vtx #t)
      (for ([successor-id (in-list (vertex-adjacent vtx))])
        (define successor (free-id-table-ref vertices successor-id))
        (cond [(not (vertex-index successor))
               (strongly-connected successor)
               (set-vertex-lowlink! vtx
                                    (min (vertex-lowlink vtx)
                                         (vertex-lowlink successor)))]
              [(vertex-stack? successor)
               (set-vertex-lowlink! vtx
                                    (min (vertex-lowlink vtx)
                                         (vertex-index successor)))]))
      ;; sets a result component if this was a root vertex
      (when (= (vertex-lowlink vtx) (vertex-index vtx))
        (define new-scc
          (for/list ([elem (in-queue stack)]
                     #:final (equal? vtx elem))
            (dequeue! stack)
            (set-vertex-stack?! vtx #f)
            (vertex-id elem)))
        (set! sccs (cons new-scc sccs))))

    ;; the body
    (define index 0)
    (define stack (make-queue))
    (define sccs '())
    (for ([(id vtx) (in-dict vertices)]
          #:unless (vertex-index vtx))
      (strongly-connected vtx))
    sccs)
  (tarjan vertex-map))

(module+ test
  ;; two aliases in their own components
  (define example-1
    (list (cons #'x (list #'x))
          (cons #'y (list #'y))))
  ;; all one component
  (define example-2
    (list (cons #'x (list #'x #'y))
          (cons #'y (list #'x))))
  ;; two components, one with two nodes
  (define example-3
    (list (cons #'x (list #'y))
          (cons #'y (list #'x))
          (cons #'z (list))))
  ;; one with cycles, two that form a line
  (define example-4
    (list (cons #'x (list #'y))
          (cons #'y (list #'x))
          (cons #'a (list #'b))
          (cons #'b (list))))
  ;; two large cycles
  (define example-5
    (list (cons #'x (list #'y #'z))
          (cons #'y (list #'x))
          (cons #'z (list #'x #'y))
          (cons #'a (list #'b))
          (cons #'b (list #'c))
          (cons #'c (list #'a))))
  ;; check topological order
  (define example-6
    (list (cons #'a (list #'b))
          (cons #'d (list))
          (cons #'c (list #'d #'e))
          (cons #'b (list #'c))
          (cons #'e (list #'f))
          (cons #'f (list))))

  ;; helper function for the tests below
  ;; ignores order of ids in the components
  (define (equal-id-sets? x y)
    (define (id-lsts->id-sets id-lsts)
      (for/list ([id-lst id-lsts])
        (for/fold ([id-tbl (make-immutable-free-id-table)])
                  ([id id-lst])
          (free-id-table-set id-tbl id #t))))
    (equal? (id-lsts->id-sets x)
            (id-lsts->id-sets y)))

  (define-binary-check (check-equal?/id equal-id-sets? actual expected))

  (check-equal?/id (find-strongly-connected-type-aliases example-1)
                   (list (list #'y) (list #'x)))
  (check-equal?/id (find-strongly-connected-type-aliases example-2)
                   (list (list #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-3)
                   (list (list #'z) (list #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-4)
                   (list (list #'a) (list #'b) (list #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-5)
                   (list (list #'b #'a #'c) (list #'z #'x #'y)))
  (check-equal?/id (find-strongly-connected-type-aliases example-6)
                   (list (list #'a) (list #'b) (list #'c)
                         (list #'e) (list #'f) (list #'d))))

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
    [((Name: _ orig-id _ _ _))
     (and (not (free-identifier=? orig-id id))
          (check (resolve-once type)))]
    [((App: rator rands stx))
     (and (check rator) (check rands))]
    [((Mu: _ body)) (check body)]
    [((Poly: names body)) (check body)]
    [((PolyDots: names body)) (check body)]
    [(_) #t])
  (unless (check type)
    (tc-error/stx
     id
     "Recursive types are not allowed directly inside their definition")))

;; get-type-alias-info : Listof<Syntax> -> Listof<Id> Dict<Id, TypeAliasInfo>
;;
;; Given the syntaxes representing type alias definitions, return
;; the information needed to register them later
(define (get-type-alias-info type-aliases)
  (for/lists (_1 _2) ([type-alias (in-list type-aliases)])
    (define-values (id name-id type-stx args) (parse-type-alias type-alias))
    ;; Register type alias names with a dummy value so that it's in
    ;; scope for the registration later.
    (register-resolved-type-alias id Err)
    (register-type-name
     name-id
     (if args
         (make-Poly (map syntax-e args) Err)
         Err))
    (values id (list id name-id type-stx args))))

;; register-all-type-aliases : Listof<Id> Dict<Id, TypeAliasInfo> -> Void
;;
;; Given parsed type aliases and a type alias map, do the work
;; of actually registering the type aliases. If struct names or
;; other definitions need to be registered, do that before calling
;; this function.
(define (register-all-type-aliases type-alias-names type-alias-map)
  ;; Find type alias dependencies
  (define-values (type-alias-dependency-map type-alias-types)
    (for/lists (_1 _2)
      ([(name alias-info) (in-dict type-alias-map)])
      (define links-box (box null))
      (define type
        (parameterize ([current-referenced-aliases links-box])
          (parse-type (cadr alias-info))))
      (define pre-dependencies (unbox links-box))
      (define alias-dependencies
        (filter (λ (id) (memf (λ (id2) (free-identifier=? id id2))
                              type-alias-names))
                pre-dependencies))
      (values (cons name alias-dependencies) type)))

  (define components
    (find-strongly-connected-type-aliases type-alias-dependency-map))

  ;; helper function for defining singletons
  (define (has-self-cycle? component)
    (define id (car component))
    (memf (λ (id2) (free-identifier=? id id2))
          (dict-ref type-alias-dependency-map id)))

  ;; A singleton component can be either a self-cycle or a node that
  ;; that does not participate in cycles, so we disambiguate
  (define acyclic-singletons
    (for/list ([component (in-list components)]
               #:when (= (length component) 1)
               #:unless (has-self-cycle? component))
      (car component)))

  ;; Register non-recursive type aliases
  ;;
  ;; Note that the connected component algorithm returns results
  ;; in topologically sorted order, so we want to go through in the
  ;; reverse order of that to avoid unbound type aliases.
  (for ([id (in-list (reverse acyclic-singletons))])
    (define type-stx (cadr (dict-ref type-alias-map id)))
    (register-resolved-type-alias id (parse-type type-stx)))

  ;; Actually register recursive type aliases
  (for ([(id record) (in-dict type-alias-map)]
        #:unless (member id acyclic-singletons))
    (match-define (list rec-name _ args) record)
    (define deps (dict-ref type-alias-dependency-map id))
    (register-resolved-type-alias id (make-Name rec-name id deps args #f)))

  (define-values (names-to-refine types-to-refine tvarss)
    (for/lists (_1 _2 _3)
               ([(id record) (in-dict type-alias-map)]
                #:unless (member id acyclic-singletons))
      (match-define (list rec-name type-stx args) record)
      (define type
        ;; make sure to reject the type if it uses polymorphic
        ;; recursion (see resolve.rkt)
        (parameterize ([current-check-polymorphic-recursion args])
          (parse-type type-stx)))
      (register-type-name rec-name type)
      (add-constant-variance! rec-name args)
      (check-type-alias-contractive id type)
      (values rec-name type args)))

  ;; Finally, do a last pass to refine the variance
  (refine-variance! names-to-refine types-to-refine tvarss))

;; Syntax -> Syntax Syntax Syntax Option<Integer>
;; Parse a type alias internal declaration
(define (parse-type-alias form)
  (kernel-syntax-case* form #f
    (define-type-alias-internal values)
    [(define-values ()
       (begin (quote-syntax (define-type-alias-internal nm rec-nm ty args))
              (#%plain-app values)))
     (values #'nm #'rec-nm #'ty (syntax-e #'args))]
    ;; this version is for `let`-like bodies
    [(begin (quote-syntax (define-type-alias-internal nm rec-nm ty args))
            (#%plain-app values))
     (values #'nm #'rec-nm #'ty (syntax-e #'args))]
    [_ (int-err "not define-type-alias")]))

