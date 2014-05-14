#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/list
         (except-in (types abbrev union utils filter-ops)
                    -> ->* one-of/c)
         (rep type-rep filter-rep object-rep rep-utils)
         (typecheck tc-subst)
         (contract-req))

(provide abstract-results
         combine-props
         tc-results->values)

(provide/cond-contract
  [replace-names (-> (listof (list/c identifier? Object?)) tc-results/c tc-results/c)])


;; abstract-results
;;
;; Given results from the range of a lambda, abstract any
;; identifier objects into index (numeric) objects. This is effectively
;; doing a kind of De Bruijn indexing for objects.
;;
;; When the body of a lambda is type-checked, its filters and object
;; may refer to variables that are in scope in that body. Since these
;; names are not in scope outside of the lambda, the type of the function
;; will instead store their De Bruijn indices.
;;
;; For example, given the function
;;
;;   (λ (x) (number? x))
;;
;; the typechecker will check the body and return
;;
;;   Boolean ; N_x | !N_x ; ∅
;;
;; but the `x`s have to be converted to indices to get the type
;;
;;      N_(0,0) | !N_(0,0)
;; Any -------------------> Boolean
;;              ∅
;;
;; where the index (0,0) indicates the first argument of
;; the current function
;;
;; Comparatively, a curried predicate like
;;
;;   (λ (x) (λ (y)(number? x)))
;;
;; gets the type
;;
;;             N_(1,0) | !N_(1,0)
;; Any -> Any -------------------> Boolean
;;                     ∅
;;
;; (ignoring filters on the first arrow)
;; where the index (1,0) indicates the first argument of
;; the enclosing lambda.
;;
;; The paper "Logical Types for Untyped Languages" takes a different
;; approach where all function types carry their names, so that the first
;; example would have the type:
;;
;;        N_x | !N_x
;; x:Any ------------> Boolean
;;            ∅
;;
;; See tc-subst.rkt for the functions that take an abstracted function
;; type and substitute in a concrete object.
;;
(define/cond-contract (abstract-results results arg-names)
  (tc-results/c (listof identifier?) . -> . SomeValues/c)
  (define keys (for/list ([(nm k) (in-indexed arg-names)]) (list 0 k)))
  (match results
    [(tc-any-results:) (make-AnyValues)]
    [(tc-results: ts fs os dty dbound)
     (make-ValuesDots
      (for/list ([t (in-list ts)] [f (in-list fs)] [o (in-list os)])
        (-result (abstract-type arg-names keys t)
                 (abstract-filter arg-names keys f)
                 (abstract-object arg-names keys o)))
      (abstract-type arg-names keys dty)
      dbound)]
    [(tc-results: ts fs os)
     (make-Values
      (for/list ([t (in-list ts)] [f (in-list fs)] [o (in-list os)])
        (-result (abstract-type arg-names keys t)
                 (abstract-filter arg-names keys f)
                 (abstract-object arg-names keys o))))]))

;; Abstract all given id objects into index objects (keys) in
;; the given type
(define/cond-contract (abstract-type ids keys type)
  (-> (listof identifier?) (listof name-ref/c) Type/c Type/c)
  (define (at type) (abstract-type ids keys type))
  (define (af filter) (abstract-filter ids keys filter))
  (define (ao obj) (abstract-object ids keys obj))
  (type-case
   (#:Type at #:Filter af #:Object ao)
   type
   [#:arr
    dom rng rest drest kws
    (let ([at*
           ;; when a new function type is encountered, increase
           ;; the scope count in the keys so that names are
           ;; substituted with the correct level of nesting
           (λ (type)
             (abstract-type ids (map add-scope keys) type))])
      (make-arr (map at dom)
                (at* rng) ; only increase scope in range
                (and rest (at rest))
                (and drest (cons (at (car drest)) (cdr drest)))
                (map at kws)))]))

;; Abstract all given id objects into index objects (keys) in
;; the given object
(define/cond-contract (abstract-object ids keys o)
  (-> (listof identifier?) (listof name-ref/c) Object? Object?)
  (define-lookup: lookup: ids keys)
  (match o
    [(Path: p (lookup: idx)) (make-Path p idx)]
    [_ -empty-obj]))

;; Abstract all given id objects into index objects (keys) in
;; the given filter set
(define/cond-contract (abstract-filter ids keys fs)
  (-> (listof identifier?) (listof name-ref/c) FilterSet/c FilterSet/c)
  (match fs
    [(FilterSet: f+ f-)
     (-FS (abo ids keys f+) (abo ids keys f-))]
    [(NoFilter:) -top-filter]))

(define/cond-contract (abo xs idxs f)
  ((listof identifier?) (listof name-ref/c) Filter/c . -> . Filter/c)
  (define (rec f) (abo xs idxs f))
  (define (sb-t t) t)
  (define-lookup: lookup: xs idxs)
  (filter-case (#:Type sb-t #:Filter rec) f
               [#:TypeFilter
                t p (lookup: idx)
                (-filter t idx p)]
               [#:NotTypeFilter
                t p (lookup: idx)
                (-not-filter t idx p)]))

;; Look up the identifier in a mapping of a list of identifiers
;; to a list of path indices
(define/cond-contract (lookup-index target ids keys)
  (-> identifier? (listof identifier?) (listof (list/c integer? integer?))
      (or/c #f (list/c integer? integer?)))
  (for/first ([id (in-list ids)] [index (in-list keys)]
              #:when (free-identifier=? id target))
    index))

;; Generates a match expander to make `lookup-index` convenient
(define-syntax-rule (define-lookup: lookup: ids keys)
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i)
       (or ;; No need to look up if it's not an identifier,
           ;; since that means it was already abstracted
           ;; into an index.
           (? identifier? (app (λ (id) (lookup-index id ids keys))
                               (? values i)))
           i)])))

(define (tc-results->values tc)
  (match tc
    [(tc-any-results:) ManyUniv]
    [(tc-results: ts) (-values ts)]
    [(tc-results: ts _ _ dty dbound) (-values-dots ts dty dbound)]))

(define/cond-contract (resolve atoms prop)
  ((listof Filter/c)
   Filter/c
   . -> .
   Filter/c)
  (for/fold ([prop prop])
    ([a (in-list atoms)])
    (match prop
      [(AndFilter: ps)
       (let loop ([ps ps] [result null])
         (if (null? ps)
             (apply -and result)
             (let ([p (car ps)])
               (cond [(contradictory? a p) -bot]
                     [(implied-atomic? p a) (loop (cdr ps) result)]
                     [else (loop (cdr ps) (cons p result))]))))]
      [_ prop])))

(define (flatten-props ps)
  (let loop ([ps ps])
    (match ps
      [(list) null]
      [(cons (AndFilter: ps*) ps) (loop (append ps* ps))]
      [(cons p ps) (cons p (loop ps))])))

(define/cond-contract (combine-props new-props old-props flag)
  ((listof Filter/c) (listof Filter/c) (box/c boolean?)
   . -> .
   (values (listof (or/c ImpFilter? OrFilter? AndFilter?)) (listof (or/c TypeFilter? NotTypeFilter?))))
  (define (atomic-prop? p) (or (TypeFilter? p) (NotTypeFilter? p)))
  (define-values (new-atoms new-formulas) (partition atomic-prop? (flatten-props new-props)))
  (let loop ([derived-props null]
             [derived-atoms new-atoms]
             [worklist (append old-props new-formulas)])
    (if (null? worklist)
        (values derived-props derived-atoms)
        (let* ([p (car worklist)]
               [p (resolve derived-atoms p)])
          (match p
            [(AndFilter: ps) (loop derived-props derived-atoms (append ps (cdr worklist)))]
            [(ImpFilter: a c)
             ;(printf "combining ~a with ~a\n" p (append derived-props derived-atoms))
             (if (for/or ([p (in-list (append derived-props derived-atoms))])
                   (implied-atomic? a p))
                 (loop derived-props derived-atoms (cons c (cdr worklist)))
                 (loop (cons p derived-props) derived-atoms (cdr worklist)))]
            [(OrFilter: ps)
             (let ([new-or
                    (let or-loop ([ps ps] [result null])
                      (cond
                        [(null? ps) (apply -or result)]
                        [(for/or ([other-p (in-list (append derived-props derived-atoms))])
                             (complementary? (car ps) other-p))
                         (or-loop (cdr ps) result)]
                        [(for/or ([other-p (in-list derived-atoms)])
                             (implied-atomic? (car ps) other-p))
                         -top]
                        [else (or-loop (cdr ps) (cons (car ps) result))]))])
               (if (OrFilter? new-or)
                   (loop (cons new-or derived-props) derived-atoms (cdr worklist))
                   (loop derived-props derived-atoms (cons new-or (cdr worklist)))))]
            [(TypeFilter: (== (Un) type-equal?) _ _) (set-box! flag #f) (values derived-props derived-atoms)]
            [(TypeFilter: _ _ _) (loop derived-props (cons p derived-atoms) (cdr worklist))]
            [(NotTypeFilter: (== Univ type-equal?) _ _) (set-box! flag #f) (values derived-props derived-atoms)]
            [(NotTypeFilter: _ _ _) (loop derived-props (cons p derived-atoms) (cdr worklist))]
            [(Top:) (loop derived-props derived-atoms (cdr worklist))]
            [(Bot:) (set-box! flag #f) (values derived-props derived-atoms)]
            [_ (loop (cons p derived-props) derived-atoms (cdr worklist))])))))

;; replace-names: (listof (list/c identifier? Object?) tc-results? -> tc-results?
;; For each name replaces all uses of it in res with the corresponding object.
;; This is used so that names do not escape the scope of their definitions
(define (replace-names names+objects res)
  (define (subber proc lst)
    (for/list ([i (in-list lst)])
      (for/fold ([s i]) ([name/object (in-list names+objects)])
        (proc s (first name/object) (second name/object) #t))))
  (match res
    [(tc-any-results:) res]
    [(tc-results: ts fs os)
     (ret (subber subst-type ts) (subber subst-filter-set fs) (subber subst-object os))]
    [(tc-results: ts fs os dt db)
     (ret (subber subst-type ts) (subber subst-filter-set fs) (subber subst-object os) dt db)]))
