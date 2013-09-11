#lang racket/base

(require "../utils/utils.rkt"
         racket/match racket/list
         (except-in (types abbrev union utils filter-ops)
                    -> ->* one-of/c)
         (rep type-rep filter-rep object-rep rep-utils)
         (contract-req))

(provide abstract-results
         combine-props
         tc-results->values)

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
        (make-Result (abstract-type arg-names keys t)
                     (abstract-filter arg-names keys f)
                     (abstract-object arg-names keys o)))
      dty dbound)]
    [(tc-results: ts fs os)
     (make-Values
      (for/list ([t (in-list ts)] [f (in-list fs)] [o (in-list os)])
        (make-Result (abstract-type arg-names keys t)
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
             (abstract-type ids (add-scope keys) type))])
      (make-arr (map at dom)
                (at* rng) ; only increase scope in range
                (and rest (at rest))
                (and drest (cons (at (car drest)) (cdr drest)))
                (map at kws)))]))

;; add-scope : Listof<name-ref/c> -> Listof<name-ref/c>
;; Add a scope to the index object
(define (add-scope keys)
  (for/list ([depth+arg keys])
    (match-define (list depth arg) depth+arg)
    (list (+ 1 depth) arg)))

;; Abstract all given id objects into index objects (keys) in
;; the given object
(define/cond-contract (abstract-object ids keys o)
  (-> (listof identifier?) (listof name-ref/c) Object? Object?)
  (define (lookup y)
    (for/first ([x (in-list ids)] [i (in-list keys)] #:when (free-identifier=? x y)) i))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (app lookup (? values i))]))
  (match o
    [(Path: p (lookup: idx)) (make-Path p idx)]
    [_ -no-obj]))

;; Abstract all given id objects into index objects (keys) in
;; the given filter set
(define/cond-contract (abstract-filter ids keys fs)
  (-> (listof identifier?) (listof name-ref/c) FilterSet/c FilterSet/c)
  (match fs
    [(FilterSet: f+ f-)
     (-FS (abo ids keys f+) (abo ids keys f-))]
    [(NoFilter:) -no-filter]))

(define/cond-contract (abo xs idxs f)
  ((listof identifier?) (listof name-ref/c) Filter/c . -> . Filter/c)
  (define/cond-contract (lookup y)
    (identifier? . -> . (or/c #f (list/c integer? integer?)))
    (for/first ([x (in-list xs)] [i (in-list idxs)] #:when (free-identifier=? x y)) i))
  (define-match-expander lookup:
    (syntax-rules ()
      [(_ i) (or (? identifier? (app lookup (? values i)))
                 i)]))
  (define (rec f) (abo xs idxs f))
  (define (sb-t t) t)
  (filter-case (#:Type sb-t #:Filter rec) f
               [#:TypeFilter
                t p (lookup: idx)
                (-filter t idx p)]
               [#:NotTypeFilter
                t p (lookup: idx)
                (-not-filter t idx p)]))

(define (merge-filter-sets fs)
  (match fs
    [(list (FilterSet: f+ f-) ...)
     (-FS (make-AndFilter f+) (make-AndFilter f-))]))

(define (tc-results->values tc)
  (match tc
    [(tc-any-results:) ManyUniv]
    [(tc-results: ts) (-values ts)]))

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
               (cond [(opposite? a p) -bot]
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
                             (opposite? (car ps) other-p))
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

