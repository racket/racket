#lang racket/unit

(require "../utils/utils.rkt"
         (types abbrev union subtype)
         (prefix-in c: (contract-req))
         (utils tc-utils)
         unstable/sequence unstable/hash
         "fail.rkt" "signatures.rkt" "constraint-structs.rkt"
         racket/match
         racket/list)

(import restrict^ dmap^)
(export constraints^)

;; Widest constraint possible
(define no-constraint (make-c (Un) Univ))

;; An empty constraint map.
(define empty-cset
  (make-cset (list (cons (hash)
                         (make-dmap (make-immutable-hash null))))))

;; Smart constructors for csets
(define/cond-contract (make-cset* maps)
  ((c:listof (c:cons/c (c:hash/c symbol? c?) dmap?)) . c:-> . (c:or/c #f cset?))
  (and (not (null? maps)) (make-cset (remove-duplicates maps))))


;; Return a cset with only the constaints on var and additionally
;; add the constraint that var ~ dcon.
(define (reduce/dotted-var cs var dcon)
  (make-cset
    (map/cset
      (match-lambda**
        [(map (dmap dm))
          (% cons (hash)
                  (% make-dmap (% hash var
                                       (if (hash-has-key? dm var)
                                           (dcon-meet dcon (hash-ref dm var))
                                           dcon))))])
      cs)))


;; Return a cset with only the constaints on var, and additionally
;; add the constraints S <: var <: T.
(define (reduce/var cs var S T)
  (define constraint (make-c S T))
  (make-cset
    (map/cset
      (match-lambda**
        [(map (dmap dm))
          (% cons
             (% hash var
                    (if (hash-has-key? map var)
                        (% c-meet (hash-ref map var) constraint)
                        constraint))
             (make-dmap (hash)))])
      cs)))

;; meet: Type Type -> Type
;; intersect the given types. produces a lower bound on both, but
;; perhaps not the GLB
(define (meet S T)
  (let ([s* (restrict S T)])
    (if (and (subtype s* S)
             (subtype s* T))
        s*
        (Un))))

;; join: Type Type -> Type
;; union the given types
(define (join T U) (Un T U))


;; meet of two variable constraints
;; never fails
;; if var is provided, the resulting constraint uses it, otherwise it
;; uses the variable from `c1` (which must be the same as the one from
;; `c2`)
(define (c-meet c1 c2)
  (match*/early (c1 c2)
    [((struct c (S T)) (struct c (S* T*)))
     (let ([S (join S S*)] [T (meet T T*)])
       (and (subtype S T)
            (make-c S T)))]))

;; compute the meet of two constraint sets
;; returns #f for failure
(define cset-meet
  (case-lambda
    [() empty-cset]
    [(x) x]
    [(x y)
     (match* (x y)
      [((struct cset (maps1)) (struct cset (maps2)))
       (define maps (for*/list ([(map1 dmap1) (in-pairs (in-list maps1))]
                                [(map2 dmap2) (in-pairs (in-list maps2))]
                                [v (in-value (% cons
                                                (hash-union/fail map1 map2 #:combine c-meet)
                                                (dmap-meet dmap1 dmap2)))]
                                #:when v)
                      v))
       (make-cset* maps)])]
    [(x . ys)
     (for/fold ([x x]) ([y (in-list ys)])
       (% cset-meet x y))]))

;; combines a list of csets using cset-meet individually
;; returns #f for failure
(define (cset-meet* args)
  (apply cset-meet args))

;; produces a cset of all of the maps in all of the given csets
;; returns #f for failure
(define (cset-join l)
  (let ([mapss (map cset-maps l)])
    (make-cset* (apply append mapss))))

;; (CMap DMap -> (U A #f)) CSet -> (Listof A)
;; Map a function over a constraint set
(define (map/cset f cset)
  (for*/list ([(cmap dmap) (in-pairs (in-list (cset-maps cset)))]
              [v (in-value (f cmap dmap))]
              #:when v)
    v))
