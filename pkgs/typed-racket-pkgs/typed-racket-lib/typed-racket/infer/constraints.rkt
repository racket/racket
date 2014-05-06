#lang racket/unit

(require "../utils/utils.rkt"
	 (types abbrev union subtype)
	 (utils tc-utils)
	 unstable/sequence unstable/hash
         "fail.rkt" "signatures.rkt" "constraint-structs.rkt"
         racket/match
         racket/list)

(import restrict^ dmap^)
(export constraints^)

;; Widest constraint possible
(define (no-constraint v)
  (make-c (Un) v Univ))

;; Create an empty constraint map from a set of type variables X and
;; index variables Y.  For now, we add the widest constraints for
;; variables in X to the cmap and create an empty dmap.
(define (empty-cset X Y)
  (make-cset (list (cons (for/hash ([x (in-list X)])
                           (values x (no-constraint x)))
                         (make-dmap (make-immutable-hash null))))))


(define (insert cs var S T)
  (match cs
    [(struct cset (maps))
     (make-cset (for/list ([(map dmap) (in-pairs maps)])
                  (cons (hash-set map var (make-c S var T))
                        dmap)))]))

;; a stupid impl
(define (meet S T)
  (let ([s* (restrict S T)])
    (if (and (subtype s* S)
             (subtype s* T))
        s*
        (Un))))

(define (join T U) (Un T U))


(define (c-meet c1 c2 [var #f])
  (match*/early (c1 c2)
    [((struct c (S X T)) (struct c (S* X* T*)))
     (unless (or var (eq? X X*))
       (int-err "Non-matching vars in c-meet: ~a ~a" X X*))
     (let ([S (join S S*)] [T (meet T T*)])
       (and (subtype S T)  
            (make-c S (or var X) T)))]))
  
(define (cset-meet x y)
  (match* (x y)
   [((struct cset (maps1)) (struct cset (maps2)))
    (define maps (for*/list
                     ([(map1 dmap1) (in-pairs (remove-duplicates maps1))]
                      [(map2 dmap2) (in-pairs (remove-duplicates maps2))]
                      [v (in-value (% cons
                                      (hash-union/fail map1 map2 #:combine c-meet)
                                      (dmap-meet dmap1 dmap2)))]
                      #:when v)
                   v))
    (cond [(null? maps)
           #f]
          [else (make-cset maps)])]
   [(_ _) (int-err "Got non-cset: ~a ~a" x y)]))

(define (cset-meet* args)
  (for/fold ([c (make-cset (list (cons
                                  (make-immutable-hash null)
                                  (make-dmap (make-immutable-hash null)))))])
    ([a (in-list args)]
     #:break (not c))
    (cset-meet a c)))

(define (cset-combine l)
  (let ([mapss (map cset-maps l)])
    (make-cset (apply append mapss))))

