#lang typed/racket/base

(require racket/list
         racket/sequence
         "../../base.rkt"
         "../unsafe.rkt"
         "statistics-utils.rkt")

(provide samples->hash
         count-samples
         (struct-out sample-bin)
         sample-bin-compact
         sample-bin-total
         bin-samples
         bin-samples/key)

;; ===================================================================================================
;; Hashing

(: unweighted-samples->hash (All (A) ((Sequenceof A) -> (HashTable A Positive-Integer))))
(define (unweighted-samples->hash xs)
  (define: h : (HashTable A Positive-Integer)  (make-hash))
  (for: ([x : A  xs])
    (hash-set! h x (unsafe-fx+ 1 (hash-ref h x (λ () 0)))))
  h)

(: weighted-samples->hash (All (A) ((Sequenceof A) (Sequenceof Real)
                                                   -> (HashTable A Nonnegative-Real))))
(define (weighted-samples->hash xs ws)
  (let-values ([(xs ws)  (sequences->weighted-samples 'samples->hash xs ws)])
    (define: h : (HashTable A Nonnegative-Real)  (make-hash))
    (for: ([x : A  xs] [w : Nonnegative-Real  ws])
      (hash-set! h x (+ w (hash-ref h x (λ () 0)))))
    h))

(: samples->hash
   (All (A) (case-> ((Sequenceof A) -> (HashTable A Positive-Integer))
                    ((Sequenceof A) (U #f (Sequenceof Real)) -> (HashTable A Nonnegative-Real)))))
(define samples->hash
  (case-lambda
    [(xs)  (unweighted-samples->hash xs)]
    [(xs ws)  (cond [ws    (weighted-samples->hash xs ws)]
                    [else  (weighted-samples->hash xs (sequence-map (λ (_) 1) xs))])]))

;; ===================================================================================================
;; Counting

(: count-unweighted-samples
   (All (A) ((Sequenceof A) -> (Values (Listof A) (Listof Positive-Integer)))))
(define (count-unweighted-samples xs)
  (let ([xs  (sequence->list xs)])
    (define h (samples->hash xs))
    (let ([xs  (remove-duplicates xs)])
      (values xs (map (λ: ([x : A]) (hash-ref h x)) xs)))))

(: count-weighted-samples
   (All (A) ((Sequenceof A) (Sequenceof Real) -> (Values (Listof A) (Listof Nonnegative-Real)))))
(define (count-weighted-samples xs ws)
  (let-values ([(xs ws)  (sequences->weighted-samples 'count-samples xs ws)])
    (define h (weighted-samples->hash xs ws))
    (let ([xs  (remove-duplicates xs)])
      (values xs (map (λ: ([x : A]) (hash-ref h x)) xs)))))

(: count-samples
   (All (A) (case-> ((Sequenceof A) -> (Values (Listof A) (Listof Positive-Integer)))
                    ((Sequenceof A) (U #f (Sequenceof Real))
                                    -> (Values (Listof A) (Listof Nonnegative-Real))))))
(define count-samples
  (case-lambda
    [(xs)  (count-unweighted-samples xs)]
    [(xs ws)  (if ws (count-weighted-samples xs ws) (count-unweighted-samples xs))]))

;; ===================================================================================================
;; Bins

(struct: (A B) sample-bin
  ([min : B] [max : B] [values : (Listof A)] [weights : (U #f (Listof Nonnegative-Real))])
  #:transparent)

(: sample-bin-compact (All (A B) ((sample-bin A B) -> (sample-bin A B))))
(define (sample-bin-compact bin)
  (let-values ([(xs ws)  (count-samples (sample-bin-values bin) (sample-bin-weights bin))])
    (sample-bin (sample-bin-min bin) (sample-bin-max bin) xs ws)))

(: sample-bin-total (All (A B) ((sample-bin A B) -> Nonnegative-Real)))
(define (sample-bin-total bin)
  (define ws (sample-bin-weights bin))
  (if ws (assert (sum ws) nonnegative?) (length (sample-bin-values bin))))

;; ===================================================================================================
;; Binning

(: list-split-after (All (A) ((Listof A) (A -> Any) -> (Values (Listof A) (Listof A)))))
(define (list-split-after xs pred?)
  (let: loop : (Values (Listof A) (Listof A)) ([xs : (Listof A)  xs]
                                               [ys : (Listof A)  empty])
    (cond [(empty? xs)  (values (reverse ys) xs)]
          [else
           (define x (first xs))
           (cond [(pred? x)  (loop (rest xs) (cons x ys))]
                 [else  (values (reverse ys) xs)])])))

(: bin-unweighted-samples/key
   (All (A B) ((Sequenceof B) (B B -> Any) (A -> B) (Sequenceof A) -> (Listof (sample-bin A B)))))
(define (bin-unweighted-samples/key bnds lte? key xs)
  (: lt? (B B -> Boolean))
  (define (lt? x1 x2)
    (and (lte? x1 x2) (not (lte? x2 x1))))
  (let* ([bnds  (sort (sequence->list bnds) lt?)]
         [xs    (sequence->list xs)]
         [xks   (map (λ: ([x : A]) (cons x (key x))) xs)]
         [xks   (sort xks (λ: ([xk1 : (Pair A B)] [xk2 : (Pair A B)])
                            (lt? (cdr xk1) (cdr xk2))))])
    (cond
      [(empty? bnds)
       (cond [(empty? xks)  empty]
             [else  (define min (cdr (first xks)))
                    (define max (cdr (last xks)))
                    (list (sample-bin min max (map (inst car A B) xks) #f))])]
      [else
       (let: loop : (Listof (sample-bin A B)) ([min : (U #f B)  #f]
                                               [max : B  (first bnds)]
                                               [bnds : (Listof B)  (rest bnds)]
                                               [xks : (Listof (Pair A B))  xks]
                                               [bins : (Listof (sample-bin A B))  empty])
         (let-values ([(yks xks)  (list-split-after xks (λ: ([xk : (Pair A B)])
                                                          (lte? (cdr xk) max)))])
           (define maybe-bin
             (cond [min  (list (sample-bin min max (map (inst car A B) yks) #f))]
                   [(empty? yks)  empty]
                   [else  (list (sample-bin (cdr (first yks)) max (map (inst car A B) yks) #f))]))
           (cond [(empty? bnds)
                  (cond [(empty? xks)  (reverse (append maybe-bin bins))]
                        [else
                         (define bin2
                           (sample-bin max (cdr (last xks)) (map (inst car A B) xks) #f))
                         (reverse (append (cons bin2 maybe-bin) bins))])]
                 [else
                  (loop max (first bnds) (rest bnds) xks (append maybe-bin bins))])))])))

(: bin-weighted-samples/key
   (All (A B) ((Sequenceof B) (B B -> Any) (A -> B) (Sequenceof A) (Sequenceof Real)
                              -> (Listof (sample-bin A B)))))
(define (bin-weighted-samples/key bnds lte? key xs ws)
  (let-values ([(xs ws)  (sequences->weighted-samples 'bin-samples/key xs ws)])
    (define xws (map (inst cons A Nonnegative-Real) xs ws))
    (define xw-key (λ: ([xw : (Pair A Nonnegative-Real)]) (key (car xw))))
    (map (λ: ([bin : (sample-bin (Pair A Nonnegative-Real) B)])
           (define xws (sample-bin-values bin))
           (sample-bin (sample-bin-min bin)
                       (sample-bin-max bin)
                       (map (inst car A Nonnegative-Real) xws)
                       (map (inst cdr A Nonnegative-Real) xws)))
         (bin-unweighted-samples/key bnds lte? xw-key xws))))

(: bin-samples/key
   (All (A B)
        (case-> ((Sequenceof B) (B B -> Any) (A -> B) (Sequenceof A) -> (Listof (sample-bin A B)))
                ((Sequenceof B) (B B -> Any) (A -> B) (Sequenceof A) (U #f (Sequenceof Real))
                                -> (Listof (sample-bin A B))))))
(define (bin-samples/key bnds lte? key xs [ws #f])
  (cond [ws    (bin-weighted-samples/key bnds lte? key xs ws)]
        [else  (bin-unweighted-samples/key bnds lte? key xs)]))

(: bin-samples
   (All (A) (case-> ((Sequenceof A) (A A -> Any) (Sequenceof A) -> (Listof (sample-bin A A)))
                    ((Sequenceof A) (A A -> Any) (Sequenceof A) (Sequenceof Real)
                                    -> (Listof (sample-bin A A))))))
(define (bin-samples bnds lte? xs [ws #f])
  (bin-samples/key bnds lte? (λ: ([x : A]) x) xs ws))
