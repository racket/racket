#lang typed/racket/base

(require racket/unsafe/ops
         racket/list
         racket/sequence)

(provide samples->immutable-hash samples->hash
         count-samples
         (struct-out sample-bin)
         Real-Bin
         bin-real-samples)

(struct: (A) sample-bin ([min : A] [max : A] [values : (Listof A)]) #:transparent)

(define-type Real-Bin (sample-bin Real))

(: samples->immutable-hash (All (A) ((Sequenceof A) -> (HashTable A Positive-Integer))))
(define (samples->immutable-hash xs)
  (define: h : (HashTable A Positive-Integer)  (make-immutable-hash null))
  (for/fold: ([h : (HashTable A Positive-Integer)  h]) ([x : A  xs])
    (hash-set h x (unsafe-fx+ 1 (hash-ref h x (λ () 0))))))

(: samples->hash (All (A) ((Sequenceof A) -> (HashTable A Positive-Integer))))
(define (samples->hash xs)
  (define: h : (HashTable A Positive-Integer)  (make-hash null))
  (for: ([x : A  xs])
    (hash-set! h x (unsafe-fx+ 1 (hash-ref h x (λ () 0)))))
  h)

(: count-samples (All (A) ((Sequenceof A) -> (Values (Listof A) (Listof Positive-Integer)))))
(define (count-samples xs)
  (define h (samples->hash xs))
  (define xws (hash-map h (λ: ([x : A] [c : Positive-Integer]) (cons x c))))
  (values (map (λ: ([xw : (Pair A Positive-Integer)]) (car xw)) xws)
          (map (λ: ([xw : (Pair A Positive-Integer)]) (cdr xw)) xws)))

(: bin-real-samples ((Sequenceof Real) (Sequenceof Real) -> (Listof Real-Bin)))
(define (bin-real-samples bin-bounds xs)
  (let* ([bin-bounds  (list* -inf.0 +inf.0 (sequence->list bin-bounds))]
         [bin-bounds  (filter (λ: ([x : Real]) (not (eqv? x +nan.0)))
                              (remove-duplicates bin-bounds))]
         [bin-bounds  (sort bin-bounds <)]
         [x-min  (first bin-bounds)]
         [x-max  (last bin-bounds)]
         [xs  (sequence->list xs)]
         [xs  (filter (λ: ([x : Real]) (<= x-min x x-max)) xs)]
         [xs  (sort xs <)])
    (define-values (res rest-xs)
      (for/fold: ([res : (Listof Real-Bin)  empty]
                  [xs : (Listof Real)  xs]
                  ) ([x1  (in-list bin-bounds)]
                     [x2  (in-list (rest bin-bounds))])
        (define-values (lst rest-xs)
          (let: loop : (Values (Listof Real) (Listof Real)) ([lst : (Listof Real)  empty]
                                                             [xs : (Listof Real)  xs])
            (if (and (not (empty? xs)) (<= x1 (first xs) x2))
                (loop (cons (first xs) lst) (rest xs))
                (values lst xs))))
        (cond [(empty? lst)  (values res rest-xs)]
              [else  (values (cons (sample-bin x1 x2 (reverse lst)) res) rest-xs)])))
    (reverse res)))
