#lang racket/base

;; Extra functions that can't be easily categorized

(require racket/sequence racket/list racket/math racket/flonum racket/match)

(provide (all-defined-out))

(define (in-cycle* s)
  (define n (sequence-length s))
  (if (zero? n) empty-sequence (in-cycle s)))

(define (sequence-take seq start end)
  (for/list ([e  (sequence-tail seq start)] 
             [_  (in-range (- end start))])
    e))

(define (sequence-head-vector name xs n)
  (define vec (for/vector ([x  xs] [i  (in-range n)]) x))
  (unless (= n (vector-length vec))
    (raise-argument-error name (format "sequence of length >= ~a" n) xs))
  vec)

(define (sequence->listof-vector name vs n)
  (map (λ (v) (sequence-head-vector name v n)) (sequence->list vs)))

(define (list-index v lst [equal? equal?])
  (for/first ([e  (in-list lst)] [i  (in-naturals)] #:when (equal? e v))
    i))

(define (list-duplicate-index lst)
  (let loop ([lst lst] [j 0])
    (cond [(empty? lst)  #f]
          [else
           (define fst (first lst))
           (define idx
             (for/first ([e  (in-list (rest lst))] [i  (in-naturals)] #:when (equal? e fst))
               (+ i j 1)))
           (if idx idx (loop (rest lst) (+ j 1)))])))

(define (cumulative-sum xs)
  (reverse (foldl (λ (x xs) (cons (+ x (first xs)) xs)) '(0) xs)))

(define (assoc-cons hash key new-value)
  (let loop ([hash  hash])
    (cond [(empty? hash)  (list (cons key (list new-value)))]
          [else
           (define entry (first hash))
           (cond [(equal? (car entry) key)  (cons (cons key (cons new-value (cdr entry)))
                                                  (rest hash))]
                 [else  (cons (first hash) (loop (rest hash)))])])))

(define (vector-find-index pred? xs [start 0] [end (vector-length xs)])
  (for/first ([i  (in-range start end)] #:when (pred? (vector-ref xs i)))
    i))

(define ((sorted-apply sort f) lst)
  (define h
    (let ([sorted-lst  (sort lst)])
      (make-hash (map cons sorted-lst (f sorted-lst)))))
  (map (λ (e) (hash-ref h e)) lst))

(define (transpose xss)
  (cond [(andmap empty? xss)  empty]
        [else  (cons (map (λ (xs) (if (empty? xs) #f (first xs))) xss)
                     (transpose (map (λ (xs) (if (empty? xs) empty (rest xs))) xss)))]))

(define (group-neighbors lst equiv?)
  (reverse
   (map reverse
        (cond
          [(empty? lst)  empty]
          [else
           (for/fold ([res  (list (list (first lst)))]) ([e  (in-list (rest lst))])
             (cond
               [(andmap (λ (e2) (equiv? e e2)) (first res))  (cons (cons e (first res)) (rest res))]
               [else  (list* (list e) res)]))]))))

(define (bin-samples bin-bounds xs)
  (let* ([bin-bounds  (filter (λ (x) (not (eqv? x +nan.0))) (remove-duplicates bin-bounds))]
         [bin-bounds  (sort bin-bounds <)]
         [x-min  (first bin-bounds)]
         [x-max  (last bin-bounds)]
         [xs  (filter (λ (x) (<= x-min x x-max)) xs)]
         [xs  (sort xs <)])
    (define-values (res rest-xs)
      (for/fold ([res empty] [xs xs]) ([x1  (in-list bin-bounds)]
                                       [x2  (in-list (rest bin-bounds))])
        (define-values (lst rest-xs)
          (let loop ([lst empty] [xs xs])
            (if (and (not (empty? xs)) (<= x1 (first xs) x2))
                (loop (cons (first xs) lst) (rest xs))
                (values lst xs))))
        (values (cons (reverse lst) res)
                rest-xs)))
    (reverse res)))

(define-syntax-rule (let-map (id ...) fun body0 body ...)
  (let ([id  (fun id)] ...)
    body0 body ...))
