#lang racket/base
(require racket/contract/base
         data/enumerate
         math/base         
         math/distributions
         math/number-theory
         racket/generator)

;; pick-an-index : ([0,1] -> Nat) ∩ (-> Nat)
(define (random-natural-w/o-limit [prob-of-zero 0.01])
  (max (random-natural/no-mean prob-of-zero)
       (random-natural/no-mean prob-of-zero)
       (random-natural/no-mean prob-of-zero)))

;; random-natural/no-mean : [0,1] -> Nat
(define (random-natural/no-mean prob-of-zero)
  (define x (sample (geometric-dist prob-of-zero)))
  (define m1 (expt 2 (exact-floor x)))
  (define m0 (quotient m1 2))
  (random-integer m0 m1))

(define (random-index e)
  (define k (size e))
  (if (infinite? k)
      (random-natural-w/o-limit)
      (random-natural k)))

(define (BPP-digits N)
  (let loop ([8Pi -8])
    (define 8i
      (+ 8 8Pi))

    (define (E k)
      (/ 1 (+ 8i k)))

    (define pi_i
      (* N
         (+ (* +4 (E 1))
            (* -2 (E 4))
            (* -1 (E 5))
            (* -1 (E 6)))))

    (for ([c (in-string (number->string pi_i))])
      (unless (eq? #\/ c)
        (yield (- (char->integer c) (char->integer #\0)))))

    (loop 8i)))

(define (bits-of k)
  (/ (log k) (log 2)))

;; XXX just subtract k if greater than k and then push the digit to
;; left and go on
(define (10-sequence->K-sequence k seq)
  (cond
   [(< k 10)
    (10-sequence->sub10-sequence k seq)]
   [(= k 10)
    seq]
   [else
    (10-sequence->sup10-sequence k seq)]))

(define (10-sequence->sub10-sequence k seq)
  (in-generator
   (for ([d seq])
     (when (< d k)
       (yield d)))))

(define (10-sequence->sup10-sequence k seq)
  (in-generator
   (let loop ()
     (define d
       (for/sum ([i (in-range (ceiling (/ (log k) (log 10))))]
                 [sub-d seq])
         (* sub-d (expt 10 i))))
     (yield (modulo d k))
     (loop))))

(module+ main
  (define HOW-MANY 5000)

  (define (test-seq K seq)
    (define d->i (make-hasheq))
    (for ([i (in-range HOW-MANY)]
          [d seq])
      (hash-update! d->i d add1 0))

    (define total
      (for/fold ([cnt 0]) ([i (in-range K)])
        (define i-cnt (hash-ref d->i i 0))
        (printf "\t~a => ~a" i i-cnt)
        (when (and (= 4 (modulo i 5)) (not (= i (sub1 K)))) (newline))
        (+ cnt i-cnt)))
    (newline)

    (unless (= HOW-MANY total)
      (error 'digits "Missed some: ~a" total)))

  (define (test-digits N)
    (printf "BPP ~a\n" N)
    (test-seq 10 (in-generator (BPP-digits N))))

  (test-digits 1)
  (test-digits 9)

  (define (test-tetris K N)
    (printf "BPP ~a -> ~a\n" N K)
    (test-seq K (10-sequence->K-sequence K (in-generator (BPP-digits N)))))

  (test-tetris 7 1)
  (test-tetris 7 2)
  (test-tetris 15 1)
  (test-tetris 15 2)

  (test-tetris 100 2))

(define (infinite-sequence/e inner/e)
  (define seed/e nat/e)
  (define K (size inner/e))
  (define (seed->seq N)
    (define K-seq
      (10-sequence->K-sequence K (in-generator (BPP-digits (+ 1 N)))))
    (in-generator
     (for ([k K-seq])
       (yield (from-nat inner/e k)))))
  (map/e seed->seq error seed/e))

(module+ test
  (define sevens/e (infinite-sequence/e (below/e 7)))
  (define s (from-nat sevens/e 42))
  (for ([e s]
        [i (in-range 10)])
    (printf "~a = ~a\n" i e)))

(define PERMS (make-hasheq))
(define (permutations-of-n/e n)
  (hash-ref!
   PERMS n
   (λ ()
     (cond
      [(zero? n)
       (const/e '())]
      [else
       (dep2/e
        (factorial n)
        (below/e n)
        (λ (v)
          (map/e
           (λ (l)
             (for/list ([i (in-list l)])
               (if (= i v)
                   (sub1 n)
                   i)))
           (λ (l)
             (for/list ([i (in-list l)])
               (if (= i (sub1 n))
                   v
                   i)))
           (permutations-of-n/e (sub1 n)))))]))))

(module+ test
  (define perms/e (permutations-of-n/e 3))
  (for ([i (in-range (size perms/e))])
    (define l (from-nat perms/e i))
    (printf "~a = ~a = ~a\n" i
            l
            (to-nat perms/e l))))

(define (permutations/e l)
  (define idx->e (list->vector l))
  (define e->idx
    (for/hash ([e (in-list l)]
               [i (in-naturals)])
      (values e i)))
  (map/e
   (λ (l)
     (for/list ([idx (in-list l)])
       (vector-ref idx->e idx)))
   (λ (l)
     (for/list ([e (in-list l)])
       (hash-ref e->idx e)))
   (permutations-of-n/e (vector-length idx->e))))

(module+ test
  (define abcds/e (permutations/e '(a b c d)))
  (for ([i (in-range 10)])
    (define l (from-nat abcds/e i))
    (printf "~a = ~a = ~a\n" i l (to-nat abcds/e l))))

(provide
 (contract-out
  [random-index
   (-> enum? exact-nonnegative-integer?)]
  [infinite-sequence/e
   (-> enum? enum?)]
  [permutations/e
   (-> list? enum?)]
  [permutations-of-n/e
   (-> exact-nonnegative-integer? enum?)]))
