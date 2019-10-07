
(define/who current-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)
                  (lambda (v)
                    (check who pseudo-random-generator? v)
                    v)
                  'current-pseudo-random-generator))

(define/who random
  (case-lambda
   [() (pseudo-random-generator-next! (current-pseudo-random-generator))]
   [(n)
    (cond
     [(pseudo-random-generator? n)
      (pseudo-random-generator-next! n)]
     [else
      (check who
             :test (and (exact-integer? n)
                        (<= 1 n 4294967087))
             :contract "(or/c (integer-in 1 4294967087) pseudo-random-generator?)"
             n)
      (pseudo-random-generator-next! (current-pseudo-random-generator) n)])]
   [(n prg)
    (check who
           :test (and (exact-integer? n)
                      (<= 1 n 4294967087))
           :contract "(or/c (integer-in 1 4294967087) pseudo-random-generator?)"
           n)
    (check who pseudo-random-generator? prg)
    (pseudo-random-generator-next! prg n)]))

(define/who (random-seed k)
  (check who
         :test (and (exact-nonnegative-integer? k)
                    (<= k (sub1 (expt 2 31))))
         :contract "(integer-in 0 (sub1 (expt 2 31)))"
         k)
  (pseudo-random-generator-seed! (current-pseudo-random-generator) k))

(define (pseudo-random-generator-vector? v)
  (let ([in-range?
         (lambda (i mx)
           (let ([x (vector-ref v i)])
             (and (exact-nonnegative-integer? x)
                  (<= x mx))))]
        [nonzero?
         (lambda (i)
           (not (zero? (vector-ref v i))))])
    (and (vector? v)
         (= 6 (vector-length v))
         (in-range? 0 4294967086)
         (in-range? 1 4294967086)
         (in-range? 2 4294967086)
         (in-range? 3 4294944442)
         (in-range? 4 4294944442)
         (in-range? 5 4294944442)
         (or (nonzero? 0) (nonzero? 1) (nonzero? 2))
         (or (nonzero? 3) (nonzero? 4) (nonzero? 5)))))
