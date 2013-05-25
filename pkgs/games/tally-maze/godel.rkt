#lang racket/base

#|

Originally from Jay McCarthy's gb library.

|#

(require racket/match
         racket/contract
         racket/function
         racket/list)


(provide k*k-bind/s
         cons/s list/s
         nat-range/s wrap/s enum/s unit/s
         decode encode spec-k)

(module+ test
  (require rackunit)
  (define N 10))

;; The core: Pairing functions
(define (core-nat-cons x y)
  (arithmetic-shift (bitwise-ior 1 (arithmetic-shift y 1))
                    x))

(define (core-nat-hd n)
  (unless (> n 0)
    (error 'core-nat-hd "Cannot take the head of 0"))
  (if (= 1 (bitwise-and n 1))
    0
    (add1 (core-nat-hd (arithmetic-shift n -1)))))

(define (core-nat-tl n)
  (arithmetic-shift n (* -1 (add1 (core-nat-hd n)))))

(define (nat-cons x y)
  (sub1 (core-nat-cons x y)))
(define (nat-hd z)
  (core-nat-hd (add1 z)))
(define (nat-tl z)
  (core-nat-tl (add1 z)))

(define (pair hd-k tl-k hd tl)
  (match* (hd-k tl-k)
    [(+inf.0 +inf.0)
     (nat-cons hd tl)]            
    [(+inf.0 tl-k)
     (+ (* hd tl-k) tl)]    
    [(hd-k +inf.0)
     (+ hd (* tl hd-k))]    
    [(hd-k tl-k)
     (+ hd (* tl hd-k))]))
(define (pair-hd hd-k tl-k n)
  (match* (hd-k tl-k)
    [(+inf.0 +inf.0)
     (nat-hd n)]    
    [(+inf.0 tl-k)
     (quotient n tl-k)]
    [(hd-k +inf.0)
     (remainder n hd-k)]
    [(hd-k tl-k)
     (remainder n hd-k)]))
(define (pair-tl hd-k tl-k n)
  (match* (hd-k tl-k)
    [(+inf.0 +inf.0)
     (nat-tl n)]    
    [(+inf.0 tl-k)
     (remainder n tl-k)]
    [(hd-k +inf.0)
     (quotient n hd-k)]
    [(hd-k tl-k)
     (quotient n hd-k)]))

(module+ test
  (for ([i (in-range N)])
    (define fst (random (* N N)))
    (define snd (random (* N N)))
    (define n (nat-cons fst snd))
    (test-equal? (format "~a,~a" fst snd) (nat-hd n) fst)
    (test-equal? (format "~a,~a" fst snd) (nat-tl n) snd)))

;; Encoding
(struct spec (k in out) #:transparent)

(define (encode spec v)
  (define n ((spec-out spec) v))
  (define k (spec-k spec))
  (unless (< n k)
    (error 
     'encode
     "spec(~e) returned encoding[~e] outside range[~e] for value[~e]"
     spec n k v))
  n)
(define (decode spec n)
  (define k (spec-k spec))
  (unless (< n k)
    (error
     'decode
     "spec(~e) received encoding[~e] outside range[~e]"
     spec n k))  
  ((spec-in spec) n))
(module+ test
  (define-syntax-rule (test-en/de s-e v-e)
    (let ()
      (define n 's-e)
      (define s s-e)
      (define v v-e)
      (test-equal? (format "s=~a v=~a" n v)
                   (decode s (encode s v))
                   v)))
  (define-syntax-rule (test-spec s-e)
    (let ()
      (define n 's-e)
      (define s s-e)
      (for ([i (in-range (min N (spec-k s)))])
        (define v (decode s i))
        (test-equal? (format "n=~a i=~a v=~a" n i v)
                     (encode s v) i))))
  (define-syntax-rule (test-spec-ex s-e v-e n-e)
    (let ()
      (define v v-e)
      (define n n-e)
      (define s s-e)
      (test-equal? (format "encode ~a ~a = ~a" 's-e v n) (encode s v) n)
      (test-equal? (format "decode ~a ~a = ~a" 's-e n v) (decode s n) v)))
  (define-syntax-rule (test-spec-exs s-e [v n] ...)
    (let ()
      (test-spec-ex s-e v n)
      ...)))

;; Specs
(define null/s
  (spec 0 error error))
(define (unit/s v)
  (spec 1 (λ (n) v) (λ (v) 0)))
(module+ test
  (define empty/s (unit/s empty))
  (test-spec empty/s)
  (for ([i (in-range N)])
    (test-en/de empty/s empty)))

(define nat/s
  (spec +inf.0 identity identity))
(module+ test
  (test-spec nat/s)
  (for ([i (in-range N)])
    (test-en/de nat/s (random (* N N)))))

(define (nat-range/s k)
  (spec k identity identity))
(module+ test
  (test-spec (nat-range/s N))
  (for ([i (in-range N)])
    (test-en/de (nat-range/s N) i)))

(define (cons/s hd/s tl/s)
  (match-define (spec hd-k _ _) hd/s)
  (match-define (spec tl-k _ _) tl/s)
  (spec (* hd-k tl-k)
        (λ (n)          
          (cons (decode hd/s (pair-hd hd-k tl-k n))
                (decode tl/s (pair-tl hd-k tl-k n))))
        (λ (v)
          (pair hd-k tl-k
                (encode hd/s (car v))
                (encode tl/s (cdr v))))))
(module+ test
  (define 2nats/s (cons/s nat/s nat/s))
  (test-spec 2nats/s)
  (for ([i (in-range N)])
    (test-en/de 2nats/s
                (cons (random (* N N))
                      (random (* N N))))))

(define (cantor-cons/s hd/s tl/s)
  (match-define (spec hd-k _ _) hd/s)
  (match-define (spec tl-k _ _) tl/s)
  (unless (= +inf.0 hd-k)
    (raise-argument-error 'cantor-cons/s
                          "an infinite /s"
                          0
                          (list hd/s tl/s)))
  (unless (= +inf.0 tl-k)
    (raise-argument-error 'cantor-cons/s
                          "an infinite /s"
                          1
                          (list hd/s tl/s)))
  (spec (* hd-k tl-k)
        (λ (z)
          (define q (- (integer-sqrt (+ (* 8 z) 1)) 1))
          (define w (if (even? q)
                        (/ q 2)
                        (/ (- q 1) 2)))
          (define t (/ (+ (* w w) w) 2))
          (define y (- z t))
          (define x (- w y))
          (cons (decode hd/s x) (decode tl/s y)))
        (λ (v)
          (define k1 (encode hd/s (car v)))
          (define k2 (encode tl/s (cdr v)))
          (+ (* 1/2 (+ k1 k2) (+ k1 k2 1)) k2))))

(module+ test
  (define cantor-2nats/s (cantor-cons/s nat/s nat/s))
  (test-spec cantor-2nats/s)
  (for ([i (in-range N)])
    (test-en/de cantor-2nats/s
                (cons (random (* N N))
                      (random (* N N)))))
  (let ()
    ;; big-n has the digits you get by doing to the cantor
    ;; pairing function using normal sqrt (via floats)
    ;; and then finding the first number that goes wrong
    ;; via repeated squaring (due to the imprecision of floats)
    ;; and then concatenating that number's digits with itself
    (define big-n
      340282366920938463463374607431768211456340282366920938463463374607431768211456)
    (test-en/de cantor-2nats/s (cons big-n big-n))))

(define (or/s left? left/s right? right/s)
  (match-define (spec left-k _ _) left/s)
  (match-define (spec right-k _ _) right/s)
  (match* (left-k right-k)
    [(+inf.0 +inf.0)
     (spec +inf.0
           (λ (n)
             (match (pair-hd 2 +inf.0 n)
               [0
                (decode left/s (pair-tl 2 +inf.0 n))]
               [1
                (decode right/s (pair-tl 2 +inf.0 n))]))
           (λ (v)
             (match v
               [(? left?)
                (pair 2 +inf.0 0 (encode left/s v))]
               [(? right?)
                (pair 2 +inf.0 1 (encode right/s v))])))]
    [(+inf.0 right-k)
     (or/s right? right/s left? left/s)]
    [(left-k right-k)
     (spec (+ left-k right-k)
           (λ (n)
             (if (< n left-k)
               (decode left/s n)
               (decode right/s (- n left-k))))
           (λ (v)
             (match v
               [(? left?)
                (encode left/s v)]
               [(? right?)
                (+ (encode right/s v) left-k)])))]))

(module+ test
  (define int/s
    (or/s exact-nonnegative-integer? nat/s
          negative? (wrap/s (wrap/s nat/s
                                    (λ (n) (* -1 n))
                                    (λ (n) (* -1 n)))
                            (λ (n) (- n 1))
                            (λ (n) (+ n 1)))))
  (test-spec int/s)
  (for ([i (in-range N)])
    (test-en/de int/s
                (if (zero? (random 2))
                  (add1 (random (* N N)))
                  (* -1 (add1 (random (* N N)))))))

  (define weird-nat/s
    (or/s (λ (i) (<= 0 i 3)) (enum/s '(0 1 2 3))
          (λ (i) (< 3 i)) (wrap/s nat/s
                                  (λ (n) (+ n 4))
                                  (λ (n) (- n 4)))))
  (test-spec weird-nat/s)
  (for ([i (in-range N)])
    (test-en/de weird-nat/s
                (random (* N N))))

  (define weird-nat/s-2
    (or/s (λ (i) (< 3 i)) (wrap/s nat/s
                                  (λ (n) (+ n 4))
                                  (λ (n) (- n 4)))
          (λ (i) (<= 0 i 3)) (enum/s '(0 1 2 3))))
  (test-spec weird-nat/s-2)
  (for ([i (in-range N)])
    (test-en/de weird-nat/s-2
                (random (* N N))))

  (define weird-nat/s-3
    (or/s (λ (i) (<= 0 i 3)) (enum/s '(0 1 2 3))
          (λ (i) (<= 4 i 6)) (enum/s '(4 5 6))))
  (test-spec weird-nat/s-3)
  (for ([i (in-range N)])
    (test-en/de weird-nat/s-3
                (random 7))))

(define (enum/s elems)
  (define elem->i
    (for/hash ([e (in-list elems)]
               [i (in-naturals)])
      (values e i)))
  (spec (length elems)
        (λ (n) (list-ref elems n))
        (λ (v) (hash-ref elem->i v))))
(define bool/s
  (enum/s (list #f #t)))

(module+ test
  (define (test-enum/s os)
    (define bool/s (enum/s os))

    (test-spec bool/s)
    (for ([x (in-list os)])
      (test-en/de bool/s x))

    (define b*b/s (cons/s bool/s bool/s))
    (test-spec b*b/s)
    (for* ([x (in-list os)]
           [y (in-list os)])
      (test-en/de b*b/s (cons x y)))

    (define n*b/s (cons/s nat/s bool/s))
    (test-spec n*b/s)
    (for* ([x (in-range N)]
           [y (in-list os)])
      (test-en/de n*b/s (cons (random (* N N)) y)))

    (define b*n/s (cons/s bool/s nat/s))
    (test-spec b*n/s)
    (for* ([y (in-range N)]
           [x (in-list os)])
      (test-en/de b*n/s (cons x (random (* N N))))))

  (test-enum/s '(#f #t))
  (test-enum/s '(0 1 2)))

(define (flist/s k elem/s)
  (match k
    [0
     (unit/s empty)]
    [n
     (cons/s elem/s (flist/s (sub1 k) elem/s))]))
(module+ test
  (test-spec-exs (flist/s 0 (enum/s '(0 1 2)))
                 [empty 0])
  (test-spec-exs (flist/s 1 (enum/s '(0 1 2)))
                 [(cons 0 empty) 0]
                 [(cons 1 empty) 1]
                 [(cons 2 empty) 2])
  (test-spec-exs (flist/s 2 (enum/s '(0 1 2)))
                 [(cons 0 (cons 0 empty)) 0]
                 [(cons 1 (cons 0 empty)) 1]
                 [(cons 2 (cons 0 empty)) 2]

                 [(cons 0 (cons 1 empty)) 3]
                 [(cons 1 (cons 1 empty)) 4]
                 [(cons 2 (cons 1 empty)) 5]

                 [(cons 0 (cons 2 empty)) 6]
                 [(cons 1 (cons 2 empty)) 7]
                 [(cons 2 (cons 2 empty)) 8])

  (define 3nats/s (flist/s 3 nat/s))
  (test-spec 3nats/s)
  (for ([i (in-range N)])
    (test-en/de 3nats/s
                (cons (random (* N N))
                      (cons (random (* N N))
                            (cons (random (* N N))
                                  empty))))))

(define (wrap/s inner/s wrap-in wrap-out)
  (match-define (spec inner-k _ _) inner/s)
  (spec inner-k
        (λ (n) (wrap-in (decode inner/s n)))
        (λ (v) (encode inner/s (wrap-out v)))))

(define (hetero-vector/s vector-of-spec)
  (define list-spec
    (foldr (λ (elem/s s) (cons/s elem/s s))
           (unit/s empty)
           (vector->list vector-of-spec)))
  (wrap/s list-spec list->vector vector->list))
(module+ test
  (define weird-vector/s
    (hetero-vector/s
     (vector (nat-range/s 2)
             (nat-range/s 3)
             (nat-range/s 4))))
  (test-spec weird-vector/s)
  (for ([i (in-range N)])
    (test-en/de weird-vector/s
                (vector (random 2)
                        (random 3)
                        (random 4)))))

(define (bind/s fst/s fst->rst/s
                #:count 
                [given-count #f]
                #:fst->rst/s-k
                [given-fst->rst/s-k
                 #f]
                #:rst-always-inf?
                [rst-always-inf? #f])
  (define fst->rst/s-k
    (or given-fst->rst/s-k
        (λ (i)
          (define fst (decode fst/s i))
          (define rst/s (fst->rst/s fst))
          (spec-k rst/s))))
  (define fst-k (spec-k fst/s))
  (cond 
    [rst-always-inf?
     (spec (* fst-k +inf.0)
           (λ (n)
             (define fst-n
               (pair-hd fst-k +inf.0 n))
             (define fst 
               (decode fst/s fst-n))
             (define rst/s (fst->rst/s fst))
             (define rst-n
               (pair-tl fst-k +inf.0 n))
             (define rst
               (decode rst/s rst-n))
             (cons fst rst))
           (λ (v)
             (match-define (cons fst rst) v)
             (define rst/s (fst->rst/s fst))             
             (pair fst-k +inf.0
                   (encode fst/s fst)
                   (encode rst/s rst))))]
    [else
     (define (check-rst-k! rst-k)
       (when (= +inf.0 rst-k)
         (error 'bind/s 
                "rst/s not always inf, but ever inf not supported")))
     (define count
       (cond
         [given-count
          given-count]
         [(= +inf.0 fst-k)
          ;; XXX This is not actually correct if (sum (forall (f) (spec-k
          ;; (fst->rst/s f)))) is finite, such as when the rst is always
          ;; empty except a finite number of times, etc.
          +inf.0]
         [else
          (for/fold ([total 0])
              ([i (in-range fst-k)])
            (define rst-k
              (fst->rst/s-k i))
            (check-rst-k! rst-k)
            (+ total rst-k))]))

     (define (bind-in i n)
       (define fst (decode fst/s i))
       (define rst/s (fst->rst/s fst))
       (define rst-k (spec-k rst/s))
       (check-rst-k! rst-k)
       (cond
         [(>= n rst-k)
          (bind-in (add1 i) (- n rst-k))]
         [else
          (cons fst (decode rst/s n))]))
     (define (bind-out-sum i)
       (cond
         [(< i 0)
          0]
         [else
          (define fst (decode fst/s i))
          (define rst/s (fst->rst/s fst))
          (define rst-k (spec-k rst/s))
          (check-rst-k! rst-k)
          (+ rst-k (bind-out-sum (sub1 i)))]))

     (spec count
           (λ (n) (bind-in 0 n))
           (λ (v)
             (define fst (car v))
             (define fst-n (encode fst/s fst))
             (define rst/s (fst->rst/s fst))
             (check-rst-k! (spec-k rst/s))
             (+ (bind-out-sum (sub1 fst-n))
                (encode rst/s (cdr v)))))]))

(define (k*k-bind/s fst/s fst->rst/s
                    #:count [count #f]
                    #:rst-k [given-rst-k #f])
  (bind/s fst/s fst->rst/s
          #:count count
          #:fst->rst/s-k
          (and given-rst-k
               (λ (i) given-rst-k))))

(module+ test  
  (let ()
    (define outer-s
      (nat-range/s 3))
    (define ex-s
      (k*k-bind/s outer-s
                  (λ (i)
                    (define inner-s
                      (nat-range/s (+ i 1)))                    
                    inner-s)))
    (check-equal?
     (for/list ([i (in-range (spec-k ex-s))])
       (decode ex-s i))
     '((0 . 0)
       (1 . 0)
       (1 . 1)
       (2 . 0)       
       (2 . 1)
       (2 . 2))))

  (define 3+less-than-three/s
    (k*k-bind/s (enum/s '(0 1 2 3)) (λ (i) (nat-range/s (add1 i)))))
  (test-spec
   3+less-than-three/s)
  (test-spec-exs
   3+less-than-three/s
   [(cons 0 0) 0]
   [(cons 1 0) 1]
   [(cons 1 1) 2]
   [(cons 2 0) 3]
   [(cons 2 1) 4]
   [(cons 2 2) 5]
   [(cons 3 0) 6]
   [(cons 3 1) 7]
   [(cons 3 2) 8]
   [(cons 3 3) 9]))

(define (k*k-bind2/s fst/s fst->rst/s
                     #:count [count #f]
                     #:rst-k [given-rst-k #f])
  ;; XXX check
  (match-define (spec fst-k _ _) fst/s)
  
  (define size-table (make-hash))
  (define total-size 0)
  (define subs
    (for/list ([i (in-range fst-k)])
      (define fst (decode fst/s i))
      (define sub (fst->rst/s fst))
      (define size (spec-k sub))
      (hash-set! size-table fst total-size)
      (set! total-size (+ total-size size))
      sub))
  (spec total-size
        (λ (n)
          (let loop ([subs subs]
                     [n n]
                     [i 0])
            (define sub (car subs))
            (define sub-k (spec-k sub))
            (cond
              [(< n sub-k)
               (define fst (decode fst/s i))
               (define rst/s (fst->rst/s fst))
               (match-define (spec rst-k _ _) rst/s)
               (cons fst (decode rst/s n))]
              [else
               (loop (cdr subs) (- n (spec-k sub)) (+ i 1))])))
        (λ (v)
          (match-define (cons fst rst) v)
          (define rst/s (fst->rst/s fst))
          (match-define (spec rst-k _ _) rst/s)
          (+ (hash-ref size-table fst) (encode rst/s rst)))))

(module+ test
  (define 3+less-than-three2/s
    (k*k-bind2/s (enum/s '(0 1 2 3)) (λ (i) (nat-range/s (add1 i)))))
  (test-spec
   3+less-than-three2/s)
  (test-spec-exs
   3+less-than-three2/s
   [(cons 0 0) 0]
   [(cons 1 0) 1]
   [(cons 1 1) 2]
   [(cons 2 0) 3]
   [(cons 2 1) 4]
   [(cons 2 2) 5]
   [(cons 3 0) 6]
   [(cons 3 1) 7]
   [(cons 3 2) 8]
   [(cons 3 3) 9]))

(define (k*inf-bind/s fst/s fst->rst/s)
  (bind/s fst/s fst->rst/s
          #:rst-always-inf? #t))
(module+ test
  (define 3+more-than-three/s
    (k*inf-bind/s (enum/s '(0 1 2 3))
                  (λ (i)
                    (wrap/s nat/s
                            (λ (out) (+ out i))
                            (λ (in) (- in i))))))
  (test-spec
   3+more-than-three/s)
  (test-spec-exs
   3+more-than-three/s
   [(cons 0 0) 0]
   [(cons 1 1) 1]
   [(cons 2 2) 2]
   [(cons 2 3) 6]
   [(cons 3 3) 3]))

;; XXX Add an optional function arg that tells you how many elements
;; there are for each n
(define (inf*k-bind/s fst/s fst->rst/s)
  (bind/s fst/s fst->rst/s))

(module+ test
  (define nat+less-than-n/s
    (inf*k-bind/s nat/s (λ (i) (nat-range/s (add1 i)))))
  (test-spec-exs
   nat+less-than-n/s
   [(cons 0 0) 0]
   [(cons 1 0) 1]
   [(cons 1 1) 2]
   [(cons 2 0) 3]
   [(cons 2 1) 4]
   [(cons 2 2) 5]
   [(cons 3 0) 6]
   [(cons 3 1) 7]
   [(cons 3 2) 8]
   [(cons 3 3) 9]))

(define (inf*inf-bind/s fst/s fst->rst/s)
  (bind/s fst/s fst->rst/s
          #:rst-always-inf? #t))
(module+ test
  (define nat+greater-than-n/s
    (inf*inf-bind/s
     nat/s (λ (i)
             (wrap/s nat/s
                     (λ (out) (+ out i))
                     (λ (in) (- in i))))))
  (test-spec nat+greater-than-n/s))

;; XXX
;; (define (union/s pred->spec)
;;   (define pred/s (enum/s (hash-keys pred->spec)))
;;   (wrap/s
;;    (bind/s pred/s (λ (pred) ((hash-ref pred->spec pred))))
;;    (λ (de) (cdr de))
;;    (λ (en) (cons (for/or ([pred (in-hash-keys pred->spec)])
;;                    (and (pred en)
;;                         pred))
;;                  en))))

;; (define (union-list/s elem/s)
;;   (define this/s
;;     (union/s (hash empty? (λ () (unit/s empty))
;;                    cons? (λ () (cons/s elem/s this/s)))))
;;   this/s)

(define (flist-prep/s inner/s)
  (wrap/s
   inner/s
   (λ (v) (cdr v))
   (λ (l) (cons (length l) l))))

(define nat-greater-than-1/s
  (wrap/s nat/s
          (λ (out) (+ out 1))
          (λ (in) (- in 1))))

(define (nelist/s elem/s)
  (define f-elem-list/s
    (λ (len) (flist/s len elem/s)))
  (cond
    [(= +inf.0 (spec-k elem/s))
     (flist-prep/s
      (inf*inf-bind/s
       nat-greater-than-1/s
       f-elem-list/s))]
    [else
     (flist-prep/s
      (inf*k-bind/s
       nat-greater-than-1/s f-elem-list/s))]))

(define (bind-list/s elem/s)
  (define f-elem-list/s
    (λ (len) (flist/s len elem/s)))
  (cond
    [(= +inf.0 (spec-k elem/s))
     (or/s empty?
           (unit/s empty)
           cons?
           (flist-prep/s
            (inf*inf-bind/s
             nat-greater-than-1/s
             f-elem-list/s)))]
    [else
     (flist-prep/s
      (inf*k-bind/s
       nat/s f-elem-list/s))]))

(define list/s bind-list/s)

(module+ test
  (define 012-list/s (list/s (enum/s '(0 1 2))))
  (test-spec 012-list/s)
  (for ([i (in-range N)])
    (test-en/de 012-list/s
                (build-list (random (* N N))
                            (λ (_) (random 3)))))

  (define nat-list/s (list/s nat/s))
  (test-spec nat-list/s)
  (for ([i (in-range N)])
    (test-en/de nat-list/s
                (build-list (random (* N N))
                            (λ (_) (random (* N N)))))))


;; XXX
(define (spec/c result/c)
  spec?)

;; XXX
(provide (all-defined-out))

(module+ main
  (let ([np/s (cons/s (nat-range/s 4) (nat-range/s 4))])
    (define (number->bits n)
      (reverse
       (let loop ([n n])
         (cond
           [(zero? n) '()]
           [(odd? n) (cons 1 (loop (/ (- n 1) 2)))]
           [(even? n) (cons 0 (loop (/ n 2)))]))))
    (define (f p)
      (+ (length (number->bits (car p)))
         (length (number->bits (cdr p)))))
    (for/list ([i (in-range 16)])
      (f (decode np/s i)))))
