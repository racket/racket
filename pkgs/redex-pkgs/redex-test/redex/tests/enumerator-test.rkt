#lang racket

(require rackunit
         data/gvector
         redex/private/enumerator
         (submod redex/private/enumerator test))

;; const/e tests
(let ([e (const/e 17)])
  (test-begin
   (check-eq? (decode e 0) 17)
   (check-exn exn:fail? 
              (λ ()
                 (decode e 1)))
   (check-eq? (encode e 17) 0)
   (check-exn exn:fail?
              (λ ()
                 (encode e 0)))
   (check-bijection? e)))

;; from-list/e tests
(let ([e (from-list/e '(5 4 1 8))])
  (test-begin
   (check-eq? (decode e 0) 5)
   (check-eq? (decode e 3) 8)
   (check-exn exn:fail?
              (λ () (decode e 4)))
   (check-eq? (encode e 5) 0)
   (check-eq? (encode e 8) 3)
   (check-exn exn:fail?
              (λ ()
                 (encode e 17)))
   (check-bijection? e)))

;; map test
(define nats+1 (nat+/e 1))

(test-begin
 (check-equal? (size nats+1) +inf.0)
 (check-equal? (decode nats+1 0) 1)
 (check-equal? (decode nats+1 1) 2)
 (check-bijection? nats+1))
;; encode check
(test-begin
 (check-exn exn:fail?
            (λ ()
               (decode nat/e -1))))

;; ints checks
(test-begin
 (check-eq? (decode int/e 0) 0)         ; 0 -> 0
 (check-eq? (decode int/e 1) 1)         ; 1 -> 1
 (check-eq? (decode int/e 2) -1)        ; 2 -> 1
 (check-eq? (encode int/e 0) 0)
 (check-eq? (encode int/e 1) 1)
 (check-eq? (encode int/e -1) 2)
 (check-bijection? int/e))              ; -1 -> 2, -3 -> 4

;; sum tests
(define evens/e
  (enum +inf.0
        (λ (n)
           (* 2 n))
        (λ (n)
           (if (and (zero? (modulo n 2))
                    (>= n 0))
               (/ n 2)
               (error 'even)))))

(define odds/e
  (enum +inf.0
        (λ (n)
           (+ (* 2 n) 1))
        (λ (n)
           (if (and (not (zero? (modulo n 2)))
                    (>= n 0))
               (/ (- n 1) 2)
               (error 'odd)))))

(test-begin
 (define bool-or-num
   (disj-sum/e (cons bool/e boolean?)
               (cons (from-list/e '(0 1 2 3)) number?)))
 (define bool-or-nat
   (disj-sum/e (cons bool/e boolean?)
               (cons nat/e number?)))
 (define nat-or-bool
   (disj-sum/e (cons nat/e number?)
               (cons bool/e boolean?)))
 (define odd-or-even
   (disj-sum/e (cons evens/e even?)
               (cons odds/e odd?)))


 
 (check-equal? (size bool-or-num) 6)
   
 (check-equal? (decode bool-or-num 0) #t)
 (check-equal? (decode bool-or-num 1) 0)
 (check-equal? (decode bool-or-num 2) #f)
 (check-equal? (decode bool-or-num 3) 1)
 (check-equal? (decode bool-or-num 4) 2)
 (check-equal? (decode bool-or-num 5) 3)
   
 (check-exn exn:fail?
            (λ ()
               (decode bool-or-num 6)))
 (check-bijection? bool-or-num)
   
 (check-equal? (size bool-or-nat)
               +inf.0)
 (check-equal? (decode bool-or-nat 0) #t)
 (check-equal? (decode bool-or-nat 1) 0)
 (check-bijection? bool-or-nat)
   
 (check-equal? (size odd-or-even)
               +inf.0)
 (check-equal? (decode odd-or-even 0) 0)
 (check-equal? (decode odd-or-even 1) 1)
 (check-equal? (decode odd-or-even 2) 2)
 (check-exn exn:fail?
            (λ ()
               (decode odd-or-even -1)))
 (check-equal? (encode odd-or-even 0) 0)   
 (check-equal? (encode odd-or-even 1) 1)
 (check-equal? (encode odd-or-even 2) 2)
 (check-equal? (encode odd-or-even 3) 3)
 (check-bijection? odd-or-even)

 (check-bijection? nat-or-bool)

 (define multi-layered
   (disj-sum/e (cons (take/e string/e 5) string?)
               (cons (from-list/e '(a b c d)) symbol?)
               (cons nat/e number?)
               (cons bool/e boolean?)
               (cons (many/e bool/e) list?)))

 (define (test-multi-layered i x)
   (check-equal? (decode multi-layered i) x))
 (map test-multi-layered
      (for/list ([i (in-range 31)])
        i)
      ;; Please don't reformat this!
      '(""   a 0 #t ()
        "a"  b 1 #f (#t)
        "aa" c 2    (#t #t)
        "b"  d 3    (#f)
        "ba"   4    (#f #t)
               5    (#t #t #t)
               6    (#f #t #t)
               7    (#t #f)
               8    (#f #f)
               9    (#t #f #t)))
 
 (check-bijection? multi-layered))

(test-begin
 (define bool-or-num
   (disj-append/e (cons bool/e boolean?)
                  (cons (from-list/e '(0 1 2 3)) number?)))
 (define bool-or-nat
   (disj-append/e (cons bool/e boolean?)
                  (cons nat/e number?)))
 (check-equal? (size bool-or-num) 6)
   
 (check-equal? (decode bool-or-num 0) #t)
 (check-equal? (decode bool-or-num 1) #f)
 (check-equal? (decode bool-or-num 2) 0)
 (check-equal? (decode bool-or-num 3) 1)
 (check-equal? (decode bool-or-num 4) 2)
 (check-equal? (decode bool-or-num 5) 3)
   
 (check-exn exn:fail?
            (λ ()
               (decode bool-or-num 6)))
 (check-bijection? bool-or-num)
   
 (check-equal? (size bool-or-nat)
               +inf.0)
 (check-equal? (decode bool-or-nat 0) #t)
 (check-equal? (decode bool-or-nat 1) #f)
 (check-equal? (decode bool-or-nat 2) 0)
 (check-bijection? bool-or-nat))

;; cons/e tests
(define bool*bool (cons/e bool/e bool/e))
(define 1*b (cons/e (const/e 1) bool/e))
(define b*1 (cons/e bool/e (const/e 1)))
(define bool*nats (cons/e bool/e nat/e))
(define nats*bool (cons/e nat/e bool/e))
(define nats*nats (cons/e nat/e nat/e))
(define ns-equal? (λ (ns ms)
                     (and (= (car ns)
                             (car ms))
                          (= (cdr ns)
                             (cdr ms)))))
;; Make sure they are layered correctly, but don't care about the
;; exact order

;; prod tests
(test-begin

 (check-equal? (size 1*b) 2)
 (check-equal? (decode 1*b 0) (cons 1 #t))
 (check-equal? (decode 1*b 1) (cons 1 #f))
 (check-bijection? 1*b)
 (check-bijection? b*1)
 (check-equal? (size bool*bool) 4)
 (check-bijection? bool*bool)

 (check-equal? (size bool*nats) +inf.0)
 (check-bijection? bool*nats)

 (check-equal? (size nats*bool) +inf.0)
 (check-bijection? nats*bool)

 (check-bijection? nats*nats)
 (check-bijection? (list/e integer/e integer/e)))


;; fair product tests
(define-simple-check (check-range? e l u approx)
  (let ([actual (for/set ([i (in-range l u)])
                  (decode e i))]
        [expected (list->set approx)])
    (equal? actual expected)))
(test-begin
 (define n*n     (cantor-list/e nat/e nat/e))
 (check-range? n*n  0  1 '((0 0)))
 (check-range? n*n  1  3 '((0 1) (1 0)))
 (check-range? n*n  3  6 '((0 2) (1 1) (2 0)))
 (check-range? n*n  6 10 '((0 3) (1 2) (2 1) (3 0)))
 (check-range? n*n 10 15 '((0 4) (1 3) (2 2) (3 1) (4 0))))
(test-begin
 (define n*n*n   (cantor-list/e nat/e nat/e nat/e))
 (define n*n*n*n (cantor-list/e nat/e nat/e nat/e nat/e))
 

 (check-range? n*n*n  0  1 '((0 0 0)))
 (check-range? n*n*n  1  4 '((0 0 1) (0 1 0) (1 0 0)))
 (check-range? n*n*n  4 10 '((0 0 2) (1 1 0) (0 1 1) (1 0 1) (0 2 0) (2 0 0)))
 (check-range? n*n*n 10 20 '((0 0 3) (0 3 0) (3 0 0)
                             (0 1 2) (1 0 2) (0 2 1) (1 2 0) (2 0 1) (2 1 0)
                             (1 1 1))))

(test-begin
 (check-bijection? (cantor-vec/e string/e nat/e real/e))
 (check-bijection? (cantor-list/e string/e nat/e real/e))
 (check-bijection? (cantor-list/e)))

(test-begin
 (define n*n     (box-list/e nat/e nat/e))
 (check-range? n*n  0  1 '((0 0)))
 (check-range? n*n  1  4 '((0 1) (1 0) (1 1)))
 (check-range? n*n  4  9 '((0 2) (1 2) (2 1) (2 0) (2 2))))
(test-begin
 (define n*n*n   (box-list/e nat/e nat/e nat/e))

 (check-range? n*n*n  0  1 '((0 0 0)))
 (check-range? n*n*n  1  8 '((0 0 1) (0 1 1) (0 1 0)
                             
                             (1 0 0) (1 0 1) (1 1 0) (1 1 1)))
 (check-range? n*n*n  8 27 '((0 0 2) (0 1 2) (0 2 2)
                             (0 2 0) (0 2 1)

                             (1 0 2) (1 1 2) (1 2 2)
                             (1 2 0) (1 2 1)

                             (2 0 0) (2 0 1) (2 0 2)
                             (2 1 0) (2 1 1) (2 1 2)
                             (2 2 0) (2 2 1) (2 2 2))))

(test-begin
 (check-bijection? (box-vec/e string/e nat/e real/e))
 (check-bijection? (box-list/e string/e nat/e real/e))
 (check-bijection? (box-list/e)))

;; helper
(test-begin
 (check-equal? (list->inc-set '(2 0 1 2)) '(2 3 5 8))
 (check-equal? (inc-set->list '(2 3 5 8)) '(2 0 1 2)))

(define (below/e n)
  (take/e nat/e n))

;; mixed finite/infinite list/e tests
(test-begin

 (check-equal?
  (to-list (list/e (below/e 3) (below/e 3) (below/e 3)))
  (to-list (take/e (list/e nat/e nat/e nat/e) 27)))

 (define n*2 (list/e nat/e (below/e 2)))
 (check-range? n*2 0 1 '((0 0)))
 (check-range? n*2 1 4 '((0 1) (1 0) (1 1)))
 (check-range? n*2 4 6 '((2 0) (2 1)))
 (check-range? n*2 6 8 '((3 0) (3 1)))

 (define n*1*2 (list/e nat/e (below/e 1) (below/e 2)))
 (check-range? n*1*2 0 1 '((0 0 0)))
 (check-range? n*1*2 1 4 '((0 0 1) (1 0 0) (1 0 1)))
 (check-range? n*1*2 4 6 '((2 0 0) (2 0 1)))
 (check-range? n*1*2 6 8 '((3 0 0) (3 0 1)))

 (define n*2*4 (list/e nat/e (below/e 2) (below/e 4)))
 (check-range? n*2*4 0 1 '((0 0 0)))
 (check-range? n*2*4 1 8 '((0 0 1) (0 1 1) (0 1 0)
                           (1 0 0) (1 0 1) (1 1 0) (1 1 1)))
 (check-range? n*2*4 8 18 ;; (8 previous . + . (2 magnitude of exhausted enums
               ;;             . * . (9 3^(number left) . - . 4 2^(number left)))

               '((0 0 2) (0 1 2)
                 (1 0 2) (1 1 2)
                 (2 0 0) (2 1 0)
                 (2 0 1) (2 1 1)
                 (2 0 2) (2 1 2)))
 (check-range? n*2*4 18 32 ;; 18 + (2 * (4^2 - 3^2))
               '((0 0 3) (0 1 3)
                 (1 0 3) (1 1 3)
                 (2 0 3) (2 1 3)
                 (3 0 0) (3 1 0)
                 (3 0 1) (3 1 1)
                 (3 0 2) (3 1 2)
                 (3 0 3) (3 1 3)))
 (check-range? n*2*4 32 40
               '((4 0 0) (4 0 1) (4 0 2) (4 0 3)
                 (4 1 0) (4 1 1) (4 1 2) (4 1 3)))
 (check-range? n*2*4 40 48
               '((5 0 0) (5 0 1) (5 0 2) (5 0 3)
                 (5 1 0) (5 1 1) (5 1 2) (5 1 3)))
 
 (check-bijection? (list/e bool/e (cons/e bool/e bool/e) (fin/e 'foo 'bar 'baz)))
 (check-bijection? (list/e nat/e string/e (many/e bool/e)))
 (check-bijection? (list/e bool/e nat/e int/e string/e (cons/e bool/e bool/e)))
 )

;; multi-arg map/e test
(define sums/e
  (map/e
   cons
   (λ (x-y)
      (values (car x-y) (cdr x-y)))
   (from-list/e '(1 2))
   (from-list/e '(3 4))))

(test-begin
 (check-bijection? sums/e))

;; dep/e tests
(define (up-to n)
  (below/e (add1 n)))
(define 3-up
  (dep/e
   (from-list/e '(0 1 2))
   up-to))

(define from-3
  (dep/e
   (from-list/e '(0 1 2))
   nat+/e))

(define nats-to
  (dep/e nat/e up-to))

(define nats-up
  (dep/e nat/e nat+/e))

(test-begin
 (check-equal? (size 3-up) 6)
 (check-equal? (decode 3-up 0) (cons 0 0))
 (check-equal? (decode 3-up 1) (cons 1 0))
 (check-equal? (decode 3-up 2) (cons 1 1))
 (check-equal? (decode 3-up 3) (cons 2 0))
 (check-equal? (decode 3-up 4) (cons 2 1))
 (check-equal? (decode 3-up 5) (cons 2 2))
 (check-bijection? 3-up)

 (check-equal? (size from-3) +inf.0)
 (check-equal? (decode from-3 0) (cons 0 0))
 (check-equal? (decode from-3 1) (cons 1 1))
 (check-equal? (decode from-3 2) (cons 2 2))
 (check-equal? (decode from-3 3) (cons 0 1))
 (check-equal? (decode from-3 4) (cons 1 2))
 (check-equal? (decode from-3 5) (cons 2 3))
 (check-equal? (decode from-3 6) (cons 0 2))
 (check-bijection? from-3)

 (check-equal? (size nats-to) +inf.0)
 (check-equal? (decode nats-to 0) (cons 0 0))
 (check-equal? (decode nats-to 1) (cons 1 0))
 (check-equal? (decode nats-to 2) (cons 1 1))
 (check-equal? (decode nats-to 3) (cons 2 0))
 (check-equal? (decode nats-to 4) (cons 2 1))
 (check-equal? (decode nats-to 5) (cons 2 2))
 (check-equal? (decode nats-to 6) (cons 3 0))
 (check-bijection? nats-to)

 (check-equal? (size nats-up) +inf.0)
 (check-equal? (decode nats-up 0) (cons 0 0))
 (check-equal? (decode nats-up 1) (cons 0 1))
 (check-equal? (decode nats-up 2) (cons 1 1))
 (check-equal? (decode nats-up 3) (cons 0 2))
 (check-equal? (decode nats-up 4) (cons 1 2))
 (check-equal? (decode nats-up 5) (cons 2 2))
 (check-equal? (decode nats-up 6) (cons 0 3))
 (check-equal? (decode nats-up 7) (cons 1 3))

 (check-bijection? nats-up))

;; find-size tests
(check-equal? (find-size (gvector) 5) #f)
(check-equal? (find-size (gvector 5) 4) 0)
(check-equal? (find-size (gvector 1 5 7) 0) 0)
(check-equal? (find-size (gvector 1 5 7) 1) 1)
(check-equal? (find-size (gvector 1 5 7) 4) 1)
(check-equal? (find-size (gvector 1 5 7) 5) 2)
(check-equal? (find-size (gvector 1 5 7) 6) 2)
(check-equal? (find-size (gvector 1 5 7) 7) #f) 

;; depend/e tests
;; same as dep unless the right side is finite
(define 3-up-2
  (dep/e
   (from-list/e '(0 1 2))
   up-to))

(define nats-to-2
  (dep/e nat/e up-to))

(test-begin
 (check-equal? (size 3-up-2) 6)
 (check-equal? (decode 3-up-2 0) (cons 0 0))
 (check-equal? (decode 3-up-2 1) (cons 1 0))
 (check-equal? (decode 3-up-2 2) (cons 1 1))
 (check-equal? (decode 3-up-2 3) (cons 2 0))
 (check-equal? (decode 3-up-2 4) (cons 2 1))
 (check-equal? (decode 3-up-2 5) (cons 2 2))
 
 (check-equal? (encode 3-up-2 (cons 0 0)) 0)
 (check-equal? (encode 3-up-2 (cons 1 0)) 1)
 (check-equal? (encode 3-up-2 (cons 1 1)) 2)
 (check-equal? (encode 3-up-2 (cons 2 0)) 3)

 (check-equal? (size nats-to-2) +inf.0)
 (check-equal? (encode nats-to-2 (cons 0 0)) 0)
 (check-equal? (encode nats-to-2 (cons 1 0)) 1)
 (check-equal? (encode nats-to-2 (cons 1 1)) 2)
 (check-equal? (encode nats-to-2 (cons 2 0)) 3)
 (check-equal? (encode nats-to-2 (cons 2 1)) 4)
 (check-equal? (encode nats-to-2 (cons 2 2)) 5)
 (check-equal? (encode nats-to-2 (cons 3 0)) 6)

 (check-equal? (decode nats-to-2 0) (cons 0 0))
 (check-equal? (decode nats-to-2 1) (cons 1 0))
 (check-equal? (decode nats-to-2 2) (cons 1 1))
 (check-equal? (decode nats-to-2 3) (cons 2 0))
 (check-equal? (decode nats-to-2 4) (cons 2 1))
 (check-equal? (decode nats-to-2 5) (cons 2 2))
 (check-equal? (decode nats-to-2 6) (cons 3 0)))

;; take/e test
(define to-2 (up-to 2))
(test-begin
 (check-equal? (size to-2) 3)
 (check-equal? (decode to-2 0) 0)
 (check-equal? (decode to-2 1) 1)
 (check-equal? (decode to-2 2) 2)
 (check-bijection? to-2))

;; slic/e test
(test-begin
 (check-equal? (to-list (slice/e nat/e 3 5)) '(3 4))
 (check-bijection? (slice/e nat/e 3 5)))

;; to-list test
(test-begin
 (check-equal? (to-list (up-to 3))
               '(0 1 2 3)))

;; except/e test

(define not-3 (except/e nat/e 3))
(test-begin
 (check-equal? (decode not-3 0) 0)
 (check-equal? (decode not-3 3) 4)
 (check-bijection? not-3))

;; fold-enum tests
(define complicated
  (fold-enum
   (λ (excepts n)
      (apply except/e (up-to n) excepts))
   '(2 4 6)))
(test-begin
 (check-bijection? complicated))

;; many/e tests
(define natss
  (many/e nat/e))
(test-begin
 (check-bijection? natss))

(define emptys/e
  (many/e empty/e))
(test-begin
 (check-equal? (decode emptys/e 0) '())
 (check-bijection? emptys/e))
