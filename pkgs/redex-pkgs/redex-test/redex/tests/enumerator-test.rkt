#lang racket

(require rackunit
         data/gvector
         redex/private/enumerator
         (submod redex/private/enumerator test))

;; basic enums
(define bools/e
  (from-list/e (list #t #f)))

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
(define nats+1 (nats+/e 1))

(test-begin
 (check-equal? (size nats+1) +inf.f)
 (check-equal? (decode nats+1 0) 1)
 (check-equal? (decode nats+1 1) 2)
 (check-bijection? nats+1))
;; encode check
(test-begin
 (check-exn exn:fail?
            (λ ()
               (decode nats/e -1))))

;; ints checks
(test-begin
 (check-eq? (decode ints/e 0) 0)         ; 0 -> 0
 (check-eq? (decode ints/e 1) 1)         ; 1 -> 1
 (check-eq? (decode ints/e 2) -1)        ; 2 -> 1
 (check-eq? (encode ints/e 0) 0)
 (check-eq? (encode ints/e 1) 1)
 (check-eq? (encode ints/e -1) 2)
 (check-bijection? ints/e))              ; -1 -> 2, -3 -> 4

;; sum tests

(define evens/e
  (enum +inf.f
        (λ (n)
           (* 2 n))
        (λ (n)
           (if (and (zero? (modulo n 2))
                    (>= n 0))
               (/ n 2)
               (error 'even)))))

(define odds/e
  (enum +inf.f
        (λ (n)
           (+ (* 2 n) 1))
        (λ (n)
           (if (and (not (zero? (modulo n 2)))
                    (>= n 0))
               (/ (- n 1) 2)
               (error 'odd)))))

(test-begin
 (let ([bool-or-num (sum/e bools/e
                           (from-list/e '(0 1 2 3)))]
       [bool-or-nat (sum/e bools/e
                           nats/e)]
       [nat-or-bool (sum/e nats/e
                           bools/e)]
       [odd-or-even (sum/e evens/e
                           odds/e)])
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
                 +inf.f)
   (check-equal? (decode bool-or-nat 0) #t)
   (check-equal? (decode bool-or-nat 1) 0)
   (check-bijection? bool-or-nat)
   
   (check-equal? (size odd-or-even)
                 +inf.f)
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
   ;; Known bug, won't fix because I'm getting rid of sum/e anyway
   ;; (check-bijection? nat-or-bool)
   ))

(test-begin
 (define bool-or-num
   (disj-sum/e #:alternate? #t
               (cons bools/e boolean?)
               (cons (from-list/e '(0 1 2 3)) number?)))
 (define bool-or-nat
   (disj-sum/e #:alternate? #t
               (cons bools/e boolean?)
               (cons nats/e number?)))
 (define nat-or-bool
   (disj-sum/e #:alternate? #t
               (cons nats/e number?)
               (cons bools/e boolean?)))
 (define odd-or-even
   (disj-sum/e #:alternate? #t
               (cons evens/e even?)
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
               +inf.f)
 (check-equal? (decode bool-or-nat 0) #t)
 (check-equal? (decode bool-or-nat 1) 0)
 (check-bijection? bool-or-nat)
   
 (check-equal? (size odd-or-even)
               +inf.f)
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

 (check-bijection? nat-or-bool))

(test-begin
 (define bool-or-num
   (disj-sum/e #:append? #t
               (cons bools/e boolean?)
               (cons (from-list/e '(0 1 2 3)) number?)))
 (define bool-or-nat
   (disj-sum/e #:append? #t
               (cons bools/e boolean?)
               (cons nats/e number?)))
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
               +inf.f)
 (check-equal? (decode bool-or-nat 0) #t)
 (check-equal? (decode bool-or-nat 1) #f)
 (check-equal? (decode bool-or-nat 2) 0)
 (check-bijection? bool-or-nat))

;; cons/e tests
(define bool*bool (cons/e bools/e bools/e))
(define 1*b (cons/e (const/e 1) bools/e))
(define b*1 (cons/e bools/e (const/e 1)))
(define bool*nats (cons/e bools/e nats/e))
(define nats*bool (cons/e nats/e bools/e))
(define nats*nats (cons/e nats/e nats/e))
(define ns-equal? (λ (ns ms)
                     (and (= (car ns)
                             (car ms))
                          (= (cdr ns)
                             (cdr ms)))))

;; prod tests
(test-begin

 (check-equal? (size 1*b) 2)
 (check-equal? (decode 1*b 0) (cons 1 #t))
 (check-equal? (decode 1*b 1) (cons 1 #f))
 (check-bijection? 1*b)
 (check-bijection? b*1)
 (check-equal? (size bool*bool) 4)
 (check-equal? (decode bool*bool 0)
               (cons #t #t))
 (check-equal? (decode bool*bool 1)
               (cons #t #f))
 (check-equal? (decode bool*bool 2)
               (cons #f #t))
 (check-equal? (decode bool*bool 3)
               (cons #f #f))
 (check-bijection? bool*bool)

 (check-equal? (size bool*nats) +inf.f)
 (check-equal? (decode bool*nats 0)
               (cons #t 0))
 (check-equal? (decode bool*nats 1)
               (cons #f 0))
 (check-equal? (decode bool*nats 2)
               (cons #t 1))
 (check-equal? (decode bool*nats 3)
               (cons #f 1))
 (check-bijection? bool*nats)

 (check-equal? (size nats*bool) +inf.f)
 (check-equal? (decode nats*bool 0)
               (cons 0 #t))
 (check-equal? (decode nats*bool 1)
               (cons 0 #f))
 (check-equal? (decode nats*bool 2)
               (cons 1 #t))
 (check-equal? (decode nats*bool 3)
               (cons 1 #f))
 (check-bijection? nats*bool)

 (check-equal? (size nats*nats) +inf.f)
 (check ns-equal?
        (decode nats*nats 0)
        (cons 0 0))
 (check ns-equal?
        (decode nats*nats 1)
        (cons 0 1))
 (check ns-equal?
        (decode nats*nats 2)
        (cons 1 0))
 (check ns-equal?
        (decode nats*nats 3)
        (cons 0 2))
 (check ns-equal?
        (decode nats*nats 4)
        (cons 1 1))
 (check-bijection? nats*nats))

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
  (take/e nats/e (+ n 1)))

(define 3-up
  (dep/e
   (from-list/e '(0 1 2))
   up-to))

(define from-3
  (dep/e
   (from-list/e '(0 1 2))
   nats+/e))

(define nats-to
  (dep/e nats/e up-to))

(define nats-up
  (dep/e nats/e nats+/e))

(test-begin
 (check-equal? (size 3-up) 6)
 (check-equal? (decode 3-up 0) (cons 0 0))
 (check-equal? (decode 3-up 1) (cons 1 0))
 (check-equal? (decode 3-up 2) (cons 1 1))
 (check-equal? (decode 3-up 3) (cons 2 0))
 (check-equal? (decode 3-up 4) (cons 2 1))
 (check-equal? (decode 3-up 5) (cons 2 2))
 (check-bijection? 3-up)

 (check-equal? (size from-3) +inf.f)
 (check-equal? (decode from-3 0) (cons 0 0))
 (check-equal? (decode from-3 1) (cons 1 1))
 (check-equal? (decode from-3 2) (cons 2 2))
 (check-equal? (decode from-3 3) (cons 0 1))
 (check-equal? (decode from-3 4) (cons 1 2))
 (check-equal? (decode from-3 5) (cons 2 3))
 (check-equal? (decode from-3 6) (cons 0 2))
 (check-bijection? from-3)

 (check-equal? (size nats-to) +inf.f)
 (check-equal? (decode nats-to 0) (cons 0 0))
 (check-equal? (decode nats-to 1) (cons 1 0))
 (check-equal? (decode nats-to 2) (cons 1 1))
 (check-equal? (decode nats-to 3) (cons 2 0))
 (check-equal? (decode nats-to 4) (cons 2 1))
 (check-equal? (decode nats-to 5) (cons 2 2))
 (check-equal? (decode nats-to 6) (cons 3 0))
 (check-bijection? nats-to)

 (check-equal? (size nats-up) +inf.f)
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
  (dep/e nats/e up-to))

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

 (check-equal? (size nats-to-2) +inf.f)
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

;; to-list test
(test-begin
 (check-equal? (to-list (up-to 3))
               '(0 1 2 3)))

;; except/e test

(define not-3 (except/e nats/e 3))
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
(check-bijection? complicated)

;; many/e tests
(define natss
  (many/e nats/e))
(check-bijection? natss)
