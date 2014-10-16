#lang lazy

(require tests/eli-tester)

;; tests for lazy language constructs

(define (basic-tests)
  (test
   (! ((car (list if)) (< 1 2) 3 (error "poof"))) => 3
   (! ((car (list or)) 3 (error "poof"))) => 3
   (! ((car (list and)) (< 2 1) (error "poof"))) => #f
   (!! (let ([x 0]) (set! x 1) (list x))) => '(1) ; implicit begin forces
   (! (let ([x 0]) (when (zero? x) (error "poof")) 1)) =error> "poof"
   (! (let ([x 0]) (when (zero? x) (set! x (add1 x)) (set! x (add1 x))) x))
   => 2
   (! (let ([x 1]) (unless (zero? x) (set! x (add1 x)) (set! x (add1 x))) x))
   => 3
   (! (let ([x 0]) (cond [(zero? x) (set! x (add1 x)) (set! x (add1 x))]) x))
   => 2
   (! (eq? 1 1)) => #t
   (! (eq? 1 2)) => #f
   (! (eqv? 1.0 1.0)) => #t
   (! (eqv? 1.0 1)) => #f
   (! (= 1.0 1)) => #t
   (! (equal? (list 1.0) (list 1.0))) => #t
   (! (letrec ([zs (cons 0 zs)]) (equal? (list zs zs) (list zs zs)))) => #t
   ))

(define (list-tests)
  (test
   (! (car 0)) =error> "car: contract violation\n  expected: pair?"
   (! (cdr 0)) =error> "cdr: contract violation\n  expected: pair?"
   (! (car (cons 1 (/ 1 0)))) => 1
   (! (cdr (cons (/ 1 0) 1))) => 1
   (! (list-ref (list (/ 1 0) 1 (/ 1 0)) 1)) => 1
   (! (list-ref (cons 1 (/ 1 0)) 0)) => 1 ; doesn't force list structure
   (! (list-tail (cons (/ 1 0) 0) 1)) => 0
   (! (length (list (/ 1 0) (/ 1 0) (/ 1 0)))) => 3
   (! (let ([l (list (/ 1 0) (/ 1 0))]) (length (append l l l)))) => 6
   (!! (member 1 (cons 0 (cons 1 2)))) => '(1 . 2)
   (!! (memq   1 (cons 0 (cons 1 2)))) => '(1 . 2)
   (!! (memv   1 (cons 0 (cons 1 2)))) => '(1 . 2)
   (! (second (map car (list 1 2 3)))) =error> "contract violation"
   (! (second (map car (list 1 '(2) 3)))) => 2
   ))

(define (take-tests)
  (define test-lst1 '(1 2 3))
  (test
   (! (take "nonnum" test-lst1))
   =error>
   #rx"take: expects type <non-negative exact integer> as 1st argument, given: \"nonnum\"; other arguments were: .*\\((list )?1 2 3\\)"
   (! (take -1 test-lst1))
   =error> "take: expects type <non-negative exact integer> as 1st argument"
   (! (take -1 "nonlist"))
   =error> "take: expects type <non-negative exact integer> as 1st argument"
   (! (take 0 "nonlist")) => '()
   (! (take 1 "nonlist")) =error> "take: not a proper list: \"nonlist\""
   (! (take 0 null)) => '()
   (! (take 0 test-lst1)) => '()
   (!! (take 1 test-lst1)) => '(1)
   (!! (take 2 test-lst1)) => '(1 2)
   (!! (take 3 (take 4 test-lst1))) => '(1 2 3) ; doesn't force the error
   (! (fourth (take 4 test-lst1)))              ; this one does
   =error> "take: index 4 too large for input list"
   (! (list-ref (take (~ 1) (list 2)) 0)) => 2
   (! (take 0 (error))) => '() ; doesn't even force the list structure
   (!! (take 1 (cons 0 (error "poof")))) => '(0)
   ))

(define (misc-tests)
  (define-struct a (b c))
  (define-struct d (e f))
  (test
   (! (a-b (make-a 1 2))) => 1
   (! (a-c (make-a 1 2))) => 2
   (! (a-b (a 1 2))) => 1
   (! (a-c (a 1 2))) => 2
   (! (a? (a 1 2))) => true
   (! (a? (d 1 2))) => false
   ))

; Lazy Racket examples from Premiers cours de programmation avec Scheme (Roy)
(define (pcps-tests)
  ;; Definitions --------------------------------------------------------------
  (define (f x y) x)
  (define (fac n)
    (if (= n 0) 1 (* n (fac (- n 1)))))
  (define (new-if test p q)
    (cond (test p)
          (else q)))
  (define fibs (cons 0 (cons 1 (map + fibs (cdr fibs)))))
  #;(define ($list-ref L k)
    (let loop ((k (force k)) (L (force L)))
      (if (= k 0)
          (car L)
          (loop (- k 1) (cdr (force L))))))
  (define L2 (cons 2 (map add1 L2)))
  (define (rayer n L)     ; L prive des multiples de n
    (filter (lambda (x) (not (= 0 (modulo x n)))) L))
  (define (crible L)
    (cons (car L) (crible (rayer (car L) (cdr L)))))
  (define PREMS (crible L2)) ; primes
  (define ZERO (cons 0 ZERO))     ; le flot infini <0,0,0,0,...>
  (define (poly->serie L)         ; L = coeffs en puissances croissantes
    (define (copy L)
      (if (null? L)
          ZERO                    ; padding à droite par des 0
          (cons (car L) (copy (cdr L)))))
    (copy L))
  (define (int-serie S)           ; integration terme a terme
    (define (aux S i)
      (cons (/ (car S) i) (aux (cdr S) (+ i 1))))
    (aux S 1))
  (define EXPO (cons 1 (int-serie EXPO)))
  (define SIN (cons 0 (int-serie COS)))
  (define COS (cons 1 (map - (int-serie SIN))))
  (define (ints-from n)
    (cons n (ints-from (+ n 1))))
  (define NAT (ints-from 0))
  (define UN (cons 1 UN))
  (define nats (cons 0 (map + nats UN)))
  (define QUATRE (filter (lambda (x) (zero? (modulo x 4))) NAT))
  (define (melanger F1 F2)     ; F1 et F2 infinis strictement croissants
    (cond ((< (car F1) (car F2)) (cons (car F1) (melanger (cdr F1) F2)))
          ((> (car F1) (car F2)) (cons (car F2) (melanger F1 (cdr F2))))
          (else (cons (car F1) (melanger (cdr F1) (cdr F2))))))
  (define (zoom x F)
    (cons (* (car F) x) (zoom x (cdr F))))
  (define PAIR (zoom 2 NAT))
  (define (hamming)
    (define h (cons 1 (melanger (zoom 2 h) (melanger (zoom 3 h) (zoom 5 h)))))
    h)
  (define h (hamming))
  (define FACT (cons 1 (map * FACT (cdr NAT))))
  (define (entrelacer s1 s2)
    (cons (car s1) (entrelacer s2 (cdr s1))))
  (define F (entrelacer NAT F))
  ;; Tests --------------------------------------------------------------------
  (test
   (!! (fac 5)) => 120
   (!! (new-if (= 1 2) (/ 1 0) 3)) => 3
   (!! (take 20 PREMS)) =>
   '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71)
   (!! (list-ref PREMS 999)) => 7919
   (!! (apply + (take 100 PREMS))) => 24133
   (!! (take 10 (poly->serie '(1 2 3)))) => '(1 2 3 0 0 0 0 0 0 0)
   (!! (take 10 EXPO)) => 
   '(1 1 1/2 1/6 1/24 1/120 1/720 1/5040 1/40320 1/362880)
   (!! (take 8 SIN)) => '(0 1 0 -1/6 0 1/120 0 -1/5040)
   (!! (take 8 COS)) => '(1 0 -1/2 0 1/24 0 -1/720 0)
   (!! (take 10 (ints-from 5))) => '(5 6 7 8 9 10 11 12 13 14)
   (!! (take 10 NAT)) => '(0 1 2 3 4 5 6 7 8 9)
   (!! (take 10 UN)) => '(1 1 1 1 1 1 1 1 1 1)
   (!! (take 10 nats)) => '(0 1 2 3 4 5 6 7 8 9)
   (!! (take 10 QUATRE)) => '(0 4 8 12 16 20 24 28 32 36)
   (!! (take 10 (melanger NAT QUATRE))) => '(0 1 2 3 4 5 6 7 8 9)
   (!! (take 10 PAIR)) => '(0 2 4 6 8 10 12 14 16 18)
   (!! (take 30 h)) => 
   '(1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 40 45 48 50 54 60 64 72 75 80)
   (!! (list-ref h 10000)) => 288555831593533440
   (!! (take 10 FACT)) => '(1 1 2 6 24 120 720 5040 40320 362880)
   (!! (take 10 (entrelacer NAT PAIR))) => '(0 0 1 2 2 4 3 6 4 8)
   (!! (take 10 F)) => '(0 0 1 0 2 1 3 0 4 2)))

(define (strictness-tests)
  (test
   (! (and (/ 1 0))) =error> "/: division by zero"
   (! (and #f (/ 1 0))) => #f
   (! (and #t (/ 1 0))) =error> "/: division by zero"
   (! (cdr (append (list (/ 1 0)) '()))) => '()
   (! (cdr (append '() (list (/ 1 0))))) => '()
   (! (append (/ 1 0) '())) =error> "/: division by zero"
   (! (append (/ 1 0) '() '())) =error> "/: division by zero"
   (! (append (/ 1 0) '(1))) =error> "/: division by zero"
   (! (append '() (/ 1 0))) =error> "/: division by zero"
   (! (car (append '(1) (/ 1 0)))) => 1
   (! (cdr (append '(1) (/ 1 0)))) =error> "/: division by zero"
   (! (car (append '(1) 1 (/ 1 0)))) => 1
   (! (foldr (/ 1 0) 0 '())) =error> "/: division by zero"
   (! (foldr 1 (/ 1 0) '())) =error> "/: division by zero"
   (! (foldr 1 2 (/ 1 0))) =error> "/: division by zero"
   (! (foldr (/ 1 0) 1 '(1))) =error> "/: division by zero"
   (! (foldr 1 (/ 1 0) '(1))) =error> "/: division by zero"
   (! (foldl (/ 1 0) 0 '())) =error> "/: division by zero"
   (! (foldl 1 (/ 1 0) '())) =error> "/: division by zero"
   (! (foldl 1 2 (/ 1 0))) =error> "/: division by zero"
   (! (foldl (/ 1 0) 1 '(1))) =error> "/: division by zero"
   (! (foldl 1 (/ 1 0) '(1))) =error> "/: division by zero"
   (! (filter (/ 1 0) '())) =error> "/: division by zero"
   (! (filter 1 (/ 1 0))) =error> "/: division by zero"
   (! (filter (/ 1 0) '(1))) =error> "/: division by zero"
   (! (map (/ 1 0) '())) =error> "/: division by zero"
   (! (map (/ 1 0) '(1))) =error> "/: division by zero"
   (! (map 1 (/ 1 0))) =error> "/: division by zero"
   (! (if (/ 1 0) 1 2)) =error> "/: division by zero"
   (! (if #t 1 (/ 1 0))) => 1
   (! (if #f (/ 1 0) 1)) => 1
   (! (andmap (/ 1 0) '())) =error> "/: division by zero"
   (! (andmap (/ 1 0) '(1))) =error> "/: division by zero"
   (! (andmap 1 (/ 1 0))) =error> "/: division by zero"
   ))

(define (values-tests)
  (test
   ;; Tests from Luke Whittlesey
   (! (let-values ([(x) (values (error "a"))]) 1)) => 1
   (! (let-values ([(x y) (values (error "a") (error "b"))]) 1)) => 1
   (! (let*-values ([(x) (values (error "a"))]) 1)) => 1
   (! (let*-values ([(x0 x1) (values (error "a") (error "b"))] [(y) (values x0)]) 1)) => 1
   (! (letrec ([x y] [y 1]) x)) =error> "y: undefined"
   (! (letrec ([x (list y)] [y 1]) (car x))) => 1
   (! (letrec-values ([(x) (values (error "a"))]) 1)) => 1
   (! (letrec-values ([(x y) (values (error "a") (error "b"))]) 1)) => 1
   (! (letrec-values ([(x) (values (list y))] [(y) (values 1)]) (car x))) => 1
   (! (letrec-values ([(x0 x1) (values (list y0) (list y1))] [(y0 y1) (values 1 2)])
        (+ (car x0) (car x1)))) => 3
   (! (letrec-values ([(A) (values (list 'a B))]
                      [(B) (values (list 'b A))]) (car A))) => 'a
   (! (letrec-values ([(A) (values (list 'a B))]
                      [(B) (values (list 'b A))]) (caadr A))) => 'b
   (! (letrec-values ([(A) (values (list 'a B))]
                      [(B) (values (list 'b A))]) (car B))) => 'b
   (! (letrec-values ([(A) (values (list 'a B))]
                      [(B) (values (list 'b A))]) (caadr B))) => 'a
   ;; this errors because let-values (and other values-extractors) must force
   ;; the rhs (one level down) to extract the values
   (let-values ([(x) (error "a")]) 1) =error> "a"
   ))

(provide lang-tests)
(module+ main (lang-tests))
(define (lang-tests)
  (! (begin (basic-tests)
            (list-tests)
            (take-tests)
            (misc-tests)
            (pcps-tests)
            (strictness-tests)
            (values-tests))))
