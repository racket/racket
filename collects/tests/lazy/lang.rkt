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
   (! (car 0)) =error> "car: expects argument of type <pair>"
   (! (cdr 0)) =error> "cdr: expects argument of type <pair>"
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
   (! (second (map car (list 1 2 3)))) =error> "expects argument of type"
   (! (second (map car (list 1 '(2) 3)))) => 2
   ))

(define (take-tests)
  (define test-lst1 '(1 2 3))
  (test
   (! (take "nonnum" test-lst1))
   =error>
   #rx"take: expects type <non-negative exact integer> as 1st .* '\\(1 2 3\\)"
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

(provide lang-tests)
(define (lang-tests)
  (! (begin (basic-tests)
            (list-tests)
            (take-tests)
            (misc-tests))))
