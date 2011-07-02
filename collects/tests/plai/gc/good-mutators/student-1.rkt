#lang plai/mutator

; mark-and-sweep-test.rkt - Ben Childs
; Designed to test the mark and sweep collector
; Runs three tests:
;
; Allocation of subsequently larger lists
;
; Use of Local variables in a loop (garbage after each iteration)
; Followed by allocation of large list (verifies that they are correctly collected)
;
; Generation of a number of circularly referenced lists
; Followed by allocation of several large lists
;
; Finally it runs the sample tests distributed with the assignment


(allocator-setup "../good-collectors/good-collector.rkt" 80)

; Helper to generate long lists
(define (gen-list x)
  (if (zero? x) '() (cons x (gen-list (- x 1)))))

; Function that defines local vars
(define (local-vars)
  (let ((x 3) (y 5) (z 10) (a 5))
    (+ x (- 10 y))))

(define (loop x)
  
  (printf "Iteration: ~a\n" x)
  
  (if (zero? x) 0
        (loop (- (+ (local-vars) (- x 1)) 8))))
; Generate gradually increasing sizes of lists
; To trigger garbage collection at different points
(printf "~a\n" (gen-list 1))
(printf "~a\n" (gen-list 2))
(printf "~a\n" (gen-list 4))
(printf "~a\n" (gen-list 8))

; Run a loop that uses local vars a few times
(printf "Generating Primitives in loops\n")
(loop 20)

(printf "Try Allocating large list again\n")
(printf "~a\n" (gen-list 8))


; Create some circular references
(define (gen-circular)
  (let ([x (cons 3 4)])
    (let ([y (cons 2 x)])
      (set-rest! x y)
      x)))

(printf "Testing Circular References\n")
(printf "~a\n" (gen-circular))
(printf "~a\n" (gen-circular))
(printf "~a\n" (gen-circular))
(printf "~a\n" (gen-circular))
(printf "~a\n" (gen-circular))
(printf "~a\n" (gen-circular))
(printf "~a\n" (gen-circular))
(printf "~a\n" (gen-circular))
(printf "~a\n" (gen-circular))

(printf "Try allocating large list again\n")
(printf "~a\n" (gen-list 8))
(printf "~a\n" (gen-list 8))
(printf "~a\n" (gen-list 8))
(printf "~a\n" (gen-list 8))
(printf "~a\n" (gen-list 8))

(printf "Running sample tests\n")
(define (fact x)
  (if (zero? x)
      1
      (* x (fact (sub1 x)))))

(define (fact-help x a)
  (if (zero? x)
      a
      (fact-help (sub1 x) (* x a))))

(define lst (cons 1 (cons 2 (cons 3 empty))))

(define (map-add n lst)
  (map (lambda (x) (+ n x)) lst))

(define (map f lst)
  (if (cons? lst)
      (cons (f (first lst)) (map f (rest lst)))
      empty))

(define (filter p lst)
  (if (cons? lst)
      (if (p (first lst))
          (cons (first lst) (filter p (rest lst)))
          (filter p (rest lst)))
      lst))

(define (append l1 l2)
  (if (cons? l1)
      (cons (first l1) (append (rest l1) l2))
      l2))

(define (length lst)
  (if (empty? lst)
      0
      (add1 (length (rest lst)))))

(define tail (cons 1 empty))
(define head (cons 4 (cons 3 (cons 2 tail))))
(set-rest! tail head)

(printf "res ~a\n" head)
(set! head empty)
(set! tail head)
(printf "res ~a\n" lst)
(printf "res ~a\n" (length '(hello goodbye)))
(printf "res ~a\n" (map sub1 lst))

(printf "(fact-help 15 1): ~a\n" (fact-help 15 1))
(printf "(fact 9): ~a\n" (fact 9))

(printf "(append lst lst): ~a\n" (append lst lst))

(printf "(map-add 5 lst): ~a\n" (map-add 5 lst))
(printf "(filter even? (map sub1 lst)): ~a\n" (filter even? (map sub1 lst)))
(printf "(length lst): ~a\n" (length lst))
