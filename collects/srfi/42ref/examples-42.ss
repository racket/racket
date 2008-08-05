; Examples for SRFI 42 as a module to PLT 204 ------------------------------
; sebastian_egner@yahoo.com, 26-Dec-2007, PLT 371.
;   + check fix for bugs found by sunnan and jens axel soegaard
; Sebastian.Egner@philips.com, 7-July-2003, PLT 204.
; For running the demos: (require srfi/42/examples-42)
; For anything else: http://srfi.schemers.org/srfi-42/

(module |examples-42| mzscheme

  (require srfi/42)

  (define (my-open-output-file filename)
    (open-output-file filename 'replace 'text) )

  (define (my-call-with-input-file filename thunk)
    (call-with-input-file filename thunk 'text) )

; examples.scm starts here -------------------------------------------------

; <PLAINTEXT>
; Examples for Eager Comprehensions in [outer..inner|expr]-Convention
; ===================================================================
;
; sebastian.egner@philips.com, Eindhoven, The Netherlands, Feb-2003.
; Scheme R5RS (incl. macros), SRFI-23 (error).
; 
; Running the examples in Scheme48 (version 0.57):
;   ,open srfi-23
;   ,load ec.scm
;   (define my-open-output-file open-output-file)
;   (define my-call-with-input-file call-with-input-file)
;   ,load examples.scm
;
; Running the examples in PLT/DrScheme (version 202): 
;   ; open "ec.scm", click Execute
;   (define (my-open-output-file filename)
;     (open-output-file filename 'replace 'text) )
;   (define (my-call-with-input-file filename thunk)
;     (call-with-input-file filename thunk 'text) )
;   (load "examples.scm")
;
; Running the examples in PLT/DrScheme (version 204): 
;   (require srfi/42-examples)
;
; Running the examples in SCM (version 5d7):
;   (require 'macro) (require 'record)
;   (load "ec.scm")
;   (define my-open-output-file open-output-file)
;   (define my-call-with-input-file call-with-input-file)
;   (load "examples.scm")


; Tools for checking results
; ==========================

(define (my-equal? x y)
  (cond
   ((or (boolean? x) 
        (null? x)
        (symbol? x) 
        (char? x) 
        (input-port? x)
        (output-port? x) )
    (eqv? x y) )
   ((string? x)
    (and (string? y) (string=? x y)) )
   ((vector? x)
    (and (vector? y)
         (my-equal? (vector->list x) (vector->list y)) ))
   ((pair? x)
    (and (pair? y)
         (my-equal? (car x) (car y))
         (my-equal? (cdr x) (cdr y)) ))
   ((real? x)
    (and (real? y)
         (eqv? (exact? x) (exact? y))
         (if (exact? x)
             (= x y)
             (< (abs (- x y)) (/ 1 (expt 10 6))) ))) ; will do here
   (else
    (error "unrecognized type" x) )))

(define my-check-correct 0)
(define my-check-wrong   0)

(define-syntax my-check
  (syntax-rules (=>)
    ((my-check ec => desired-result)
     (begin
       (newline)
       (write (quote ec))
       (newline)
       (let ((actual-result ec))
         (display "  => ")
         (write actual-result)
         (if (my-equal? actual-result desired-result)
             (begin
               (display " ; correct")
               (set! my-check-correct (+ my-check-correct 1)) )
             (begin
               (display " ; *** wrong ***, desired result:")
               (newline)
               (display "  => ")
               (write desired-result)
               (set! my-check-wrong (+ my-check-wrong 1)) ))
         (newline) )))))
             

; ==========================================================================
; do-ec 
; ==========================================================================

(my-check 
  (let ((x 0)) (do-ec (set! x (+ x 1))) x) 
  => 1)

(my-check 
  (let ((x 0)) (do-ec (:range i 10) (set! x (+ x 1))) x) 
  => 10)

(my-check 
  (let ((x 0)) (do-ec (:range n 10) (:range k n) (set! x (+ x 1))) x) 
  => 45)


; ==========================================================================
; list-ec and basic qualifiers 
; ==========================================================================

(my-check (list-ec 1) => '(1))

(my-check (list-ec (:range i 4) i) => '(0 1 2 3))

(my-check (list-ec (:range n 3) (:range k (+ n 1)) (list n k)) 
  => '((0 0) (1 0) (1 1) (2 0) (2 1) (2 2)) )

(my-check 
  (list-ec (:range n 5) (if (even? n)) (:range k (+ n 1)) (list n k)) 
  => '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )


(my-check 
  (list-ec (:range n 5) (not (even? n)) (:range k (+ n 1)) (list n k)) 
  => '((1 0) (1 1) (3 0) (3 1) (3 2) (3 3)) )

(my-check
  (list-ec (:range n 5) 
           (and (even? n) (> n 2)) 
           (:range k (+ n 1)) 
           (list n k) )
  => '((4 0) (4 1) (4 2) (4 3) (4 4)) )

(my-check
  (list-ec (:range n 5) 
           (or (even? n) (> n 3)) 
           (:range k (+ n 1)) 
           (list n k) )
  => '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4)) )

(my-check
 (let ((x 0)) (list-ec (:range n 10) (begin (set! x (+ x 1))) n) x)
 => 10 )

(my-check
 (list-ec (nested (:range n 3) (:range k n)) k)
 => '(0 0 1) )


; ==========================================================================
; Other comprehensions
; ==========================================================================

(my-check (append-ec '(a b)) => '(a b))
(my-check (append-ec (:range i 0) '(a b)) => '())
(my-check (append-ec (:range i 1) '(a b)) => '(a b))
(my-check (append-ec (:range i 2) '(a b)) => '(a b a b))

(my-check (string-ec #\a) => (string #\a))
(my-check (string-ec (:range i 0) #\a) => "")
(my-check (string-ec (:range i 1) #\a) => "a")
(my-check (string-ec (:range i 2) #\a) => "aa")

(my-check (string-append-ec "ab") => "ab")
(my-check (string-append-ec (:range i 0) "ab") => "")
(my-check (string-append-ec (:range i 1) "ab") => "ab")
(my-check (string-append-ec (:range i 2) "ab") => "abab")

(my-check (vector-ec 1) => (vector 1))
(my-check (vector-ec (:range i 0) i) => (vector))
(my-check (vector-ec (:range i 1) i) => (vector 0))
(my-check (vector-ec (:range i 2) i) => (vector 0 1))

(my-check (vector-of-length-ec 1 1) => (vector 1))
(my-check (vector-of-length-ec 0 (:range i 0) i) => (vector))
(my-check (vector-of-length-ec 1 (:range i 1) i) => (vector 0))
(my-check (vector-of-length-ec 2 (:range i 2) i) => (vector 0 1))

(my-check (sum-ec 1) => 1)
(my-check (sum-ec (:range i 0) i) => 0)
(my-check (sum-ec (:range i 1) i) => 0)
(my-check (sum-ec (:range i 2) i) => 1)
(my-check (sum-ec (:range i 3) i) => 3)

(my-check (product-ec 1) => 1)
(my-check (product-ec (:range i 1 0) i) => 1)
(my-check (product-ec (:range i 1 1) i) => 1)
(my-check (product-ec (:range i 1 2) i) => 1)
(my-check (product-ec (:range i 1 3) i) => 2)
(my-check (product-ec (:range i 1 4) i) => 6)

(my-check (min-ec 1) => 1)
(my-check (min-ec (:range i 1) i) => 0)
(my-check (min-ec (:range i 2) i) => 0)

(my-check (max-ec 1) => 1)
(my-check (max-ec (:range i 1) i) => 0)
(my-check (max-ec (:range i 2) i) => 1)

(my-check (first-ec #f 1) => 1)
(my-check (first-ec #f (:range i 0) i) => #f)
(my-check (first-ec #f (:range i 1) i) => 0)
(my-check (first-ec #f (:range i 2) i) => 0)

(my-check 
  (let ((last-i -1))
    (first-ec #f (:range i 10) (begin (set! last-i i)) i)
    last-i )
  => 0 )

(my-check (last-ec #f 1) => 1)
(my-check (last-ec #f (:range i 0) i) => #f)
(my-check (last-ec #f (:range i 1) i) => 0)
(my-check (last-ec #f (:range i 2) i) => 1)

(my-check (any?-ec #f) => #f)
(my-check (any?-ec #t) => #t)
(my-check (any?-ec (:range i 2 2) (even? i)) => #f)
(my-check (any?-ec (:range i 2 3) (even? i)) => #t)

(my-check (every?-ec #f) => #f)
(my-check (every?-ec #t) => #t)
(my-check (every?-ec (:range i 2 2) (even? i)) => #t)
(my-check (every?-ec (:range i 2 3) (even? i)) => #t)
(my-check (every?-ec (:range i 2 4) (even? i)) => #f)

(my-check 
 (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
   (fold-ec 0 (:range i 10) i sum-sqr) )
 => 285 )

(my-check 
 (let ((minus-1 (lambda (x) (- x 1)))
       (sum-sqr (lambda (x result) (+ result (* x x)))))
   (fold3-ec (error "wrong") (:range i 10) i minus-1 sum-sqr) )
 => 284 )

(my-check 
 (fold3-ec 'infinity (:range i 0) i min min)
 => 'infinity )


; ==========================================================================
; Typed generators
; ==========================================================================

(my-check (list-ec (:list x '()) x) => '())
(my-check (list-ec (:list x '(1)) x) => '(1))
(my-check (list-ec (:list x '(1 2 3)) x) => '(1 2 3))
(my-check (list-ec (:list x '(1) '(2)) x) => '(1 2))
(my-check (list-ec (:list x '(1) '(2) '(3)) x) => '(1 2 3))

(my-check (list-ec (:string c "") c) => '())
(my-check (list-ec (:string c "1") c) => '(#\1))
(my-check (list-ec (:string c "123") c) => '(#\1 #\2 #\3))
(my-check (list-ec (:string c "1" "2") c) => '(#\1 #\2))
(my-check (list-ec (:string c "1" "2" "3") c) => '(#\1 #\2 #\3))

(my-check (list-ec (:vector x (vector)) x) => '())
(my-check (list-ec (:vector x (vector 1)) x) => '(1))
(my-check (list-ec (:vector x (vector 1 2 3)) x) => '(1 2 3))
(my-check (list-ec (:vector x (vector 1) (vector 2)) x) => '(1 2))
(my-check 
 (list-ec (:vector x (vector 1) (vector 2) (vector 3)) x)
 => '(1 2 3))

(my-check (list-ec (:range x -2) x) => '())
(my-check (list-ec (:range x -1) x) => '())
(my-check (list-ec (:range x  0) x) => '())
(my-check (list-ec (:range x  1) x) => '(0))
(my-check (list-ec (:range x  2) x) => '(0 1))

(my-check (list-ec (:range x  0  3) x) => '(0 1 2))
(my-check (list-ec (:range x  1  3) x) => '(1 2))
(my-check (list-ec (:range x -2 -1) x) => '(-2))
(my-check (list-ec (:range x -2 -2) x) => '())

(my-check (list-ec (:range x 1 5  2) x) => '(1 3))
(my-check (list-ec (:range x 1 6  2) x) => '(1 3 5))
(my-check (list-ec (:range x 5 1 -2) x) => '(5 3))
(my-check (list-ec (:range x 6 1 -2) x) => '(6 4 2))

(my-check (list-ec (:real-range x 0.0 3.0)     x) => '(0. 1. 2.))
(my-check (list-ec (:real-range x 0   3.0)     x) => '(0. 1. 2.))
(my-check (list-ec (:real-range x 0   3   1.0) x) => '(0. 1. 2.))

(my-check 
 (string-ec (:char-range c #\a #\z) c) 
 => "abcdefghijklmnopqrstuvwxyz" )

(my-check 
 (begin
   (let ((f (my-open-output-file "tmp1")))
     (do-ec (:range n 10) (begin (write n f) (newline f)))
     (close-output-port f))
   (my-call-with-input-file "tmp1"
    (lambda (port) (list-ec (:port x port read) x)) ))
 => (list-ec (:range n 10) n) )

(my-check 
 (begin
   (let ((f (my-open-output-file "tmp1")))
     (do-ec (:range n 10) (begin (write n f) (newline f)))
     (close-output-port f))
   (my-call-with-input-file "tmp1"                 
     (lambda (port) (list-ec (:port x port) x)) ))
 => (list-ec (:range n 10) n) )


; ==========================================================================
; The special generators :do :let :parallel :while :until
; ==========================================================================

(my-check (list-ec (:do ((i 0)) (< i 4) ((+ i 1))) i) => '(0 1 2 3))

(my-check 
 (list-ec 
  (:do (let ((x 'x)))
       ((i 0)) 
       (< i 4) 
       (let ((j (- 10 i))))
       #t
       ((+ i 1)) )
  j )
 => '(10 9 8 7) )

(my-check (list-ec (:let x 1) x) => '(1))
(my-check (list-ec (:let x 1) (:let y (+ x 1)) y) => '(2))
(my-check (list-ec (:let x 1) (:let x (+ x 1)) x) => '(2))

(my-check 
 (list-ec (:parallel (:range i 1 10) (:list x '(a b c))) (list i x))
 => '((1 a) (2 b) (3 c)) )


(my-check 
 (list-ec (:while (:range i 1 10) (< i 5)) i)
 => '(1 2 3 4) )

(my-check 
 (list-ec (:until (:range i 1 10) (>= i 5)) i)
 => '(1 2 3 4 5) )

; with generator that might use inner bindings

(my-check
 (list-ec (:while (:list i '(1 2 3 4 5 6 7 8 9)) (< i 5)) i)
 => '(1 2 3 4) )
; Was broken in original reference implementation as pointed
; out by sunnan@handgranat.org on 24-Apr-2005 comp.lang.scheme.
; Refer to http://groups-beta.google.com/group/comp.lang.scheme/
; browse_thread/thread/f5333220eaeeed66/75926634cf31c038#75926634cf31c038

(my-check 
 (list-ec (:until (:list i '(1 2 3 4 5 6 7 8 9)) (>= i 5)) i)
 => '(1 2 3 4 5) )

(my-check
 (list-ec (:while (:vector x (index i) '#(1 2 3 4 5))
		  (< x 10))
	  x)
 => '(1 2 3 4 5))
; Was broken in reference implementation, even after fix for the
; bug reported by Sunnan, as reported by Jens-Axel Soegaard on
; 4-Jun-2007.

; combine :while/:until and :parallel

(my-check
 (list-ec (:while (:parallel (:range i 1 10)
                             (:list j '(1 2 3 4 5 6 7 8 9)))
                  (< i 5))
          (list i j))
 => '((1 1) (2 2) (3 3) (4 4)))

(my-check
 (list-ec (:until (:parallel (:range i 1 10)
                             (:list j '(1 2 3 4 5 6 7 8 9)))
                  (>= i 5))
          (list i j))
 => '((1 1) (2 2) (3 3) (4 4) (5 5)))

; check that :while/:until really stop the generator

(my-check
 (let ((n 0))
   (do-ec (:while (:range i 1 10) (begin (set! n (+ n 1)) (< i 5)))
          (if #f #f))
   n)
 => 5)

(my-check
 (let ((n 0))
   (do-ec (:until (:range i 1 10) (begin (set! n (+ n 1)) (>= i 5)))
          (if #f #f))
   n)
 => 5)

(my-check
 (let ((n 0))
   (do-ec (:while (:parallel (:range i 1 10)
                             (:do () (begin (set! n (+ n 1)) #t) ()))
                  (< i 5))
          (if #f #f))
   n)
 => 5)

(my-check
 (let ((n 0))
   (do-ec (:until (:parallel (:range i 1 10)
                             (:do () (begin (set! n (+ n 1)) #t) ()))
                  (>= i 5))
          (if #f #f))
   n)
 => 5)

; ==========================================================================
; The dispatching generator
; ==========================================================================

(my-check (list-ec (: c '(a b)) c) => '(a b))
(my-check (list-ec (: c '(a b) '(c d)) c) => '(a b c d))

(my-check (list-ec (: c "ab") c) => '(#\a #\b))
(my-check (list-ec (: c "ab" "cd") c) => '(#\a #\b #\c #\d))

(my-check (list-ec (: c (vector 'a 'b)) c) => '(a b))
(my-check (list-ec (: c (vector 'a 'b) (vector 'c)) c) => '(a b c))

(my-check (list-ec (: i 0) i) => '())
(my-check (list-ec (: i 1) i) => '(0))
(my-check (list-ec (: i 10) i) => '(0 1 2 3 4 5 6 7 8 9))
(my-check (list-ec (: i 1 2) i) => '(1))
(my-check (list-ec (: i 1 2 3) i) => '(1))
(my-check (list-ec (: i 1 9 3) i) => '(1 4 7))

(my-check (list-ec (: i 0.0 1.0 0.2) i) => '(0. 0.2 0.4 0.6 0.8))

(my-check (list-ec (: c #\a #\c) c) => '(#\a #\b #\c))

(my-check 
 (begin
   (let ((f (my-open-output-file "tmp1")))
     (do-ec (:range n 10) (begin (write n f) (newline f)))
     (close-output-port f))
   (my-call-with-input-file "tmp1"                 
     (lambda (port) (list-ec (: x port read) x)) ))
 => (list-ec (:range n 10) n) )
    
(my-check 
 (begin
   (let ((f (my-open-output-file "tmp1")))
     (do-ec (:range n 10) (begin (write n f) (newline f)))
     (close-output-port f))
   (my-call-with-input-file "tmp1"                 
     (lambda (port) (list-ec (: x port) x)) ))
 => (list-ec (:range n 10) n) )


; ==========================================================================
; With index variable
; ==========================================================================

(my-check (list-ec (:list c (index i) '(a b)) (list c i)) => '((a 0) (b 1)))
(my-check (list-ec (:string c (index i) "a") (list c i)) => '((#\a 0)))
(my-check (list-ec (:vector c (index i) (vector 'a)) (list c i)) => '((a 0)))

(my-check 
 (list-ec (:range i (index j) 0 -3 -1) (list i j)) 
 => '((0 0) (-1 1) (-2 2)) )

(my-check 
 (list-ec (:real-range i (index j) 0 1 0.2) (list i j)) 
 => '((0. 0) (0.2 1) (0.4 2) (0.6 3) (0.8 4)) )

(my-check 
 (list-ec (:char-range c (index i) #\a #\c) (list c i)) 
 => '((#\a 0) (#\b 1) (#\c 2)) )

(my-check 
 (list-ec (: x (index i) '(a b c d)) (list x i))
 => '((a 0) (b 1) (c 2) (d 3)) )

(my-check 
 (begin
   (let ((f (my-open-output-file "tmp1")))
     (do-ec (:range n 10) (begin (write n f) (newline f)))
     (close-output-port f))
   (my-call-with-input-file "tmp1"
     (lambda (port) (list-ec (: x (index i) port) (list x i))) ))
 => '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9)) )


; ==========================================================================
; The examples from the SRFI document
; ==========================================================================

; from Abstract

(my-check (list-ec (: i 5) (* i i)) => '(0 1 4 9 16))

(my-check 
  (list-ec (: n 1 4) (: i n) (list n i)) 
  => '((1 0) (2 0) (2 1) (3 0) (3 1) (3 2)) )

; from Generators

(my-check 
  (list-ec (: x (index i) "abc") (list x i)) 
  => '((#\a 0) (#\b 1) (#\c 2)) )

(my-check
  (list-ec (:string c (index i) "a" "b") (cons c i))
  => '((#\a . 0) (#\b . 1)) )


; ==========================================================================
; Little Shop of Horrors
; ==========================================================================

(my-check (list-ec (:range x 5) (:range x x) x) => '(0 0 1 0 1 2 0 1 2 3))

(my-check (list-ec (:list x '(2 "23" (4))) (: y x) y) => '(0 1 #\2 #\3 4))

(my-check 
 (list-ec (:parallel (:integers x) 
                     (:do ((i 10)) (< x i) ((- i 1))))
          (list x i))
 => '((0 10) (1 9) (2 8) (3 7) (4 6)) )


; ==========================================================================
; Less artificial examples
; ==========================================================================

(define (factorial n) ; n * (n-1) * .. * 1 for n >= 0
  (product-ec (:range k 2 (+ n 1)) k) )

(my-check (factorial  0) => 1)
(my-check (factorial  1) => 1)
(my-check (factorial  3) => 6)
(my-check (factorial  5) => 120)


(define (eratosthenes n) ; primes in {2..n-1} for n >= 1
  (let ((p? (make-string n #\1)))
    (do-ec (:range k 2 n)
           (if (char=? (string-ref p? k) #\1))
           (:range i (* 2 k) n k)
           (string-set! p? i #\0) )
    (list-ec (:range k 2 n) (if (char=? (string-ref p? k) #\1)) k) ))

(my-check 
 (eratosthenes 50)
 => '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47) )

(my-check
 (length (eratosthenes 100000))
 => 9592 ) ; we expect 10^5/ln(10^5)


(define (pythagoras n) ; a, b, c s.t. 1 <= a <= b <= c <= n, a^2 + b^2 = c^2
  (list-ec 
   (:let sqr-n (* n n))
   (:range a 1 (+ n 1))
; (begin (display a) (display " "))
   (:let sqr-a (* a a))
   (:range b a (+ n 1)) 
   (:let sqr-c (+ sqr-a (* b b)))
   (if (<= sqr-c sqr-n))
   (:range c b (+ n 1))
   (if (= (* c c) sqr-c))
   (list a b c) ))
           
(my-check
 (pythagoras 15)
 => '((3 4 5) (5 12 13) (6 8 10) (9 12 15)) )

(my-check
 (length (pythagoras 200))
 => 127 )


(define (qsort xs) ; stable
  (if (null? xs)
      '()
      (let ((pivot (car xs)) (xrest (cdr xs)))
        (append
         (qsort (list-ec (:list x xrest) (if (<  x pivot)) x))
         (list pivot)
         (qsort (list-ec (:list x xrest) (if (>= x pivot)) x)) ))))

(my-check 
 (qsort '(1 5 4 2 4 5 3 2 1 3))
 => '(1 1 2 2 3 3 4 4 5 5) )


(define (pi-BBP m) ; approx. of pi within 16^-m (Bailey-Borwein-Plouffe)
  (sum-ec 
    (:range n 0 (+ m 1))
    (:let n8 (* 8 n))
    (* (- (/ 4 (+ n8 1))
          (+ (/ 2 (+ n8 4))
             (/ 1 (+ n8 5))
             (/ 1 (+ n8 6))))
       (/ 1 (expt 16 n)) )))

(my-check
 (pi-BBP 5)
 => (/ 40413742330349316707 12864093722915635200) )


(define (my-read-line port) ; next line (incl. #\newline) of port
  (let ((line
         (string-ec 
          (:until (:port c port read-char)
                  (char=? c #\newline) )
          c )))
    (if (string=? line "")
        (read-char port) ; eof-object
        line )))

(define (my-read-lines filename) ; list of all lines
  (my-call-with-input-file 
   filename
   (lambda (port)
     (list-ec (:port line port my-read-line) line) )))

(my-check
 (begin
   (let ((f (my-open-output-file "tmp1")))
     (do-ec (:range n 10) (begin (write n f) (newline f)))
     (close-output-port f))
   (my-read-lines "tmp1") )
 => (list-ec (:char-range c #\0 #\9) (string c #\newline)) )


; ==========================================================================
; Summary
; ==========================================================================

(begin
  (newline)
  (newline)
  (display "correct examples : ")
  (display my-check-correct)
  (newline)
  (display "wrong examples   : ")
  (display my-check-wrong)
  (newline)
  (newline) )

; examples.scm ends here ---------------------------------------------------
) ; end module
