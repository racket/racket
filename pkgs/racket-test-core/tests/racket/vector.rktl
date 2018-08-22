
(load-relative "loadtest.rktl")

(Section 'vector)

(require racket/vector)


(test #t vector? '#(0 (2 2 2 2) "Anna"))
(test #t vector? '#())
(arity-test vector? 1 1)
(test '#(a b c) vector 'a 'b 'c)
(test '#() vector)
(test 3 vector-length '#(0 (2 2 2 2) "Anna"))
(test 0 vector-length '#())
(arity-test vector-length 1 1)
(err/rt-test (vector-length "apple"))
(test 8 vector-ref '#(1 1 2 3 5 8 13 21) 5)
(arity-test vector-ref 2 2)
(err/rt-test (vector-ref "apple" 3))
(err/rt-test (vector-ref #(4 5 6) 3) exn:application:mismatch?)
(err/rt-test (vector-ref #() 0) exn:application:mismatch?)
(err/rt-test (vector-ref #() (expt 2 100)) exn:application:mismatch?)
(err/rt-test (vector-ref #(4 5 6) -1))
(err/rt-test (vector-ref #(4 5 6) 2.0))
(err/rt-test (vector-ref #(4 5 6) "2"))
(test '#(0 ("Sue" "Sue") "Anna") 'vector-set!
	(let ((vec (vector 0 '(2 2 2 2) "Anna")))
	  (vector-set! vec 1 '("Sue" "Sue"))
	  vec))
(test '#(0 1 2) 'vector-set*!
      (let ([vec (vector #f #f #f)])
        (vector-set*! vec
                      0 (gensym) 0 0
                      1 (gensym) 1 1
                      2 (gensym) 2 2)
        vec))

(test '#(hi hi) make-vector 2 'hi)
(test '#() make-vector 0)
(test '#() make-vector 0 'a)
(test 2048 vector-length (make-vector 2048 'a))
(arity-test make-vector 1 2)
(err/rt-test (make-vector "a" 'a))
(err/rt-test (make-vector 1.0 'a))
(err/rt-test (make-vector 10.2 'a))
(err/rt-test (make-vector -1 'a))
(err/rt-test (make-vector 1000000000000000000000 'a) exn:fail:out-of-memory?)
(arity-test vector-set! 3 3)
(err/rt-test (vector-set*! #() 0 'x 1) exn:fail?)
(err/rt-test (vector-set! #() 0 'x) exn:application:mismatch?)
(err/rt-test (vector-set! #(1 2 3) -1 'x))
(err/rt-test (vector-set! #(1 2 3) 3 'x) exn:application:mismatch?)
(err/rt-test (vector-set! #(1 2 3) (expt 2 100) 'x) exn:application:mismatch?)
(err/rt-test (vector-set! '(1 2 3) 2 'x))
(err/rt-test (vector-set! #(1 2 3) "2" 'x))
(define v (vector 1 2 3))
(vector-fill! v 0)
(test (quote #(0 0 0)) 'vector-fill! v)
(arity-test vector-fill! 2 2)
(err/rt-test (vector-fill! '(1 2 3) 0))



;; ---------- vector-take/drop[-right] ----------
(let ()
  (define-syntax-rule (vals-list expr)
    (call-with-values (lambda () expr) list))
  (define (split-at*       l n) (vals-list (vector-split-at       l n)))
  (define (split-at-right* l n) (vals-list (vector-split-at-right l n)))
  (define funs (list vector-take vector-drop vector-take-right vector-drop-right
                     split-at* split-at-right*))
  (define tests
    ;; -----args------ --take--- --drop--- --take-r--- --drop-r-
    '([(#(a b c d) 2)   #(a b)     #(c d)     #(c d)       #(a b)    ]
      [(#(a b c d) 0)   #()        #(a b c d) #()          #(a b c d)]
      [(#(a b c d) 4)   #(a b c d) #()        #(a b c d)   #()       ]))
  (for ([t tests]
        #:when #t
        [expect `(,@(cdr t)
                  ,(list (list-ref t 1) (list-ref t 2))
                  ,(list (list-ref t 4) (list-ref t 3)))]
        [fun funs])
    (apply test expect fun (car t)))
  (for ([fun funs])
    (arity-test fun 2 2)
    (err/rt-test (fun #(1 2 3) 2.0))
    (err/rt-test (fun #(1) '(1)))
    (err/rt-test (fun #(1) -1))
    (err/rt-test (fun #(1) 2) exn:application:mismatch?)))

;; ---------- vector-append ----------
(let ()
  (test #() vector-append #())
  (test #() vector-append #() #())
  (test #(1 2) vector-append #(1 2) #())
  (test #(1 2) vector-append #() #(1 2))
  (test #(a b) vector-append #(a) #(b))
  (test #(a b c) vector-append #(a b) #() #(c))
  (test #(a b d c) vector-append #(a b) #(d) #(c)))


;; ---------- vector-filter[-not] ----------
(let ()
  (define f vector-filter)
  (define fn vector-filter-not)

  (test #()              f  number? #())
  (test #()              fn number? #())
  (test #(1 2 3)         f  number? #(1 a 2 b 3 c d))
  (test #(a b c d)       fn number? #(1 a 2 b 3 c d))
  (test #()              f  string? #(1 a 2 b 3 c d))
  (test #(1 a 2 b 3 c d) fn string? #(1 a 2 b 3 c d))
  (err/rt-test (f  2 #(1 2 3)))
  (err/rt-test (fn 2 #(1 2 3)))
  (err/rt-test (f cons #(1 2 3)))
  (err/rt-test (fn cons #(1 2 3)))
  (arity-test f  2 2)
  (arity-test fn 2 2))


;; ---------- vector-count ----------

(let ()
  (test 0 vector-count even? #())
  (test 4 vector-count even? #(0 2 4 6))
  (test 0 vector-count even? #(1 3 5 7))
  (test 2 vector-count even? #(1 2 3 4))
  (test 2 vector-count < #(1 2 3 4) #(4 3 2 1)))

;; ---------- vector-copy ----------

(let ()
  (test #() vector-copy #())
  (test #(1 2 3) vector-copy #(1 2 3))
  (test #() vector-copy #(1 2 3) 3)
  (test #(2 3) vector-copy #(1 2 3) 1)
  (test #(2) vector-copy #(1 2 3) 1 2)
  (test #f immutable? (vector-copy #(1 2 3)))
  (let ([v (vector 1 2 3)])
    (test #f eq? v (vector-copy v))))



;; ---------- vector-arg{min,max} ----------

(let ()

  (define ((check-regs . regexps) exn)
    (and (exn:fail? exn)
         (andmap (λ (reg) (regexp-match reg (exn-message exn)))
                 regexps)))

  (test 'vector-argmin object-name vector-argmin)
  (test 1 vector-argmin (lambda (x) 0) (vector 1))
  (test 1 vector-argmin (lambda (x) x) (vector 1 2 3))
  (test 1 vector-argmin (lambda (x) 1) (vector 1 2 3))

  (test 3
        'vector-argmin-makes-right-number-of-calls
        (let ([c 0])
          (vector-argmin (lambda (x) (set! c (+ c 1)) 0)
			 (vector 1 2 3))
          c))

  (test '(1 banana) vector-argmin car #((3 pears) (1 banana) (2 apples)))

  (err/rt-test (vector-argmin 1 (vector 1)) (check-regs #rx"vector-argmin" #rx"any/c . -> . real"))
  (err/rt-test (vector-argmin (lambda (x) x) 3) (check-regs #rx"vector-argmin" #rx"vector"))
  (err/rt-test (vector-argmin (lambda (x) x) (vector 1 #f)) (check-regs #rx"vector-argmin" #rx"real"))
  (err/rt-test (vector-argmin (lambda (x) x) (vector #f)) (check-regs #rx"vector-argmin" #rx"real"))

  (err/rt-test (vector-argmin (lambda (x) x) (vector +i)) (check-regs #rx"vector-argmin" #rx"real"))
  (err/rt-test (vector-argmin (lambda (x) x) (vector)) (check-regs #rx"vector-argmin" #rx".and/c vector.*vector-length"))

  (test 'vector-argmax object-name vector-argmax)
  (test 1 vector-argmax (lambda (x) 0) (vector 1))
  (test 3 vector-argmax (lambda (x) x) (vector 1 2 3))
  (test 1 vector-argmax (lambda (x) 1) (vector 1 2 3))

  (test 3
        'vector-argmax-makes-right-number-of-calls
        (let ([c 0])
          (vector-argmax (lambda (x) (set! c (+ c 1)) 0)
                  (vector 1 2 3))
          c))

  (test '(3 pears) vector-argmax car #((3 pears) (1 banana) (2 apples)))

  (err/rt-test (vector-argmax 1 (vector 1)) (check-regs #rx"vector-argmax" #rx"any/c . -> . real"))
  (err/rt-test (vector-argmax (lambda (x) x) 3) (check-regs #rx"vector-argmax" #rx"vector"))
  (err/rt-test (vector-argmax (lambda (x) x) (vector 1 #f)) (check-regs #rx"vector-argmax" #rx"real"))
  (err/rt-test (vector-argmax (lambda (x) x) (vector #f)) (check-regs #rx"vector-argmax" #rx"real"))

  (err/rt-test (vector-argmax (lambda (x) x) (vector +i)) (check-regs #rx"vector-argmax" #rx"real"))
  (err/rt-test (vector-argmax (lambda (x) x) (vector)) (check-regs #rx"vector-argmax" #rx".and/c vector.*vector-length")))

;; vector-mem{ber,v,q}

  (test 0 vector-member 7 #(7 1 2))
  (test #f vector-member 7 #(0 1 2))
  (test 1 vector-memq 'x #(7 x 2))
  (test 1 vector-memv 'x #(7 x 2))
  (test #f vector-memq (* 10 100000000000000000000) #(7 1000000000000000000000 2))
  (test 1 vector-memv (* 10 100000000000000000000) #(7 1000000000000000000000 2))
  (test 1 vector-member (* 10 100000000000000000000) #(7 1000000000000000000000 2))
  (test #f vector-memq (cons 1 2) (vector 7 (cons 1 2) 2))
  (test #f vector-memv (cons 1 2) (vector 7 (cons 1 2) 2))
  (test 1 vector-member (cons 1 2) (vector 7 (cons 1 2) 2))

;; ---------------------------------------- vector-map

(let ()

  (define ((check-regs . regexps) exn)
    (and (exn:fail? exn)
         (andmap (λ (reg) (regexp-match reg (exn-message exn)))
                 regexps)))
  (test #(2) vector-map add1 #(1))
  (test #(1 2 3) vector-map (lambda (x y) (max x y)) #(1 -2 -3) #(0 2 3))
  (let ([vec (vector 1 -2 -3)])
    (test #(1 2 3) vector-map! (lambda (x y) (max x y)) vec #(0 2 3))
    (test #(1 2 3) values vec))
  (err/rt-test (vector-map! add1 #(1)))
  (err/rt-test (vector-map 1 #()) (check-regs #rx"vector-map" #rx"procedure"))
  (err/rt-test (vector-map (lambda (x) x) 1) (check-regs #rx"vector-map" #rx"vector"))
  (err/rt-test (vector-map (lambda (x) x) #() 1) (check-regs #rx"vector-map" #rx"vector"))
  (err/rt-test (vector-map (lambda (x) x) #() #(1)) (check-regs #rx"vector-map" #rx"same size"))
  (err/rt-test (vector-map (lambda (x) x) #() #() #()) (check-regs #rx"vector-map" #rx"mismatch between procedure arity")))


;; ---------- vector-sort basic ----------
(test #("a" "b" "c" "c" "d" "e" "f")
      vector-sort
      #("d" "f" "e" "c" "a" "c" "b")
      string<?)
(test #("a" "c" "e")
      vector-sort
      #("d" "f" "e" "c" "a" "c" "b")
      string<?
      2
      5)
(test #(("a") ("c") ("e"))
      vector-sort
      #(("d") ("f") ("e") ("c") ("a") ("c") ("b"))
      string<?
      2
      5
      #:key car)
(test #(("a") ("c") ("e"))
      vector-sort
      #(("d") ("f") ("e") ("c") ("a") ("c") ("b"))
      string<?
      2
      5
      #:key car
      #:cache-keys? #t)

(let ()
  (define (car< x y) (< (car x) (car y)))
  (define (random-vec n range)
    (build-vector n (λ _ (list (random range)))))
  ;; sort a vector, then sort it with a #:key and with #:cache-keys?
  ;; and make sure they're all doing the same thing
  (define (vector-sort* v [start 0] [end (vector-length v)])
    (let ([v1 (vector-sort v car< start end)]
          [v2 (vector-sort v < start end #:key car)]
          [v3 (vector-sort v < start end #:key car #:cache-keys? #t)])
      (test #t equal? v1 v2)
      (test #t equal? v1 v3)
      v1))
  (define (test-sort len times)
    (or (zero? times)
        ;; build a random vector, sort it, check that it's sorted
        (and (let* ([rand (random-vec len (if (even? times) 1000000 10))]
                    [sorted (vector-sort* rand)])
               (and (= len (vector-length sorted))
                    (or (<= len 1)
                        (let loop ([i 0] [i+1 1])
                          (or (= i+1 len)
                              (and (let ([ival   (car (vector-ref sorted i))]
                                         [i+1val (car (vector-ref sorted i+1))])
                                     (<= ival i+1val))
                                   (loop (add1 i) (add1 i+1))))))))
             (test-sort len (sub1 times)))))
  (test #t test-sort    1  10)
  (test #t test-sort    2  20)
  (test #t test-sort    3  60)
  (test #t test-sort    4 100)
  (test #t test-sort    5 100)
  (test #t test-sort   10 100)
  (test #t test-sort  100 100)
  (test #t test-sort 1000 100)
  ;; test stability for vector-sort
  (test #((1) (2) (3 a) (3 b) (3 c)) vector-sort* #((3 a) (1) (3 b) (2) (3 c)))
  ;; test short lists (+ stable)
  (test #() vector-sort* #())
  (test #((1 1)) vector-sort* #((1 1)))
  (test #((1 2) (1 1)) vector-sort* #((1 2) (1 1)))
  (test #((1) (2)) vector-sort* #((2) (1)))
  (for-each (λ (v) (test #((0 3) (1 1) (1 2)) vector-sort* v))
            '(#((1 1) (1 2) (0 3))
              #((1 1) (0 3) (1 2))
              #((0 3) (1 1) (1 2))))
  (for-each (λ (v) (test #((0 2) (0 3) (1 1)) vector-sort* v))
            '(#((1 1) (0 2) (0 3))
              #((0 2) (1 1) (0 3))
              #((0 2) (0 3) (1 1))))
  ;; exhaustive tests for 2 and 3 item lists
  (for-each (λ (v) (test #((1 x) (2 y)) vector-sort* v))
            '(#((1 x) (2 y))
              #((2 y) (1 x))))
  (for-each (λ (v) (test #((1 x) (2 y) (3 z)) vector-sort* v))
            '(#((1 x) (2 y) (3 z))
              #((2 y) (1 x) (3 z))
              #((2 y) (3 z) (1 x))
              #((3 z) (2 y) (1 x))
              #((3 z) (1 x) (2 y))
              #((1 x) (3 z) (2 y))))
  (test #((2)) vector-sort* #((4) (2) (1) (3)) 1 2)
  (test #((1) (2)) vector-sort* #((4) (2) (1) (3)) 1 3)
  (test #((1) (2) (3)) vector-sort* #((4) (2) (1) (3)) 1 4)
  (test #((1) (2) (4)) vector-sort* #((4) (2) (1) (3)) 0 3)
  (test #((1) (2) (3) (4) (5) (6) (7) (8)) vector-sort*
        #((4) (2) (1) (3) (6) (5) (8) (7) (10) (9))
        0 8)
  (test #((1) (3) (5) (6) (7) (8) (9) (10)) vector-sort*
        #((4) (2) (1) (3) (6) (5) (8) (7) (10) (9))
        2 10)

  (define ((check-regs . regexps) exn)
    (and (exn:fail? exn)
         (andmap (λ (reg) (regexp-match reg (exn-message exn)))
                 regexps)))

  (err/rt-test (vector-sort! (list 1) <) (check-regs #rx"vector-sort!" #rx"vector" #rx"immutable"))
  (err/rt-test (vector-sort! #(1) <) (check-regs #rx"vector-sort!" #rx"vector" #rx"immutable"))
  (err/rt-test (vector-sort (list 1) <) (check-regs #rx"vector-sort" #rx"vector"))
  (err/rt-test (vector-sort! (vector 1) 1) (check-regs #rx"vector-sort!" #rx"any/c any/c . -> . any/c"))
  (err/rt-test (vector-sort (vector 1) 1) (check-regs #rx"vector-sort" #rx"any/c any/c . -> . any/c"))
  (err/rt-test (vector-sort! (vector 1) (λ (x) x)) (check-regs #rx"vector-sort!" #rx"any/c any/c . -> . any/c"))
  (err/rt-test (vector-sort (vector 1) (λ (x) x)) (check-regs #rx"vector-sort" #rx"any/c any/c . -> . any/c"))
  (err/rt-test (vector-sort! (vector 1) < #:key 42) (check-regs #rx"vector-sort!" #rx"any/c . -> . any/c"))
  (err/rt-test (vector-sort! (vector 1) < #:key cons) (check-regs #rx"vector-sort!" #rx"any/c . -> . any/c"))
  (err/rt-test (vector-sort (vector 1) < #:key 42) (check-regs #rx"vector-sort" #rx"any/c . -> . any/c"))
  (err/rt-test (vector-sort (vector 1) < #:key cons) (check-regs #rx"vector-sort" #rx"any/c . -> . any/c")))

;; ---------- vector-sort! actually mutates arg, and vector-sort does not  ----------
;; verify underlying vector is sorted
(let ([v (vector 3 2 1)])
  (vector-sort! v <)
  (test #t
        equal?
        v
        (vector 1 2 3)))
(let ([v (vector "d" "f" "e" "c" "a" "c" "b")])
  (vector-sort! v string<? 2 5)
  (test #t
        equal?
        v
        (vector "d" "f" "a" "c" "e" "c" "b")))
;; verify underlying vector is unchanged
(let ([v (vector 3 2 1)])
  (vector-sort v <)
  (test #t
        equal?
        v
        (vector 3 2 1)))
(let ([v (vector "d" "f" "e" "c" "a" "c" "b")])
  (vector-sort v string<? 2 5)
  (test #t
        equal?
        v
        (vector "d" "f" "e" "c" "a" "c" "b")))

;; test #:key and #:cache-keys?
(let ()
  (define v #((0) (9) (1) (8) (2) (7) (3) (6) (4) (5)))
  (define sorted #((0) (1) (2) (3) (4) (5) (6) (7) (8) (9)))
  (test sorted vector-sort v < #:key car)
  (let ([c1 0] [c2 0] [touched '()])
    (test sorted
          vector-sort v (λ (x y) (set! c1 (add1 c1)) (< x y))
          #:key (λ (x)
                  (set! c2 (add1 c2))
                  (set! touched (cons x touched))
                  (car x)))
    ;; test that the number of key uses is half the number of comparisons
    (test #t = (* 2 c1) c2)
    ;; and that this is larger than the number of items in the list
    (test #t < (vector-length v) c2)
    ;; and that every item was touched
    (test null remove* touched (vector->list v)))
  (let ([c 0] [touched '()])
    ;; now cache the keys
    (test sorted
          vector-sort v <
          #:key (λ (x)
                  (set! c (add1 c))
                  (set! touched (cons x touched))
                  (car x))
          #:cache-keys? #t)
    ;; test that the number of key uses is the same as the list length
    (test #t = c (vector-length v))
    ;; and that every item was touched
    (test null remove* touched (vector->list v)))
  (let* ([c 0] [getkey (λ (x) (set! c (add1 c)) x)])
    ;; either way, we never use the key proc on no arguments
    (test #() vector-sort #() < #:key getkey #:cache-keys? #f)
    (test #() vector-sort #() < #:key getkey #:cache-keys? #t)
    (test #t = c 0)
    ;; we also don't use it for 1-arg lists
    (test #(1) vector-sort #(1) < #:key getkey #:cache-keys? #f)
    (test #t = c 0)
    ;; but we do use it once if caching happens (it's a consistent interface)
    (test #(1) vector-sort #(1) < #:key getkey #:cache-keys? #t)
    (test #t = c 1)
    ;; check a few other short lists
    (test #(1 2) vector-sort #(2 1) < #:key getkey #:cache-keys? #t)
    (test #(1 2 3) vector-sort #(2 3 1) < #:key getkey #:cache-keys? #t)
    (test #(1 2 3 4) vector-sort #(4 2 3 1) < #:key getkey #:cache-keys? #t)
    (test #t = c 10)))


(report-errs)
