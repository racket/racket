#lang racket/base

;; a library for integer interval sets

(require racket/contract/base
         racket/match
         racket/stream
         racket/struct)

(provide well-formed-set?
         (contract-out
          (struct integer-set ((contents well-formed-set?)))
          [make-range
           (->i () ((i exact-integer?) (j (i) (and/c exact-integer? (>=/c i))))
                [res integer-set?])]
          [rename merge union (integer-set? integer-set? . -> . integer-set?)]
          [split (integer-set? integer-set?
                  . -> .
                  (values integer-set? integer-set? integer-set?))]
          [intersect (integer-set? integer-set? . -> . integer-set?)]
          [subtract (integer-set? integer-set? . -> . integer-set?)]
          [symmetric-difference
           (integer-set? integer-set? . -> . integer-set?)]
          [complement (->i ((s integer-set?) (min exact-integer?)
                            (max (min) (and/c exact-integer? (>=/c min))))
                           [res integer-set?])]
          [member? (exact-integer? integer-set? . -> . any)]
          [get-integer (integer-set? . -> . (or/c #f exact-integer?))]
          [rename is-foldr foldr (any/c any/c integer-set? . -> . any)]
          [partition ((listof integer-set?) . -> . (listof integer-set?))]
          [count (integer-set? . -> . natural-number/c)]
          [subset? (integer-set? integer-set? . -> . any)]))

;; test macro, now updated to use submodules
(define-syntax test-block
  (syntax-rules ()
    ((_ defs (code right-ans) ...)
     (module+ test
       (let* defs
         (let ((real-ans code))
           (unless (equal? real-ans right-ans)
             (printf "Test failed: ~e gave ~e.  Expected ~e\n"
                     'code real-ans 'right-ans))) ...)))))

;; An integer-set is (make-integer-set (listof (cons int int)))
;; Each cons represents a range of integers, and the entire
;; set is the union of the ranges.  The ranges must be disjoint and
;; increasing.  Further, adjacent ranges must have at least
;; one number between them.
(define-struct integer-set (contents) #:mutable
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (set) 'integer-set)
      (λ (set) (integer-set-contents set))))]
  #:methods gen:equal+hash
  [(define (equal-proc s1 s2 rec-equal?)
     (rec-equal? (integer-set-contents s1)
                 (integer-set-contents s2)))
   (define (hash-proc set rec-hash)
     (rec-hash (integer-set-contents set)))
   (define (hash2-proc set rec-hash)
     (rec-hash (integer-set-contents set)))]
  #:methods gen:stream
  [(define (stream-empty? set)
     (null? (integer-set-contents set)))
   (define (stream-first set)
     (define contents (integer-set-contents set))
     ;; the contract lets us assume non-null
     (caar contents))
   (define (stream-rest set)
     (define contents (integer-set-contents set))
     (match-define (cons low hi) (car contents))
     (make-integer-set
      (if (= low hi)
          (cdr contents)
          (cons (cons (+ 1 low) hi) (cdr contents)))))])

;; well-formed-set? : X -> bool
(define (well-formed-set? x)
  (let loop ((set x)
             (current-num -inf.0))
    (or
     (null? set)
     (and (pair? set)
          (pair? (car set))
          (exact-integer? (caar set))
          (exact-integer? (cdar set))
          (< (add1 current-num) (caar set))
          (<= (caar set) (cdar set))
          (loop (cdr set) (cdar set))))))

(test-block ()
            ((well-formed-set? '((0 . 4) (7 . 9))) #t)
            ((well-formed-set? '((-1 . 4))) #t)
            ((well-formed-set? '((11 . 10))) #f)
            ((well-formed-set? '((0 . 10) (8 . 12))) #f)
            ((well-formed-set? '((10 . 20) (1 . 2))) #f)
            ((well-formed-set? '((-10 . -20))) #f)
            ((well-formed-set? '((-20 . -10))) #t)
            ((well-formed-set? '((1 . 1))) #t)
            ((well-formed-set? '((1 . 1) (2 . 3))) #f)
            ((well-formed-set? '((1 . 1) (3 . 3))) #t)
            ((well-formed-set? null) #t))

;; make-range : int * int -> integer-set
;; creates a set of integers between i and j.  i <= j
(define make-range
  (case-lambda
    (() (make-integer-set null))
    ((i) (make-integer-set (list (cons i i))))
    ((i j) (make-integer-set (list (cons i j))))))
(test-block ()
            ((integer-set-contents (make-range)) '())
            ((integer-set-contents (make-range 12)) '((12 . 12)))
            ((integer-set-contents (make-range 97 110)) '((97 . 110)))
            ((integer-set-contents (make-range 111 111)) '((111 . 111))))


;; sub-range? : (cons int int) (cons int int) -> bool
;; true iff the interval [(car r1), (cdr r1)] is a subset of
;; [(car r2), (cdr r2)]
(define (sub-range? r1 r2)
  (and (>= (car r1) (car r2))
       (<= (cdr r1) (cdr r2))))

;; overlap? : (cons int int) (cons int int) -> bool
;; true iff the intervals [(car r1), (cdr r1)] and [(car r2), (cdr r2)]
;; have non-empty intersections and (car r1) >= (car r2)
(define (overlap? r1 r2)
  (and (>= (car r1) (car r2))
       (>= (cdr r1) (cdr r2))
       (<= (car r1) (cdr r2))))

;; merge-helper : (listof (cons int int)) (listof (cons int int)) -> (listof (cons int int))
(define (merge-helper s1 s2)
  (cond
    ((null? s2) s1)
    ((null? s1) s2)
    (else
     (let ((r1 (car s1))
           (r2 (car s2)))
       (cond
         ((sub-range? r1 r2) (merge-helper (cdr s1) s2))
         ((sub-range? r2 r1) (merge-helper s1 (cdr s2)))
         ((or (overlap? r1 r2) (= (car r1) (add1 (cdr r2))))
          (merge-helper (cons (cons (car r2) (cdr r1)) (cdr s1)) (cdr s2)))
         ((or (overlap? r2 r1) (= (car r2) (add1 (cdr r1))))
          (merge-helper (cdr s1) (cons (cons (car r1) (cdr r2)) (cdr s2))))
         ((< (car r1) (car r2))
          (cons r1 (merge-helper (cdr s1) s2)))
         (else
          (cons r2 (merge-helper s1 (cdr s2)))))))))

(test-block ()
            ((merge-helper null null) null)
            ((merge-helper null '((1 . 10))) '((1 . 10)))
            ((merge-helper '((1 . 10)) null) '((1 . 10)))
            ;; r1 in r2
            ((merge-helper '((5 . 10)) '((5 . 10))) '((5 . 10)))
            ((merge-helper '((6 . 9)) '((5 . 10))) '((5 . 10)))
            ((merge-helper '((7 . 7)) '((5 . 10))) '((5 . 10)))
            ;; r2 in r1
            ((merge-helper '((5 . 10)) '((5 . 10))) '((5 . 10)))
            ((merge-helper '((5 . 10)) '((6 . 9))) '((5 . 10)))
            ((merge-helper '((5 . 10)) '((7 . 7))) '((5 . 10)))
            ;; r2 and r1 are disjoint
            ((merge-helper '((5 . 10)) '((12 . 14))) '((5 . 10) (12 . 14)))
            ((merge-helper '((12 . 14)) '((5 . 10))) '((5 . 10) (12 . 14)))
            ;; r1 and r1 are adjacent
            ((merge-helper '((5 . 10)) '((11 . 13))) '((5 . 13)))
            ((merge-helper '((11 . 13)) '((5 . 10))) '((5 . 13)))
            ;; r1 and r2 overlap
            ((merge-helper '((5 . 10)) '((7 . 14))) '((5 . 14)))
            ((merge-helper '((7 . 14)) '((5 . 10))) '((5 . 14)))
            ((merge-helper '((5 . 10)) '((10 . 14))) '((5 . 14)))
            ((merge-helper '((7 . 10)) '((5 . 7))) '((5 . 10)))
            ;; with lists
            ((merge-helper '((1 . 1) (3 . 3) (5 . 10) (100 . 200))
                    '((2 . 2) (10 . 12) (300 . 300)))
             '((1 . 3) (5 . 12) (100 . 200) (300 . 300)))
            ((merge-helper '((1 . 1) (3 . 3) (5 . 5) (8 . 8) (10 . 10) (12 . 12))
                    '((2 . 2) (4 . 4) (6 . 7) (9 . 9) (11 . 11)))
             '((1 . 12)))
            ((merge-helper '((2 . 2) (4 . 4) (6 . 7) (9 . 9) (11 . 11))
                    '((1 . 1) (3 . 3) (5 . 5) (8 . 8) (10 . 10) (12 . 12)))
             '((1 . 12))))

;; merge : integer-set integer-set -> integer-set
;; Union of s1 and s2
(define (merge s1 s2)
  (make-integer-set (merge-helper (integer-set-contents s1) (integer-set-contents s2))))

;; split-sub-range : (cons int int) (cons int int) -> char-set
;; (subrange? r1 r2) must hold.
;; returns [(car r2), (cdr r2)] - ([(car r1), (cdr r1)] intersect [(car r2), (cdr r2)]).
(define (split-sub-range r1 r2)
  (let ((r1-car (car r1))
        (r1-cdr (cdr r1))
        (r2-car (car r2))
        (r2-cdr (cdr r2)))
    (cond
      ((and (= r1-car r2-car) (= r1-cdr r2-cdr)) null)
      ((= r1-car r2-car) (list (cons (add1 r1-cdr) r2-cdr)))
      ((= r1-cdr r2-cdr) (list (cons r2-car (sub1 r1-car))))
      (else
       (list (cons r2-car (sub1 r1-car)) (cons (add1 r1-cdr) r2-cdr))))))

(test-block ()
            ((split-sub-range '(1 . 10) '(1 . 10)) '())
            ((split-sub-range '(1 . 5) '(1 . 10)) '((6 . 10)))
            ((split-sub-range '(2 . 10) '(1 . 10)) '((1 . 1)))
            ((split-sub-range '(2 . 5) '(1 . 10)) '((1 . 1) (6 . 10))))

;; split-acc : (listof (cons int int))^5 -> integer-set^3
(define (split-acc s1 s2 i s1-i s2-i)
  (cond
    ((null? s1) (values (make-integer-set (reverse i))
                        (make-integer-set (reverse s1-i))
                        (make-integer-set (reverse (append (reverse s2) s2-i)))))
    ((null? s2) (values (make-integer-set (reverse i))
                        (make-integer-set (reverse (append (reverse s1) s1-i)))
                        (make-integer-set (reverse s2-i))))
    (else
     (let ((r1 (car s1))
           (r2 (car s2)))
       (cond
         ((sub-range? r1 r2)
          (split-acc (cdr s1) (append (split-sub-range r1 r2) (cdr s2))
                     (cons r1 i) s1-i s2-i))
         ((sub-range? r2 r1)
          (split-acc (append (split-sub-range r2 r1) (cdr s1)) (cdr s2)
                     (cons r2 i) s1-i s2-i))
         ((overlap? r1 r2)
          (split-acc (cons (cons (add1 (cdr r2)) (cdr r1)) (cdr s1))
                     (cdr s2)
                     (cons (cons (car r1) (cdr r2)) i)
                     s1-i
                     (cons (cons (car r2) (sub1 (car r1))) s2-i)))
         ((overlap? r2 r1)
          (split-acc (cdr s1)
                     (cons (cons (add1 (cdr r1)) (cdr r2)) (cdr s2))
                     (cons (cons (car r2) (cdr r1)) i)
                     (cons (cons (car r1) (sub1 (car r2)))s1-i )
                     s2-i))
         ((< (car r1) (car r2))
          (split-acc (cdr s1) s2 i (cons r1 s1-i) s2-i))
         (else
          (split-acc s1 (cdr s2) i s1-i (cons r2 s2-i))))))))

;; split : integer-set integer-set -> integer-set integer-set integer-set
;; returns (s1 intersect s2), s1 - (s1 intersect s2) and s2 - (s1 intersect s2)
;; Of course, s1 - (s1 intersect s2) = s1 intersect (complement s2) = s1 - s2
(define (split s1 s2)
  (split-acc (integer-set-contents s1) (integer-set-contents s2) null null null))

;; intersect: integer-set integer-set -> integer-set
(define (intersect s1 s2)
  (let-values (((i s1-s2 s2-s1) (split s1 s2)))
    i))

;; subtract: integer-set integer-set -> integer-set
(define (subtract s1 s2)
  (let-values (((i s1-s2 s2-s1) (split s1 s2)))
    s1-s2))

;; symmetric-difference: integer-set integer-set -> integer-set
(define (symmetric-difference s1 s2)
  (let-values (((i s1-s2 s2-s1) (split s1 s2)))
    (merge s1-s2 s2-s1)))

(test-block ((s (lambda (s1 s2)
                  (map integer-set-contents
                       (call-with-values (lambda () (split (make-integer-set s1)
                                                           (make-integer-set s2))) list)))))
            ((s null null) '(() () ()))
            ((s '((1 . 10)) null) '(() ((1 . 10)) ()))
            ((s null '((1 . 10))) '(() () ((1 . 10))))
            ((s '((1 . 10)) null) '(() ((1 . 10)) ()))
            ((s '((1 . 10)) '((1 . 10))) '(((1 . 10)) () ()))
            ((s '((1 . 10)) '((2 . 5))) '(((2 . 5)) ((1 . 1) (6 . 10)) ()))
            ((s '((2 . 5)) '((1 . 10))) '(((2 . 5)) () ((1 . 1) (6 . 10))))
            ((s '((2 . 5)) '((5 . 10))) '(((5 . 5)) ((2 . 4)) ((6 . 10))))
            ((s '((5 . 10)) '((2 . 5))) '(((5 . 5)) ((6 . 10)) ((2 . 4))))
            ((s '((2 . 10)) '((5 . 14))) '(((5 . 10)) ((2 . 4)) ((11 . 14))))
            ((s '((5 . 14)) '((2 . 10))) '(((5 . 10)) ((11 . 14)) ((2 . 4))))
            ((s '((10 . 20)) '((30 . 50))) '(() ((10 . 20)) ((30 . 50))))
            ((s '((100 . 200)) '((30 . 50))) '(() ((100 . 200)) ((30 . 50))))
            ((s '((1 . 5) (7 . 9) (100 . 200) (500 . 600) (600 . 700))
                '((2 . 8) (50 . 60) (101 . 104) (105 . 220)))
             '(((2 . 5) (7 . 8) (101 . 104) (105 . 200))
               ((1 . 1) (9 . 9) (100 . 100) (500 . 600) (600 . 700))
               ((6 . 6) (50 . 60) (201 . 220))))
            ((s '((2 . 8) (50 . 60) (101 . 104) (105 . 220))
                '((1 . 5) (7 . 9) (100 . 200) (500 . 600) (600 . 700)))
             '(((2 . 5) (7 . 8) (101 . 104) (105 . 200))
               ((6 . 6) (50 . 60) (201 . 220))
               ((1 . 1) (9 . 9) (100 . 100) (500 . 600) (600 . 700))))
            )

;; complement-helper : (listof (cons int int)) int int -> (listof (cons int int))
;; The current-nat accumulator keeps track of where the
;; next range in the complement should start.
(define (complement-helper s min max)
  (cond
    ((null? s) (if (<= min max)
                   (list (cons min max))
                   null))
    (else
     (let ((s-car (car s)))
       (cond
         ((< min (car s-car))
          (cons (cons min (sub1 (car s-car)))
                (complement-helper (cdr s) (add1 (cdr s-car)) max)))
         ((<= min (cdr s-car))
          (complement-helper (cdr s) (add1 (cdr s-car)) max))
         (else
          (complement-helper (cdr s) min max)))))))


;; complement : integer-set int int -> integer-set
;; A set of all the nats not in s and between min and max, inclusive.
;; min <= max
(define (complement s min max)
  (make-integer-set (complement-helper (integer-set-contents s) min max)))

(test-block ((c (lambda (a b c)
                  (integer-set-contents (complement (make-integer-set a) b c)))))
            ((c null 0 255) '((0 . 255)))
            ((c '((1 . 5) (7 . 7) (10 . 200)) 0 255)
             '((0 . 0) (6 . 6) (8 . 9) (201 . 255)))
            ((c '((0 . 254)) 0 255) '((255 . 255)))
            ((c '((1 . 255)) 0 255) '((0 . 0)))
            ((c '((0 . 255)) 0 255) null)
            ((c '((1 . 10)) 2 5) null)
            ((c '((1 . 5) (7 . 12)) 2 8) '((6 . 6)))
            ((c '((1 . 5) (7 . 12)) 6 6) '((6 . 6)))
            ((c '((1 . 5) (7 . 12)) 7 7) '()))

;; member?-helper : int (listof (cons int int)) -> bool
(define (member?-helper i is)
  (and
    (pair? is)
    (or (<= (caar is) i (cdar is))
        (member?-helper i (cdr is)))))

;; member? : int integer-set -> bool
(define (member? i is)
  (member?-helper i (integer-set-contents is)))

(test-block ()
            ((member? 1 (make-integer-set null)) #f)
            ((member? 19 (make-integer-set '((1 . 18) (20 . 21)))) #f)
            ((member? 19 (make-integer-set '((1 . 2) (19 . 19) (20 . 21)))) #t))

;; get-integer : integer-set -> (union int #f)
(define (get-integer is)
  (let ((l (integer-set-contents is)))
    (cond
      ((null? l) #f)
      (else (caar l)))))

(test-block ()
            ((get-integer (make-integer-set null)) #f)
            ((get-integer (make-integer-set '((1 . 2) (5 . 6)))) 1))

;; is-foldr-helper : (int Y -> Y) Y int int (listof (cons int int)) -> Y
(define (is-foldr-helper f base start stop is)
  (cond
    ((and (> start stop) (null? is)) base)
    ((> start stop)
     (is-foldr-helper f base (caar is) (cdar is) (cdr is)))
    (else
     (f start
        (is-foldr-helper f base (add1 start) stop is)))))

;; is-foldr : (int Y -> Y) Y integer-set -> Y
(define (is-foldr f base is)
  (let ((l (integer-set-contents is)))
    (cond
      ((null? l) base)
      (else
       (is-foldr-helper f base (caar l) (cdar l) (cdr l))))))

(test-block ()
            ((is-foldr cons null (make-integer-set null)) null)
            ((is-foldr cons null (make-integer-set '((1 . 2) (5 . 10))))
             '(1 2 5 6 7 8 9 10)))

;; partition : (listof integer-set) -> (listof integer-set)
;; The coarsest refinment r of sets such that the integer-sets in r
;; are pairwise disjoint.
(define (partition sets)
  (map make-integer-set (foldr partition1 null sets)))

;; partition1 : integer-set (listof (listof (cons int int))) -> (listof (listof (cons int int)))
;; All the integer-sets in sets must be pairwise disjoint.  Splits set
;; against each element in sets.
(define (partition1 set sets)
  (let ((set (integer-set-contents set)))
    (cond
      ((null? set) sets)
      ((null? sets) (list set))
      (else
       (let ((set2 (car sets)))
         (let-values (((i s1 s2) (split-acc set set2 null null null)))
           (let ((rest (partition1 s1 (cdr sets)))
                 (i (integer-set-contents i))
                 (s2 (integer-set-contents s2)))
             (cond
               ((null? i)
                (cons s2 rest))
               ((null? s2)
                (cons i rest))
               (else
                (cons i (cons s2 rest)))))))))))

(test-block ((->is (lambda (str)
                     (foldr (lambda (c cs)
                              (merge (make-range (char->integer c))
                                     cs))
                            (make-range)
                            (string->list str))))
             (->is2 (lambda (str)
                      (integer-set-contents (->is str)))))
            ((partition null) null)
            ((map integer-set-contents (partition (list (->is "1234")))) (list (->is2 "1234")))
            ((map integer-set-contents (partition (list (->is "1234") (->is "0235"))))
             (list (->is2 "23") (->is2 "05") (->is2 "14")))
            ((map integer-set-contents (partition (list (->is "12349") (->is "02359") (->is "67") (->is "29"))))
             (list (->is2 "29") (->is2 "67") (->is2 "3") (->is2 "05") (->is2 "14")))
            ((partition1 (->is "bcdjw") null) (list (->is2 "bcdjw")))
            ((partition1 (->is "") null) null)
            ((partition1 (->is "") (list (->is2 "a") (->is2 "b") (->is2 "1")))
             (list (->is2 "a") (->is2 "b") (->is2 "1")))
            ((partition1 (->is "bcdjw")
                         (list (->is2 "z")
                               (->is2 "ab")
                               (->is2 "dj")))
             (list (->is2 "z") (->is2 "b") (->is2 "a") (->is2 "dj") (->is2 "cw"))))

;; count : integer-set -> nat
(define (count s)
  (foldr (lambda (range sum) (+ 1 sum (- (cdr range) (car range))))
         0
         (integer-set-contents s)))

(test-block ()
            ((count (make-integer-set null)) 0)
            ((count (make-integer-set '((1 . 1)))) 1)
            ((count (make-integer-set '((-1 . 10)))) 12)
            ((count (make-integer-set '((-10 . -5) (-1 . 10) (12 . 12)))) 19))

;; subset?-helper : (listof (cons int int)) (listof (cons int int)) -> bool
(define (subset?-helper l1 l2)
  (cond
    ((null? l1) #t)
    ((null? l2) #f)
    (else
     (let ((r1 (car l1))
           (r2 (car l2)))
       (cond
         ((sub-range? r1 r2) (subset?-helper (cdr l1) l2))
         ((<= (car r1) (cdr r2)) #f)
         (else (subset?-helper l1 (cdr l2))))))))

(test-block ()
            ((subset?-helper null null) #t)
            ((subset?-helper null '((1 . 1))) #t)
            ((subset?-helper '((1 . 1)) null) #f)
            ((subset?-helper '((1 . 1)) '((0 . 10))) #t)
            ((subset?-helper '((1 . 1)) '((2 . 10))) #f)
            ((subset?-helper '((-4 . -4) (2 . 10)) '((-20 . -17) (-15 . -10) (-5 . -4) (-2 . 0) (1 . 12))) #t)
            ((subset?-helper '((-4 . -3) (2 . 10)) '((-20 . -17) (-15 . -10) (-5 . -4) (-2 . 0) (1 . 12))) #f)
            ((subset?-helper '((-4 . -4) (2 . 10)) '((-20 . -17) (-15 . -10) (-5 . -4) (-2 . 0) (3 . 12))) #f))

;; subset? : integer-set integer-set -> bool
(define (subset? s1 s2)
  (subset?-helper (integer-set-contents s1) (integer-set-contents s2)))
