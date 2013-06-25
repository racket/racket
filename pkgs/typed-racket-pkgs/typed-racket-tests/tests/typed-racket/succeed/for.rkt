#lang typed/scheme

(: check (All (a) ((a a -> Boolean) a a -> Boolean)))
;; Simple check function as RackUnit doesn't work in Typed Scheme (yet)
(define (check f a b)
  (if (f a b)
      #t
      (error (format "Check (~a ~a ~a) failed" f a b))))

(check string=?
       (with-output-to-string
         (lambda ()
           (for: : Void ([i : Integer (in-range 10)])
                 (display i))))
       "0123456789")

(check string=?
       (with-output-to-string
         (lambda ()
           (for: : Void
                 ((i : Integer '(1 2 3))
                  (j : Char "abc")
                  #:when (odd? i)
                  (k : Boolean #(#t #t))
                  #:when k)
                 (display (list i j k)))))
       "(1 a #t)(1 a #t)(3 c #t)(3 c #t)")

(check equal?
       (for/list: : (Listof Integer) ([i : Integer (in-range 10)]) i)
       '(0 1 2 3 4 5 6 7 8 9))

(check equal?
       (for/list: : (Listof Integer)
                  ((i : Integer '(1 2 3))
                   (j : Integer '(10 20 30))
                   #:when (odd? i))
                  (+ i j 10))
       '(21 43))
(check equal?
       (for/list: : (Listof Integer)
                  ((i : Integer '(1 2 3))
                   (j : Integer '(10 20 30))
                   #:unless (odd? i))
                  (+ i j 10))
       '(32))

(check equal?
       (for/or: : Boolean
                ((i : Integer '(1 2 3)))
                (>= i 3))
       #t)

(check equal?
       (for/or: : Boolean
                ((i : Integer '(1 2 3))
                 (j : Integer '(2 1 3)))
                (>= i j))
       #t)

(check equal?
       (let-values: ([([x : (Listof Integer)] [y : (Listof Integer)])
                      (for/lists: : (values (Listof Integer) (Listof Integer))
                                  ((x : (Listof Integer))
                                   (y : (Listof Integer)))
                                  ((i : Integer '(1 2 3))
                                   #:when #t
                                   (j : Integer '(10 20 30))
                                   #:when (> j 12))
                                  (values i j))])
                    (append x y))
       '(1 1 2 2 3 3 20 30 20 30 20 30))

(check =
       (for/fold: : Integer
                  ((acc : Integer 0))
                  ((i : Integer '(1 2 3))
                   (j : Integer '(10 20 30)))
                  (+ acc i j))
       66)

(check =
       (for/fold: : Integer
                  ((acc : Integer 0))
                  ((i : Integer '(1 2 3))
                   #:when (even? i)
                   (j : Integer '(10 20 30))
                   #:when #t
                   (k : Integer '(100 200 300)))
                  (+ acc i j k))
       1998)

(check string=?
       (with-output-to-string
         (lambda ()
           (for*: : Void
                  ((i : Integer '(1 2 3))
                   (j : Integer '(10 20 30)))
                  (display (list i j)))))
       "(1 10)(1 20)(1 30)(2 10)(2 20)(2 30)(3 10)(3 20)(3 30)")

(check equal?
       (let-values: ([([x : (Listof Integer)] [y : (Listof Integer)])
                      (for*/lists: : (values (Listof Integer) (Listof Integer))
                                   ((x : (Listof Integer))
                                    (y : (Listof Integer)))
                                   ((i : Integer '(1 2 3))
                                    (j : Integer '(10 20 30))
                                    #:when (> j 12))
                                   (values i j))])
                    (append x y))
       '(1 1 2 2 3 3 20 30 20 30 20 30))

(check =
       (for*/fold: : Integer
                   ((acc : Integer 0))
                   ((i : Integer '(1 2 3))
                    #:when (even? i)
                    (j : Integer '(10 20 30))
                    (k : Integer '(100 200 300)))
                   (+ acc i j k))
       1998)
(check =
       (for*/fold: : Integer
                   ((acc : Integer 0))
                   ((i : Integer '(1 2 3))
                    #:unless (even? i)
                    (j : Integer '(10 20 30))
                    (k : Integer '(100 200 300)))
                   (+ acc i j k))
       3996)

(check =
       (for/sum: : Integer
                 ([i : Integer (in-range 10)])
                 i)
       45)
(check =
       (for/sum: : Integer
                 ([i : Integer (in-range 10)]
                  [j : Integer (in-range 10)])
                 (+ i j))
       90)

(check =
       (for/product: : Integer
                     ([i : Integer (in-range 10)])
                     i)
       0)
(check =
       (for/product: : Integer
                     ([i : Integer (in-range 1 10)])
                     i)
       362880)
(check =
       (for/product: : Integer
                     ([i : Integer (in-range 1 10)]
                      [j : Integer (in-range 1 10)])
                     (+ i j))
       185794560)

;; for/product: had problems with Real due to an unannotated accumulator
(check =
       (for/product: : Real
                     ([i (in-list (list 1.2 -1.0 0.5))])
         i)
       -0.6)

;; multiclause versions of these don't currently work properly
(check =
       (for*/sum: : Integer
                  ([i : Integer (in-range 10)])
                  i)
       45)

(check =
       (for*/product: : Integer
                      ([i : Integer (in-range 10)])
                      i)
       0)
(check =
       (for*/product: : Integer
                      ([i : Integer (in-range 1 10)])
                      i)
       362880)


;; Integers as sequences.
(check =
       (for/sum: : Integer
                 ([i : Byte 4])
                 i)
       6)
(check =
       (for/sum: : Integer
                 ([i : Index (ann 4 Index)])
                 i)
       6)
(check =
       (for/sum: : Integer
                 ([i : Nonnegative-Fixnum (ann 4 Fixnum)])
                 i)
       6)
(check =
       (for/sum: : Integer
                 ([i : Natural (ann 4 Integer)])
                 i)
       6)

(check string=?
       (with-output-to-string
         (lambda ()
           (for: ([x 10] #:unless (> x 3)) (display x))))
       "0123")

(check equal?
       (for/hasheq: : (HashTable Integer String) ([k (list 2 3 4)]) (values k "val"))
       #hasheq((2 . "val") (3 . "val") (4 . "val")))

(check equal?
       (for/vector: ([i : Natural (in-range 3)]) 5)
       (vector 5 5 5))

(check equal?
       (for/vector: : (Vectorof Number) ([i : Natural (in-range 3)]) 5)
       (vector 5 5 5))


(check equal?
       (for/list: : (Listof Natural)
                  ((i : Natural (and (in-naturals)))
                   (j : Natural (and (in-range 5))))
             (+ i j))
       (list 0 2 4 6 8))

;; break and final clauses
;; TODO typechecker can't handle these
;; (check string=?
;;        (with-output-to-string
;;          (lambda ()
;;            (for: ([x 10] #:break (> x 3)) (display x))))
;;        "0123")
;; (check string=?
;;        (with-output-to-string
;;          (lambda ()
;;            (for: ([x 10]) #:break (> x 3) (display x))))
;;        "0123")
;; (check =
;;        (for/sum: : Integer ([x : Integer 10] #:break (> x 3)) (ann x Integer))
;;        6)
;; (check =
;;        (for/sum: : Integer ([x 10] #:final (> x 3)) x)
;;        10)
