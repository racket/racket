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
                  (k : True #(#t #t))
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
