#lang typed/scheme

(require
 scheme/flonum
 scheme/unsafe/ops)

(: check (All (a) ((a a -> Boolean) a a -> Boolean)))
;; Simple check function as RackUnit doesn't work in Typed Scheme (yet)
(define (check f a b)
  (if (f a b)
      #t
      (error (format "Check (~a ~a ~a) failed" f a b))))

(: check-pred (All (a) ((a -> Boolean) a -> Boolean)))
(define (check-pred pred v)
  (if (pred v)
      #t
      (error (format "Check predicate (~a ~a) failed" pred v))))

(: true? (Any -> Boolean))
(define (true? x)
  (if x #t #f))

;; Check that flonum (safe and unsafe) functions work as expected

(check = (flabs 1.45) (unsafe-flabs 1.45))
(check = (fl+ 1.45 2.36) (unsafe-fl+ 1.45 2.36))
(check = (fl- 1.45 2.36) (unsafe-fl- 1.45 2.36))
(check = (fl* 1.45 2.36) (unsafe-fl* 1.45 2.36))
(check = (fl/ 1.45 2.36) (unsafe-fl/ 1.45 2.36))
(check-pred true? (fl= 1.45 1.45))
(check-pred true? (fl<= 1.45 1.45))
(check-pred true? (fl>= 1.45 1.45))
(check-pred true? (fl> 1.45 1.36))
(check-pred true? (fl< 1.36 1.45))
(check-pred true? (unsafe-fl= 1.45 1.45))
(check-pred true? (unsafe-fl<= 1.45 1.45))
(check-pred true? (unsafe-fl>= 1.45 1.45))
(check-pred true? (unsafe-fl> 1.45 1.36))
(check-pred true? (unsafe-fl< 1.36 1.45))
(check = (flmin 1.45 2.36) (unsafe-flmin 1.45 2.36))
(check = (flmax 1.45 2.36) (unsafe-flmax 1.45 2.36))
(check = (flround 1.45) (unsafe-flround 1.45))
(check = (flfloor 1.45) (unsafe-flfloor 1.45))
(check = (flceiling 1.45) (unsafe-flceiling 1.45))
(check = (fltruncate 1.45) (unsafe-fltruncate 1.45))
(check = (flsin 1.45) (unsafe-flsin 1.45))
(check = (flcos 1.45) (unsafe-flcos 1.45))
(check = (fltan 1.45) (unsafe-fltan 1.45))
(check = (flatan 1.45) (unsafe-flatan 1.45))
(check = (flasin .45) (unsafe-flasin .45))
(check = (flacos .45) (unsafe-flacos .45))
(check = (fllog 1.45) (unsafe-fllog 1.45))
(check = (flexp 1.45) (unsafe-flexp 1.45))
(check = (flsqrt 1.45) (unsafe-flsqrt 1.45))
(check = (->fl 1) 1.0)
(check = (unsafe-fx->fl 1) 1.0)
