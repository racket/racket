#lang scheme/base

(require "sort.ss"
         "vector-util.scm"
         srfi/27)

(define insert-sort!
  ; http://www.math.grin.edu/~stone/events/scheme-workshop/insertion.html
  (lambda (< v)
    (let ((precedes? <)
          (len (vector-length v)))
      (let ((insert!
             (lambda (position)
               (let ((new (vector-ref v position)))
                 (let loop ((trial (- position 1)))
                   (if (negative? trial)          ; at the left end: stop!
                       (vector-set! v 0 new)
                       (let ((displaced (vector-ref v trial)))
                         (if (precedes? new displaced)
                             (begin
                               (vector-set! v (+ trial 1) displaced)
                               (loop (- trial 1)))
                             (vector-set! v (+ trial 1) new)))))))))

        (do ((index 1 (+ index 1)))
            ((<= len index))
          (insert! index))))))

;;; Little test harness, 'cause I'm paraoid about tricky code.
;;; ,open i/o srfi-27 sort list-merge-sort vector-insertion-sort vector-utils 

;;; For testing stable sort -- 3 & -3 compare the same.
(define (my< x y) (< (abs x) (abs y)))

(define (unstable-sort-test v) ; quick & heap vs simple insert
  (let ((v1 (vector-copy v))
	(v2 (vector-copy v)))
    (vector-heap-sort!    < v1)
    (insert-sort!  < v2)
    (and (or (not (equal? v1 v2))
	     (not (vector-sorted? < v1)))
	 (list v v1 v2))))

(define (stable-sort-test v) ; insert, list & vector merge sorts
  (let ((v1 (vector-copy v))
	(v2 (vector-copy v))
	(v3 (list->vector (list-merge-sort! my< (vector->list v))))
	(v4 (list->vector (list-merge-sort  my< (vector->list v)))))
    (vector-merge-sort! my< v1)
    (insert-sort!       my< v2)
    (and (or (not (equal? v1 v2))
	     (not (equal? v1 v3))
	     (not (equal? v1 v4))
	     (not (vector-sorted? my< v1)))
	 (list v v1 v2 v3 v4))))

(define (do-test max-size [max-iterations #f])
  (let lp ((i 0)
           (total-iterations 0))
    (when (or (not max-iterations)
              (< total-iterations max-iterations))
      (let ((i (cond ((= i 1000)
                      (write-char #\.)
                      (flush-output (current-output-port))
                      0)
                     (else (+ i 1))))
            (v (random-vector (random-integer max-size))))
        (cond ((unstable-sort-test v) => (lambda (x) (cons 'u x)))
              ((stable-sort-test   v) => (lambda (x) (cons 's x)))
              (else (lp i (+ total-iterations 1))))))))

(define (random-vector size)
  (let ((v (make-vector size)))
    (fill-vector-randomly! v (* 10 size))
    v))

(define (fill-vector-randomly! v range)
  (let ((half (quotient range 2)))
    (do ((i (- (vector-length v) 1) (- i 1)))
	((< i 0))
      (vector-set! v i (- (random-integer range) half)))))


(do-test 100 10000)
