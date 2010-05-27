;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         triangle.sch
; Description:  TRIANGLE benchmark
; Author:       Richard Gabriel
; Created:      12-Apr-85
; Modified:     12-Apr-85 10:30:32 (Bob Shaw)
;               11-Aug-87 (Will Clinger)
;               22-Jan-88 (Will Clinger)
;               10-May-10 (Vincent St-Amour)
; Language:     Typed Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; TRIANG -- Board game benchmark.

(: *board* (Vectorof Integer))
(define *board* (make-vector 16 1))
(: *sequence* (Vectorof Integer))
(define *sequence* (make-vector 14 0))
(: *a* (Vectorof Integer))
(define *a* (make-vector 37))
(: *b* (Vectorof Integer))
(define *b* (make-vector 37))
(: *c* (Vectorof Integer))
(define *c* (make-vector 37))
(: *answer* (Listof (Listof Integer)))
(define *answer* '())
(: *final* (Listof Integer))
(define *final* '())

(: last-position ( -> Integer))
(define (last-position)
  (do ((i 1 (+ i 1)))
      ((or (= i 16) (= 1 (vector-ref *board* i)))
       (if (= i 16) 0 i))))

(: ttry (Integer Integer -> Any))
(define (ttry i depth)
  (cond ((= depth 14)
         (let ((lp (last-position)))
           (if (not (member lp *final*))
             (set! *final* (cons lp *final*))
             #t))
         (set! *answer*
               (cons (cdr (vector->list *sequence*)) *answer*))
         #t)
        ((and (= 1 (vector-ref *board* (vector-ref *a* i)))
              (= 1 (vector-ref *board* (vector-ref *b* i)))
              (= 0 (vector-ref *board* (vector-ref *c* i))))
         (vector-set! *board* (vector-ref *a* i) 0)
         (vector-set! *board* (vector-ref *b* i) 0)
         (vector-set! *board* (vector-ref *c* i) 1)
         (vector-set! *sequence* depth i)
         (do ((j 0 (+ j 1))
              (depth (+ depth 1)))
             ((or (= j 36) (ttry j depth)) #f))
         (vector-set! *board* (vector-ref *a* i) 1)
         (vector-set! *board* (vector-ref *b* i) 1)
         (vector-set! *board* (vector-ref *c* i) 0) '())
        (else #f)))

(: gogogo (Integer -> Any))
(define (gogogo i)
  (let ((*answer* '())
        (*final* '()))
    (ttry i 1)))
 
(for-each (lambda: ((i : Integer) (x : Integer)) (vector-set! *a* i x))
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
            21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36)
          '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
            13 7 8 4 4 7 11 8 12 13 6 10
            15 9 14 13 13 14 15 9 10
             6 6))
(for-each (lambda: ((i : Integer) (x : Integer)) (vector-set! *b* i x))
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
            21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36)
          '(2 4 7 5 8 9 3 6 10 5 9 8
            12 13 14 8 9 5 2 4 7 5 8
            9 3 6 10 5 9 8 12 13 14
            8 9 5 5))
(for-each (lambda: ((i : Integer) (x : Integer)) (vector-set! *c* i x))
          '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
            21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36)
          '(4 7 11 8 12 13 6 10 15 9 14 13
            13 14 15 9 10 6 1 2 4 3 5 6 1
            3 6 2 5 4 11 12 13 7 8 4 4))
(vector-set! *board* 5 0)
 
;;; call:  (gogogo 22))
 
(time (let: loop : 'done ((n : Integer 1000000))
        (if (zero? n)
            'done
            (begin
              (gogogo 22)
              (loop (- n 1))))))
