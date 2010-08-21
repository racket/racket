(load-relative "loadtest.rktl")

(Section 'flonum)

(require scheme/flonum)

(define (flonum-close? fl1 fl2)
  (<= (flabs (fl- fl1 fl2))
      1e-8))

;; in-flvector tests.
(let ((flv (flvector 1.0 2.0 3.0)))
  (let ((flv-seq (in-flvector flv)))
    (for ((x (in-flvector flv))
          (xseq flv-seq)
          (i (in-naturals)))
      (test (+ i 1.0) 'in-flvector-fast x)
      (test (+ i 1.0) 'in-flvector-sequence xseq))))

;; for/flvector test
(let ((flv (flvector 1.0 2.0 3.0))
      (flv1 (for/flvector ((i (in-range 3))) (+ i 1.0)))
      (flv2 (for/flvector #:length 3 ((i (in-range 3))) (+ i 1.0))))
  (test flv 'for/flvector flv1)
  (test flv 'for/flvector-fast flv2))

;; for*/flvector test
(let ((flv (flvector 0.0 0.0 0.0 0.0 1.0 2.0 0.0 2.0 4.0))
      (flv1 (for*/flvector ((i (in-range 3)) (j (in-range 3))) (exact->inexact (* 1.0 i j))))
      (flv2 (for*/flvector #:length 9 ((i (in-range 3)) (j (in-range 3))) (exact->inexact (* 1.0 i j)))))
  (test flv 'for*/flvector flv1)
  (test flv 'for*/flvector-fast flv2))

;; Test failure when too many iterations
(test #t 'for/vector-too-many-iters 
      (with-handlers ((exn:fail? (lambda (exn) #t)))
        (for/flvector #:length 3 ((i (in-range 4))) (+ i 1.0))))

;; Test for many body expressions
(let* ((flv (flvector 1.0 2.0 3.0))
       (flv2 (for/flvector ((i (in-range 3))) 
               (flvector-set! flv i (+ (flvector-ref flv i) 1.0))
               (flvector-ref flv i)))
       (flv3 (for/flvector #:length 3 ((i (in-range 3)))
               (flvector-set! flv i (+ (flvector-ref flv i) 1.0))
               (flvector-ref flv i))))
  (test (flvector 2.0 3.0 4.0) 'for/flvector-many-body flv2)
  (test (flvector 3.0 4.0 5.0) 'for/flvector-length-many-body flv3))

(report-errs)