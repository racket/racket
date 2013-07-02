(load-relative "loadtest.rktl")
(Section 'fixnum)
(require scheme/fixnum
         scheme/unsafe/ops
         "for-util.rkt")

(define 64-bit? (fixnum? (expt 2 33)))

(define (fixnum-width) (if 64-bit? 63 31))
(define (least-fixnum) (if 64-bit? (- (expt 2 62)) -1073741824))
(define (greatest-fixnum) (if 64-bit? (- (expt 2 62) 1) +1073741823))


(define unary-table 
  (list (list fxnot unsafe-fxnot)
        (list fxabs unsafe-fxabs)
        (list fx->fl unsafe-fx->fl)
        (list (lambda (v) (fl->fx (exact->inexact x)))
              (lambda (v) (unsafe-fl->fx (exact->inexact x))))))

(define binary-table
  (list (list fx+ unsafe-fx+)
        (list fx- unsafe-fx-)
        (list fx* unsafe-fx*)

        (list fxquotient unsafe-fxquotient)
        (list fxremainder unsafe-fxremainder)
        (list fxmodulo unsafe-fxmodulo)
        
        (list fxand unsafe-fxand)
        (list fxior unsafe-fxior)
        (list fxxor unsafe-fxxor)

        (list fx>= unsafe-fx>=)
        (list fx> unsafe-fx>)
        (list fx= unsafe-fx=)
        (list fx<= unsafe-fx<=)
        (list fx< unsafe-fx<)
        (list fxmin unsafe-fxmin)
        (list fxmax unsafe-fxmax)))

(define binary/small-second-arg-table
  (list (list fxlshift unsafe-fxlshift)
        (list fxrshift unsafe-fxrshift)))

(define nary-table
  (list))

(define table (append binary/small-second-arg-table binary-table unary-table nary-table))
  
(define (check-arity fx unsafe-fx)
  (let ([same-arities? (λ (x y) (equal? (procedure-arity x)
                                        (procedure-arity y)))])
    (test #t
          same-arities?
          fx
          unsafe-fx)))


;; same-results : (fixnum ... -> any) (fixnum ... -> any) (listof fixnum) -> #t
;; applies fx to args; if it raises an error, the function returns #t.
;;                     if it returns a result, the function applies args
;;                     to unsafe-fx and makes sure the results are either eq? or
;;                     (if the results are flonums), =
;; raises an exception when it finds a bug.
(define (same-results fx unsafe-fx args) 
  (let/ec k
    (let* ([fx-result (with-handlers ((exn:fail? (λ (x) (k #t))))
                        (apply fx args))]
           [unsafe-result (apply unsafe-fx args)]
           [ans
            (or (eq? fx-result unsafe-result)
                (and (flonum? fx-result)
                     (flonum? unsafe-result)
                     (= fx-result unsafe-result)))])
      (unless ans
        (newline)
        (error 'same-results "better die now, rather than continue, what with unsafe ops around:\n     fx-result ~s\n unsafe-result ~s"
               fx-result
               unsafe-result))
      #t)))

(define (flonum? x) (inexact-real? x))

(define (same-results/range/table)
  (for ([line (in-list unary-table)])
    (for ([i (in-range (- (expt 2 8)) (expt 2 8))])
      (test #t same-results (list-ref line 0) (list-ref line 1) (list i))))
  
  (for ([line (in-list (append binary/small-second-arg-table 
                               binary-table
                               nary-table))])
    (for ([i (in-range (- (expt 2 4)) (expt 2 4))])
      (for ([j (in-range (- (expt 2 4)) (expt 2 4))])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i j))))))

(define (same-results/extremum)
  (let ([interesting-values (list (least-fixnum) -1 0 1 (greatest-fixnum))])
    (for ([line (in-list unary-table)])
      (for ([i (in-list interesting-values)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i))))
    
    (for ([line (in-list (append binary/small-second-arg-table
                                 binary-table
                                 nary-table))])
      (for ([i (in-list interesting-values)])
        (for ([j (in-list interesting-values)])
          (test #t same-results (list-ref line 0) (list-ref line 1) (list i j)))))))

(define (same-results/random/table)
  (for ([ignore (in-range 0 800)])
    (let ([i (random-fixnum)]
          [j (random-fixnum)]
          [k (inexact->exact (floor (* (random) (+ 1 (fixnum-width)))))]
          [more-fixnums (build-list (random 20) (λ (i) (random-fixnum)))])
      (for ([line (in-list unary-table)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i)))
      (for ([line (in-list binary-table)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i j)))
      (for ([line (in-list binary/small-second-arg-table)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i k)))
      (for ([line (in-list nary-table)])
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i))
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i j))
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i j k))
        (test #t same-results (list-ref line 0) (list-ref line 1) (list i k j))
        (test #t same-results (list-ref line 0) (list-ref line 1) more-fixnums)))))

(define (random-fixnum)
  (inexact->exact (floor (+ (least-fixnum) (* (random) (+ (- (greatest-fixnum) (least-fixnum)) 1))))))

;; check the arities
(for-each (λ (x) (apply check-arity x)) table)

;; check the extreme values (against themselves and few other values)
(same-results/extremum)

;; check randomly
(same-results/random/table)

;; check a small range
(same-results/range/table)

;; ----------------------------------------

;; in-fxvector tests.
(let ((flv (fxvector 1 2 3)))
  (let ((flv-seq (in-fxvector flv)))
    (for ((x (in-fxvector flv))
          (xseq flv-seq)
          (i (in-naturals)))
      (test (+ i 1) 'in-fxvector-fast x)
      (test (+ i 1) 'in-fxvector-sequence xseq))))

;; for/fxvector test
(let ((flv (fxvector 1 2 3))
      (flv1 (for/fxvector ((i (in-range 3))) (+ i 1)))
      (flv2 (for/fxvector #:length 3 ((i (in-range 3))) (+ i 1))))
  (test flv 'for/fxvector flv1)
  (test flv 'for/fxvector-fast flv2))

;; for*/fxvector test
(let ((flv (fxvector 0 0 0 0 1 2 0 2 4))
      (flv1 (for*/fxvector ((i (in-range 3)) (j (in-range 3))) (* 1 i j)))
      (flv2 (for*/fxvector #:length 9 ((i (in-range 3)) (j (in-range 3))) (* 1 i j))))
  (test flv 'for*/fxvector flv1)
  (test flv 'for*/fxvector-fast flv2))

;; Test for both length too long and length too short
(let ((v (make-fxvector 3)))
  (fxvector-set! v 0 0)
  (fxvector-set! v 1 1)
  (let ((w (for/fxvector #:length 3 ((i (in-range 2))) i)))
    (test v 'for/fxvector-short-iter w)))

(let ((v (make-fxvector 10)))
  (for* ((i (in-range 3))
         (j (in-range 3)))
    (fxvector-set! v (+ j (* i 3)) (+ 1 i j)))
  (let ((w (for*/fxvector #:length 10 ((i (in-range 3)) (j (in-range 3))) (+ 1 i j))))
    (test v 'for*/fxvector-short-iter w)))

(test 2 'for/fxvector-long-iter
      (fxvector-length (for/fxvector #:length 2 ((i (in-range 10))) i)))
(test 5 'for*/fxvector-long-iter 
      (fxvector-length (for*/fxvector #:length 5 ((i (in-range 3)) (j (in-range 3))) (+ i j))))

;; Test for many body expressions
(let* ((flv (fxvector 1 2 3))
       (flv2 (for/fxvector ((i (in-range 3))) 
               (fxvector-set! flv i (+ (fxvector-ref flv i) 1))
               (fxvector-ref flv i)))
       (flv3 (for/fxvector #:length 3 ((i (in-range 3)))
               (fxvector-set! flv i (+ (fxvector-ref flv i) 1))
               (fxvector-ref flv i))))
  (test (fxvector 2 3 4) 'for/fxvector-many-body flv2)
  (test (fxvector 3 4 5) 'for/fxvector-length-many-body flv3))

;; fxvector-copy test
(let ((v (fxvector 0 1 2 3)))
  (let ((vc (fxvector-copy v)))
    (test (fxvector-length v) 'fxvector-copy (fxvector-length vc))
    (for ((vx (in-fxvector v))
          (vcx (in-fxvector vc)))
      (test vx 'fxvector-copy vcx))
    (fxvector-set! vc 2 -10)
    (test 2 'fxvector-copy (fxvector-ref v 2))
    (test -10 'fxvector-copy (fxvector-ref vc 2))
    (test '(2 3) 'fxvector-copy (for/list ([i (in-fxvector (fxvector-copy v 2))]) i))
    (test '(2) 'fxvector-copy (for/list ([i (in-fxvector (fxvector-copy v 2 3))]) i))))

;; ----------------------------------------

;; in-flvector tests, copied from for.rktl

(test-sequence [(1 2 3)] (in-fxvector (fxvector 1 2 3)))
(test-sequence [(2 3 4)] (in-fxvector (fxvector 1 2 3 4) 1))
(test-sequence [(2 3 4)] (in-fxvector (fxvector 1 2 3 4 5) 1 4))
(test-sequence [(2 4 6)] (in-fxvector (fxvector 1 2 3 4 5 6 7 8) 1 7 2))
(test-sequence [(8 6 4)] (in-fxvector (fxvector 1 2 3 4 5 6 7 8) 7 1 -2))
(test-sequence [(2 4 6)] (in-fxvector (fxvector 1 2 3 4 5 6 7 8) 1 6 2))
(test-sequence [(8 6 4)] (in-fxvector (fxvector 1 2 3 4 5 6 7 8) 7 2 -2))

;; Check safety:
(err/rt-test (for/fxvector ([i 5]) 8.0))
(err/rt-test (for/fxvector #:length 5 ([i 5]) 8.0))
(err/rt-test (for/fxvector #:length 5 #:fill 0 ([i 5]) 8.0))
(err/rt-test (for/fxvector #:length 5 #:fill 0.0 ([i 5]) 8))
(err/rt-test (for/fxvector #:length 10 #:fill 0.0 ([i 5]) 8))


(report-errs)
