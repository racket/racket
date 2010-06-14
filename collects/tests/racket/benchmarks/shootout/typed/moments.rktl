;; Moments.scm

(require (only-in mzlib/list sort)
         (only-in mzlib/string real->decimal-string)
         racket/flonum)

(: to-str (Number -> String))
(define (to-str n) (real->decimal-string n 6))

(let*: ((sum : Float 0.0)
        (numlist : (Listof Float)
                 (let: loop : (Listof Float)
                       ((line : String (read-line))
                        (numlist : (Listof Float) '()))
                  (cond ((eof-object? line) numlist)
                        (else
                         (let ((num (exact->inexact (assert (string->number line) real?))))
                           (set! sum (+ num sum))
                           (loop (read-line) (cons num numlist))))))))
  (unless (null? numlist)
    (let ((n (length numlist)))
      (let: ((mean : Float (/ sum (exact->inexact n)))
             (average_deviation : Float 0.0)
             (standard_deviation : Float 0.0)
             (variance : Float 0.0)
             (skew : Float 0.0)
             (kurtosis : Float 0.0)
             (median : Float 0.0)
             (deviation : Float 0.0))
        (let loop ((nums numlist))
          (if (not (null? nums))
              (begin
                (set! deviation (- (car nums) mean))
                (set! average_deviation (+ average_deviation (abs deviation)))
                (set! variance (+ variance (expt deviation 2)))
                (set! skew (+ skew (expt deviation 3)))
                (set! kurtosis (+ kurtosis (expt deviation 4)))
                (loop (cdr nums)))
              #t))

        (set! average_deviation (/ average_deviation (exact->inexact n)))
        (set! variance (/ variance (- n 1)))
        (set! standard_deviation (flsqrt variance))

        (cond ((> variance 0.0)
               (set! skew (exact->inexact (/ skew (* n variance standard_deviation))))
               (set! kurtosis (- (/ kurtosis (* n variance variance))
                                 3.0))))

        (set! numlist ((inst sort Float Float) numlist (lambda: ((x : Float) (y : Float))
                                                                (< x y))))

        (let ((mid (quotient n 2)))
          (if (zero? (modulo n 2))
              (set! median (/ (+ (car (list-tail numlist mid))
                                 (car (list-tail numlist (assert (- mid 1) exact-nonnegative-integer?))))
                              2.0))
              (set! median (car (list-tail numlist mid)))))


        (set! standard_deviation (exact->inexact (/ (round (* standard_deviation 1000000))
                                                 1000000)))

        (for-each display
                  `("n:                  " ,n                   "\n"
                    "median:             " ,(to-str median)  "\n"
                    "mean:               " ,(to-str mean)    "\n"
                    "average_deviation:  " ,(to-str average_deviation ) "\n"
                    "standard_deviation: " ,(to-str standard_deviation) "\n"
                    "variance:           " ,(to-str variance)"\n"
                    "skew:               " ,(to-str skew)    "\n"
                    "kurtosis:           " ,(to-str kurtosis)"\n" ))))))
