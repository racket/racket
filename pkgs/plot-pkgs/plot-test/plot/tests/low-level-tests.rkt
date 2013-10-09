#lang racket

(require rackunit racket/date
         plot plot/utils
         plot/private/common/utils
         (only-in plot/private/common/math
                  vector-andmap
                  vector-ormap)
         (only-in plot/private/common/date-time
                  utc-seconds-round-year
                  utc-seconds-round-month
                  seconds-per-minute
                  seconds-per-hour
                  seconds-per-day
                  seconds-per-week)
         (only-in plot/private/common/format
                  int-str->e-str frac-str->e-str)
         plot/private/common/worker-thread)

(check-equal? (linear-seq 1 1 2) '(1 1))
(check-equal? (linear-seq 0 1 2 #:start? #t #:end? #t) '(0 1))
(check-equal? (linear-seq 0 1 2 #:start? #t #:end? #f) '(0 2/3))
(check-equal? (linear-seq 0 1 2 #:start? #f #:end? #t) '(1/3 1))
(check-equal? (linear-seq 0 1 2 #:start? #f #:end? #f) '(1/4 3/4))
(check-equal? (linear-seq 1 0 2 #:start? #t #:end? #t) '(1 0))
(check-equal? (linear-seq 1 0 2 #:start? #t #:end? #f) '(1 1/3))
(check-equal? (linear-seq 1 0 2 #:start? #f #:end? #t) '(2/3 0))
(check-equal? (linear-seq 1 0 2 #:start? #f #:end? #f) '(3/4 1/4))

(check-exn exn:fail:contract?
           (λ () (vector-field (λ (v [z 0]) v) -4 4 -4 4))
           "Exception should be 'two of the clauses in the or/c might both match' or similar")

;; ===================================================================================================
;; Formatting

(check-equal? (int-str->e-str "") "0")
(check-equal? (int-str->e-str "0") "0")
(check-equal? (int-str->e-str "10") "1×10\u00b9")

(check-equal? (frac-str->e-str "") "0")
(check-equal? (frac-str->e-str "0") "0")
(check-equal? (frac-str->e-str "00") "0")
(check-equal? (frac-str->e-str "1") "1×10\u207b\u00b9")
(check-equal? (frac-str->e-str "01") "1×10\u207b\u00b2")

;; ===================================================================================================
;; Date rounding

(check-equal? (utc-seconds-round-year (find-seconds 0 0 12 2 7 1970 #f))
              (find-seconds 0 0 0 1 1 1970 #f))
(check-equal? (utc-seconds-round-year (find-seconds 0 0 13 2 7 1970 #f))
              (find-seconds 0 0 0 1 1 1971 #f))
;; A leap year's middle is a half day earlier on the calendar:
(check-equal? (utc-seconds-round-year (find-seconds 0 0 0 2 7 1976 #f))
              (find-seconds 0 0 0 1 1 1976 #f))
(check-equal? (utc-seconds-round-year (find-seconds 0 0 1 2 7 1976 #f))
              (find-seconds 0 0 0 1 1 1977 #f))

(check-equal? (utc-seconds-round-month (find-seconds 0 0 0 16 1 2010 #f))
              (find-seconds 0 0 0 1 1 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 0 17 1 2010 #f))
              (find-seconds 0 0 0 1 2 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 12 16 1 2010 #f))
              (find-seconds 0 0 0 1 1 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 13 16 1 2010 #f))
              (find-seconds 0 0 0 1 2 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 0 16 12 2010 #f))
              (find-seconds 0 0 0 1 12 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 0 17 12 2010 #f))
              (find-seconds 0 0 0 1 1 2011 #f))

;; ===================================================================================================
;; Time conversion

(check-equal? (seconds->plot-time 0)                (plot-time 0 0 0 0))
(check-equal? (seconds->plot-time #e59.999999)      (plot-time #e59.999999 0 0 0))
(check-equal? (seconds->plot-time 60)               (plot-time 0 1 0 0))
(check-equal? (seconds->plot-time #e60.000001)      (plot-time #e0.000001 1 0 0))
(check-equal? (seconds->plot-time #e119.999999)     (plot-time #e59.999999 1 0 0))
(check-equal? (seconds->plot-time 120)              (plot-time 0 2 0 0))
(check-equal? (seconds->plot-time #e120.000001)     (plot-time #e0.000001 2 0 0))
(check-equal? (seconds->plot-time 3599)             (plot-time 59 59 0 0))
(check-equal? (seconds->plot-time 3600)             (plot-time 0 0 1 0))
(check-equal? (seconds->plot-time 3601)             (plot-time 1 0 1 0))
(check-equal? (seconds->plot-time (- seconds-per-day 1))         (plot-time 59 59 23 0))
(check-equal? (seconds->plot-time seconds-per-day)               (plot-time 0 0 0 1))
(check-equal? (seconds->plot-time (- seconds-per-day))           (plot-time 0 0 0 -1))
(check-equal? (seconds->plot-time (- (- seconds-per-day) 1))     (plot-time 59 59 23 -2))

(define sec-secs (sequence->list (in-range -60 61 #e0.571123)))
(define min-secs (sequence->list (in-range (- seconds-per-hour) (+ seconds-per-hour 1)
                                           (* #e0.571123 seconds-per-minute))))
(define hour-secs (sequence->list (in-range (- seconds-per-day) (+ seconds-per-day 1)
                                            (* #e0.571123 seconds-per-hour))))
(define day-secs (sequence->list (in-range (- seconds-per-week) (+ seconds-per-week 1)
                                           (* #e0.571123 seconds-per-day))))
(check-equal? (map (compose plot-time->seconds seconds->plot-time) sec-secs) sec-secs)
(check-equal? (map (compose plot-time->seconds seconds->plot-time) min-secs) min-secs)
(check-equal? (map (compose plot-time->seconds seconds->plot-time) hour-secs) hour-secs)
(check-equal? (map (compose plot-time->seconds seconds->plot-time) day-secs) day-secs)

;; ===================================================================================================
;; Intervals

(check-false (ivl-rational? (ivl #f #f)))
(check-false (ivl-rational? (ivl +nan.0 +nan.0)))

(check-true (ivl-empty? (ivl-meet empty-ivl (ivl 0 3))))

;;; ivl-meet (similar to an intersection)

;; All specified
(check-true (ivl-empty? (ivl-meet (ivl 0 1) (ivl 2 3))))
(check-equal? (ivl-meet (ivl 0 2) (ivl 1 3)) (ivl 1 2))
(check-equal? (ivl-meet (ivl 0 3) (ivl 1 2)) (ivl 1 2))

;; One not specified
;; <--- 1    2 -- 3
(check-true (ivl-empty? (ivl-meet (ivl #f 1) (ivl 2 3))))
;; 0 -- 1    2 --->
(check-true (ivl-empty? (ivl-meet (ivl 0 1) (ivl 2 #f))))
;; <-------- 2
;;      1 ------- 3
(check-equal? (ivl-meet (ivl #f 2) (ivl 1 3)) (ivl 1 2))
;;           2 --->
;; 0 ------------ 3
(check-equal? (ivl-meet (ivl 2 #f) (ivl 0 3)) (ivl 2 3))
;; <------------- 3
;;      1 -- 2
(check-equal? (ivl-meet (ivl #f 3) (ivl 1 2)) (ivl 1 2))
;; 0 ------------->
;;      1 -- 2
(check-equal? (ivl-meet (ivl 0 #f) (ivl 1 2)) (ivl 1 2))

;; Two not specified
;; <--- 1    2 --->
(check-true (ivl-empty? (ivl-meet (ivl #f 1) (ivl 2 #f))))
;; <-------------->
;;      1 -- 2
(check-equal? (ivl-meet (ivl #f #f) (ivl 1 2)) (ivl 1 2))
;;      1 -------->
;; <-------- 2
(check-equal? (ivl-meet (ivl 1 #f) (ivl #f 2)) (ivl 1 2))
;; <-------- 2
;; <------------- 3
(check-equal? (ivl-meet (ivl #f 2) (ivl #f 3)) (ivl #f 2))
;; 0 ------------->
;;      1 -------->
(check-equal? (ivl-meet (ivl 0 #f) (ivl 1 #f)) (ivl 1 #f))

;; Three not specified
;; <-------------->
;;      1 -------->
(check-equal? (ivl-meet (ivl #f #f) (ivl 1 #f)) (ivl 1 #f))
;; <-------------->
;; <-------- 2
(check-equal? (ivl-meet (ivl #f #f) (ivl #f 2)) (ivl #f 2))

;; Four not specified
(check-equal? (ivl-meet (ivl #f #f) (ivl #f #f)) (ivl #f #f))

;; One infinite
;; <--- 1    2 -- 3
(check-true (ivl-empty? (ivl-meet (ivl -inf.0 1) (ivl 2 3))))
;; 0 -- 1    2 --->
(check-true (ivl-empty? (ivl-meet (ivl 0 1) (ivl 2 +inf.0))))
;; <-------- 2
;;      1 ------- 3
(check-equal? (ivl-meet (ivl -inf.0 2) (ivl 1 3)) (ivl 1 2))
;;           2 --->
;; 0 ------------ 3
(check-equal? (ivl-meet (ivl 2 +inf.0) (ivl 0 3)) (ivl 2 3))
;; <------------- 3
;;      1 -- 2
(check-equal? (ivl-meet (ivl -inf.0 3) (ivl 1 2)) (ivl 1 2))
;; 0 ------------->
;;      1 -- 2
(check-equal? (ivl-meet (ivl 0 +inf.0) (ivl 1 2)) (ivl 1 2))

;; Two infinite
;; <--- 1    2 --->
(check-true (ivl-empty? (ivl-meet (ivl -inf.0 1) (ivl 2 +inf.0))))
;; <-------------->
;;      1 -- 2
(check-equal? (ivl-meet (ivl -inf.0 +inf.0) (ivl 1 2)) (ivl 1 2))
;;      1 -------->
;; <-------- 2
(check-equal? (ivl-meet (ivl 1 +inf.0) (ivl -inf.0 2)) (ivl 1 2))
;; <-------- 2
;; <------------- 3
(check-equal? (ivl-meet (ivl -inf.0 2) (ivl -inf.0 3)) (ivl -inf.0 2))
;; 0 ------------->
;;      1 -------->
(check-equal? (ivl-meet (ivl 0 +inf.0) (ivl 1 +inf.0)) (ivl 1 +inf.0))

;; Three infinite
;; <-------------->
;;      1 -------->
(check-equal? (ivl-meet (ivl -inf.0 +inf.0) (ivl 1 +inf.0)) (ivl 1 +inf.0))
;; <-------------->
;; <-------- 2
(check-equal? (ivl-meet (ivl -inf.0 +inf.0) (ivl -inf.0 2)) (ivl -inf.0 2))

;; Four infinite
(check-equal? (ivl-meet (ivl -inf.0 +inf.0) (ivl -inf.0 +inf.0)) (ivl -inf.0 +inf.0))

;;; ivl-join (similar to a union)

(check-true (ivl-empty? (ivl-join empty-ivl empty-ivl)))
(check-equal? (ivl-join empty-ivl (ivl 0 3)) (ivl 0 3))

;; All specified
(check-equal? (ivl-join (ivl 0 1) (ivl 2 3)) (ivl 0 3))
(check-equal? (ivl-join (ivl 0 2) (ivl 1 3)) (ivl 0 3))
(check-equal? (ivl-join (ivl 0 3) (ivl 1 2)) (ivl 0 3))

;; One not specified
;; <--- 1    2 -- 3
(check-equal? (ivl-join (ivl #f 1) (ivl 2 3)) (ivl 2 3))
;; 0 -- 1    2 --->
(check-equal? (ivl-join (ivl 0 1) (ivl 2 #f)) (ivl 0 1))
;; <-------- 2
;;      1 ------- 3
(check-equal? (ivl-join (ivl #f 2) (ivl 1 3)) (ivl 1 3))
;;           2 --->
;; 0 ------------ 3
(check-equal? (ivl-join (ivl 2 #f) (ivl 0 3)) (ivl 0 3))
;; <------------- 3
;;      1 -- 2
(check-equal? (ivl-join (ivl #f 3) (ivl 1 2)) (ivl 1 3))
;; 0 ------------->
;;      1 -- 2
(check-equal? (ivl-join (ivl 0 #f) (ivl 1 2)) (ivl 0 2))

;; Two not specified
;; <--- 1    2 --->
(check-equal? (ivl-join (ivl #f 1) (ivl 2 #f)) (ivl 1 2))
;; <-------------->
;;      1 -- 2
(check-equal? (ivl-join (ivl #f #f) (ivl 1 2)) (ivl 1 2))
;;      1 -------->
;; <-------- 2
(check-equal? (ivl-join (ivl 1 #f) (ivl #f 2)) (ivl 1 2))
;; <-------- 2
;; <------------- 3
(check-equal? (ivl-join (ivl #f 2) (ivl #f 3)) (ivl #f 3))
;; 0 ------------->
;;      1 -------->
(check-equal? (ivl-join (ivl 0 #f) (ivl 1 #f)) (ivl 0 #f))

;; Three not specified
;; <-------------->
;;      1 -------->
(check-equal? (ivl-join (ivl #f #f) (ivl 1 #f)) (ivl 1 #f))
;; <-------------->
;; <-------- 2
(check-equal? (ivl-join (ivl #f #f) (ivl #f 2)) (ivl #f 2))

;; Four not specified
(check-equal? (ivl-join (ivl #f #f) (ivl #f #f)) (ivl #f #f))

;; One infinite
;; <--- 1    2 -- 3
(check-equal? (ivl-join (ivl -inf.0 1) (ivl 2 3)) (ivl -inf.0 3))
;; 0 -- 1    2 --->
(check-equal? (ivl-join (ivl 0 1) (ivl 2 +inf.0)) (ivl 0 +inf.0))
;; <-------- 2
;;      1 ------- 3
(check-equal? (ivl-join (ivl -inf.0 2) (ivl 1 3)) (ivl -inf.0 3))
;;           2 --->
;; 0 ------------ 3
(check-equal? (ivl-join (ivl 2 +inf.0) (ivl 0 3)) (ivl 0 +inf.0))
;; <------------- 3
;;      1 -- 2
(check-equal? (ivl-join (ivl -inf.0 3) (ivl 1 2)) (ivl -inf.0 3))
;; 0 ------------->
;;      1 -- 2
(check-equal? (ivl-join (ivl 0 +inf.0) (ivl 1 2)) (ivl 0 +inf.0))

;; Two infinite
;; <-------------->
;;      1 -- 2
(check-equal? (ivl-join (ivl -inf.0 +inf.0) (ivl 1 2)) (ivl -inf.0 +inf.0))
;;      1 -------->
;; <-------- 2
(check-equal? (ivl-join (ivl 1 +inf.0) (ivl -inf.0 2)) (ivl -inf.0 +inf.0))
;; <--- 1    2 --->
(check-equal? (ivl-join (ivl -inf.0 1) (ivl 2 +inf.0)) (ivl -inf.0 +inf.0))
;; <-------- 2
;; <------------- 3
(check-equal? (ivl-join (ivl -inf.0 2) (ivl -inf.0 3)) (ivl -inf.0 3))
;; 0 ------------->
;;      1 -------->
(check-equal? (ivl-join (ivl 0 +inf.0) (ivl 1 +inf.0)) (ivl 0 +inf.0))

;; Three infinite
;; <-------------->
;;      1 -------->
(check-equal? (ivl-join (ivl -inf.0 +inf.0) (ivl 1 +inf.0)) (ivl -inf.0 +inf.0))
;; <-------------->
;; <-------- 2
(check-equal? (ivl-join (ivl -inf.0 +inf.0) (ivl -inf.0 2)) (ivl -inf.0 +inf.0))

;; Four infinite
(check-equal? (ivl-join (ivl -inf.0 +inf.0) (ivl -inf.0 +inf.0)) (ivl -inf.0 +inf.0))

;; ===================================================================================================
;; Vectors

(check-true (vector-andmap zero? #(0 0 0 0)))
(check-false (vector-andmap zero? #(0 0 1 0)))

(check-true (vector-andmap (λ (x y) (and (= x 1) (= y 2)))
                           #(1 1 1 1)
                           #(2 2 2 2)))
(check-false (vector-andmap (λ (x y) (and (= x 1) (= y 2)))
                            #(1 1 1 1)
                            #(2 1 2 2)))

(check-true (vector-ormap zero? #(0 0 1 0)))
(check-false (vector-ormap zero? #(1 1 1 1)))

(check-true (vector-ormap (λ (x y) (and (= x 1) (= y 2)))
                          #(0 0 1 0)
                          #(0 0 2 0)))
(check-false (vector-ormap (λ (x y) (and (= x 1) (= y 2)))
                           #(0 0 1 0)
                           #(0 2 0 0)))

;; ===================================================================================================
;; Worker threads

(let ()
  (define wt (make-worker-thread (match-lambda
                                   [(list x y z)  (sleep 0.1)
                                                  (+ x y z)])))
  
  (collect-garbage)
  (collect-garbage)
  (check-true (worker-thread-waiting? wt))
  (check-true (worker-thread-put wt (list 1 2 3)))
  (check-true (worker-thread-working? wt))
  (check-equal? (worker-thread-get wt) 6)
  (check-true (worker-thread-put wt (list 1 2 3)))
  (check-false (worker-thread-try-put wt (list 10 20 30)))
  (check-exn exn? (λ () (worker-thread-put wt (list 10 20 30))))
  (check-false (worker-thread-try-get wt))
  (sleep 0.2)
  (check-equal? (worker-thread-try-get wt) 6)
  (check-true (worker-thread-put wt (list 10 20 30)))
  (check-equal? (worker-thread-send wt (list 1 2 3)) 6)
  (check-exn exn? (λ () (worker-thread-get wt)))
  (check-false (worker-thread-try-get wt))
  (check-true (worker-thread-try-put wt (list 1 2 3)))
  (sleep 0.2)
  (check-false (worker-thread-try-put wt (list 10 20 30)))
  (check-equal? (worker-thread-wait wt) (void))
  (check-true (worker-thread-put wt (list 1 2)))
  (check-exn exn? (λ () (worker-thread-get wt)))
  )
