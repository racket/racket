#lang racket/base

(provide seconds->date)

(require racket/fixnum
         "../common/check.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../string/main.rkt"
         "../error/main.rkt")

(define rktio_seconds_to_date-error-kind
  (vector-immutable RKTIO_ERROR_KIND_RACKET
                    RKTIO_ERROR_TIME_OUT_OF_RANGE))

(define/who (seconds->date s [local? #t])
  (check who real? s)
  (let* ([s (inexact->exact s)]
         [si (floor s)]
         [get-gmt (if local? 0 1)]
         [nsecs (floor (* (- s si) 1000000000))]
         ;; The allocation, deallocation and the conversion of the
         ;; rktio_date_t* result is hidden in rktio_seconds_to_date*,
         ;; therefore no atomicity is needed here.
         [dt (rktio_seconds_to_date* rktio si nsecs get-gmt)])
    (cond
      [(date*? dt)
       dt]
      [(equal? dt rktio_seconds_to_date-error-kind)
       (raise-arguments-error who "integer is out-of-range"
                              "integer" si)]
      [else
       (raise-rktio-error who dt "conversion error")])))
