#lang racket/base

(provide seconds->date)

(require racket/fixnum
         (only-in '#%kernel [date*? kernel:date*?])
         "../common/check.rkt"
         "../host/rktio.rkt"
         "../string/main.rkt"
         "../error/main.rkt")

(define rktio_seconds_to_date-error-kind
  (vector-immutable RKTIO_ERROR_KIND_RACKET
                    RKTIO_ERROR_TIME_OUT_OF_RANGE ))

(define/who seconds->date
  (case-lambda
   [(s) (seconds->date s #t)]
   [(s local?)
    (check who real? s)
    (let* ([s (inexact->exact s)]
           [si (floor s)]
           [get-gmt (if local? 0 1)]
           [nsecs (floor (* (- s si) 1000000000))]
           [dt (rktio_seconds_to_date* rktio si nsecs get-gmt)])
      (cond
        [(kernel:date*? dt)
         dt]
        [(equal? dt rktio_seconds_to_date-error-kind)
         (raise-arguments-error who "integer is out-of-range"
                                "integer" si)]
        [else
         (error who "conversion error\n  error: ~a; ~a"
                (vector-ref dt 1)
                (bytes->string/utf-8
                 (rktio_to_bytes
                  (rktio_get_last_error_string rktio))))]))]))
