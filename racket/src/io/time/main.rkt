#lang racket/base

(provide seconds->date)

(require racket/fixnum
         (only-in '#%kernel [date* kernel:date*])
         "../common/check.rkt"
         "../host/rktio.rkt"
         "../string/main.rkt"
         "../error/main.rkt")

(define rktio_seconds_to_date-error-kind
  (vector-immutable RKTIO_ERROR_KIND_RACKET
                    RKTIO_ERROR_TIME_OUT_OF_RANGE ))

(define unknown-zone-name
  (string->immutable-string "?"))

(define/who seconds->date
  (case-lambda
   [(s) (seconds->date s #t)]
   [(s local?)
    (check who real? s)
    (let* ([s (inexact->exact s)]
           [si (floor s)])
      (unless (fixnum? si)
        (raise-arguments-error who "integer is out-of-range"
                               "integer" si))
      (let* ([get-gmt (if local? 0 1)]
             [nsecs (floor (* (- s si) 1000000000))]
             [p (rktio_seconds_to_date rktio si nsecs get-gmt)])
        (cond
          [(not (vector? p))
           (let* ([dt (rktio_date_to_vector p)]
                  [tzn (vector-ref dt 11)]
                  [zname (if tzn
                             (string->immutable-string
                              (bytes->string/utf-8 (rktio_to_bytes tzn)))
                             unknown-zone-name)])
             (when tzn
               (rktio_free tzn))
             (rktio_free p)
             (kernel:date*
              (vector-ref dt 1)
              (vector-ref dt 2)
              (vector-ref dt 3)
              (vector-ref dt 4)
              (vector-ref dt 5)
              (vector-ref dt 6)
              (vector-ref dt 7)
              (vector-ref dt 8)
              (if (fx= (vector-ref dt 9) 0) #f #t)
              (vector-ref dt 10)
              (vector-ref dt 0)
              zname))]
          [(equal? p rktio_seconds_to_date-error-kind)
           (raise-arguments-error who "integer is out-of-range"
                                  "integer" si)]
          [else
           (error who "conversion error\n  error: ~a; ~a"
                  (vector-ref p 1)
                  (bytes->string/utf-8
                   (rktio_to_bytes
                    (rktio_get_last_error_string rktio))))])))]))
