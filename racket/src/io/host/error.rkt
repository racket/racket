#lang racket/base
(require "../string/convert.rkt"
         "rktio.rkt"
         "thread.rkt")

(provide remap-rktio-error
         format-rktio-message
         format-rktio-system-error-message
         raise-rktio-error
         check-rktio-error
         check-rktio-error*)

(define (remap-rktio-error err)
  (start-atomic)
  (rktio_set_last_error rktio
                        (rktio-errkind err)
                        (rktio-errno err))
  (rktio_remap_last_error rktio)
  (define errno (rktio_get_last_error rktio))
  (define errkind (rktio_get_last_error_kind rktio))
  (end-atomic)
  (vector errkind errno))

(define (format-rktio-message who err base-msg)
  (string-append (if who (symbol->string who) "")
                 (if who ": " "")
                 base-msg
                 "\n  system error: "
                 (format-rktio-system-error-message err)))

(define (format-rktio-system-error-message err)
  (start-atomic)
  (define p (rktio_get_error_string rktio
                                    (rktio-errkind err)
                                    (rktio-errno err)))
  (define system-msg (rktio_to_bytes p))
  (end-atomic)
  (string-append (bytes->string/utf-8 system-msg #\?)
                 "; "
                 (let ([kind (rktio-errkind err)])
                   (cond
                     [(eqv? kind RKTIO_ERROR_KIND_POSIX) "errno"]
                     [(eqv? kind RKTIO_ERROR_KIND_WINDOWS) "win_err"]
                     [(eqv? kind RKTIO_ERROR_KIND_GAI) "gai_err"]
                     [else "rkt_err"]))
                 "="
                 (number->string (rktio-errno err))))

(define (raise-rktio-error who err base-msg)
  (raise
   (exn:fail
    (format-rktio-message who err base-msg)
    (current-continuation-marks))))

(define (check-rktio-error v base-msg)
  (when (rktio-error? v)
    (raise-rktio-error #f v base-msg))
  v)

(define (check-rktio-error* v base-msg)
  (check-rktio-error v base-msg)
  (void))
