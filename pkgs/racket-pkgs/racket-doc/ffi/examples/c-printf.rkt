#lang racket/base

(require ffi/unsafe)

;; This code demonstrates how to interface `printf' which can be used with
;; different arities and types.  Also, `printf' is unsafe unless this code will
;; parse the format string and make sure that all the types match, instead, if
;; this code is used in a library, it should be provided from a module with
;; `unsafe' in its name.  (Note that originally there was an `unsafe!'
;; declaration for using the ffi, and this code demonstrated adding a similar
;; `c-printf-is-dangerous!' declaration.)

;; It's not too useful, since the C printf will obviously ignore
;; `current-output-port'.

(provide c-printf)

(define interfaces (make-hash))

(define (c-printf fmt . args)
  (define itypes
    (cons _string
          (map (lambda (x)
                 (cond [(and (integer? x) (exact? x)) _int]
                       [(and (number? x) (real? x))   _double*]
                       [(string? x)  _string]
                       [(bytes? x)   _bytes]
                       [(symbol? x)  _symbol]
                       [else (error 'c-printf
                                    "don't know how to deal with ~e" x)]))
               args)))
  (let ([printf (hash-ref interfaces itypes
                  (lambda ()
                    ;; Note: throws away the return value of printf
                    (let ([i (get-ffi-obj "printf" #f
                                          (_cprocedure itypes _void))])
                      (hash-set! interfaces itypes i)
                      i)))])
    (apply printf fmt args)))
