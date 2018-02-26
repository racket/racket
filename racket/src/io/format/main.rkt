#lang racket/base
(require "../common/check.rkt"
         "../port/parameter.rkt"
         "../port/output-port.rkt"
         "../port/string-port.rkt"
         "printf.rkt")

(provide format
         fprintf
         printf
         eprintf)

(define/who (format fmt . args)
  (check who string? fmt)
  (define o (open-output-string))
  (do-printf 'printf o fmt args)
  (get-output-string o))

(define/who (fprintf o fmt . args)
  (check who output-port? o)
  (check who string? fmt)
  (do-printf who o fmt args))

(define/who (printf fmt . args)
  (check who string? fmt)
  (do-printf who (current-output-port) fmt args))

(define/who (eprintf fmt . args)
  (check who string? fmt)
  (do-printf who (current-error-port) fmt args))
