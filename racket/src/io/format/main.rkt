#lang racket/base
(require "../common/check.rkt"
         "../port/parameter.rkt"
         "../port/output-port.rkt"
         "../port/string-port.rkt"
         "../string/convert.rkt"
         "printf.rkt")

(provide fprintf
         printf
         eprintf
         format)

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

;; ----------------------------------------

(define (general-format fmt args)
  (check 'format string? fmt)
  (define o (open-output-string))
  (do-printf 'format o fmt args)
  (get-output-string o))

(define (simple-format a)
  (cond
    [(boolean? a) (string-copy (if a "#t" "#f"))]
    [(number? a) (number->string a)]
    [(keyword? a) (string-append "#:" (keyword->string a))]
    [else #f]))

(define format
  (case-lambda
    [(fmt a)
     (cond
       [(or (equal? fmt "~a") (equal? fmt "~A"))
        (or (simple-format a)
            (cond
              [(bytes? a) (bytes->string/utf-8 a #\?)]
              [(string? a) (string-copy a)]
              [(symbol? a) (symbol->string a)]
              [else (general-format fmt (list a))]))]
       [(or (equal? fmt "~s") (equal? fmt "~S"))
        (or (simple-format a)
            (general-format fmt (list a)))]
       [else (general-format fmt (list a))])]
    [(fmt . args)
     (general-format fmt args)]))
