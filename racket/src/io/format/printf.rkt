#lang racket/base
(require "../print/main.rkt"
         (submod "../print/main.rkt" internal)
         "../port/string-output.rkt")

(provide do-printf)

;; Since this module implements formatting, it can't use the usual
;; error functions or other formatting functions.

(define (do-printf who o fmt all-args)
  (define len (string-length fmt))
  
  ;; First pass: check format and argument consistency
  (define (next args) (and (pair? args) (cdr args)))
  (let loop ([i 0] [expected-count 0] [args all-args] [error-thunk #f])
    (cond
     [(= i len)
      (check-conclusions who expected-count args error-thunk fmt all-args)]
     [else
      (case (string-ref fmt i)
        [(#\~)
         (let ([i (add1 i)])
           (when (= i len)
             (ill-formed-error who "cannot end in `~`" fmt all-args))
           (case (string-ref fmt i)
             [(#\~ #\% #\n #\N)
              (loop (add1 i) expected-count args error-thunk)]
             [(#\a #\A #\s #\S #\v #\V #\e #\E)
              (loop (add1 i) (add1 expected-count) (next args) error-thunk)]
             [(#\.)
              (let ([i (add1 i)])
                (define (bad-dot)
                  (ill-formed-error who "tag `~.` not followed by `a`, `s`, or `v`" fmt all-args))
                (when (= i len)
                  (bad-dot))
                (case (string-ref fmt i)
                  [(#\a #\A #\s #\S #\v #\V)
                   (loop (add1 i) (add1 expected-count) (next args) error-thunk)]
                  [else (bad-dot)]))]
             [(#\x #\X #\o #\O #\b #\B)
              (define new-error-thunk (and (not error-thunk)
                                           (pair? args)
                                           (let ([a (car args)])
                                             (or (not (number? a))
                                                 (not (exact? a))))
                                           (lambda ()
                                             (arg-type-error who "exact integer" (car args) fmt args))))
              (loop (add1 i) (add1 expected-count) (next args) new-error-thunk)]
             [(#\c #\C)
              (define new-error-thunk (and (not error-thunk)
                                           (pair? args)
                                           (not (char? (car args)))
                                           (lambda ()
                                             (arg-type-error who "character" (car args) fmt args))))
              (loop (add1 i) (add1 expected-count) (next args) new-error-thunk)]
             [else
              (cond
               [(char-whitespace? (string-ref fmt i))
                (loop (add1 i) expected-count args error-thunk)]
               [else
                (ill-formed-error who
                                  (string-append "tag `~" (substring fmt i (add1 i)) "` not allowed")
                                  fmt
                                  all-args)])]))]
        [else (loop (add1 i) expected-count args error-thunk)])]))
  
  ;; Second pass: output
  (let loop ([start-i 0] [i 0] [args all-args])
    (cond
     [(= i len)
      (write-string fmt o start-i i)]
     [else
      (case (string-ref fmt i)
        [(#\~)
         (define (next i args) (let ([i (add1 i)])
                                 (loop i i args)))
         (write-string fmt o start-i i)
         (let ([i (add1 i)])
           (define c (string-ref fmt i))
           (case c
             [(#\~)
              (write-string "~" o)
              (next i args)]
             [(#\% #\n #\N)
              (write-string "\n" o)
              (next i args)]
             [(#\a #\A)
              (do-display who (car args) o)
              (next i (cdr args))]
             [(#\s #\S)
              (do-write who (car args) o)
              (next i (cdr args))]
             [(#\v #\V)
              (do-global-print who (car args) o)
              (next i (cdr args))]
             [(#\e #\E)
              (parameterize ([print-unreadable #t])
                (write-string ((error-value->string-handler)
                               (car args)
                               (error-print-width))
                              o))
              (next i (cdr args))]
             [(#\.)
              (let ([i (add1 i)])
                (case (string-ref fmt i)
                  [(#\a #\A)
                   (do-display who (car args) o (error-print-width))
                   (next i (cdr args))]
                  [(#\s #\S)
                   (do-write who (car args) o (error-print-width))
                   (next i (cdr args))]
                  [(#\v #\V)
                   ;; Intentionally using `do-print` instead of
                   ;; `do-global-print`:
                   (do-print who (car args) o 0 (error-print-width))
                   (next i (cdr args))]))]
             [(#\x #\X)
              (write-string (number->string (car args) 16) o)
              (next i (cdr args))]
             [(#\o #\O)
              (write-string (number->string (car args) 8) o)
              (next i (cdr args))]
             [(#\b #\B)
              (write-string (number->string (car args) 2) o)
              (next i (cdr args))]
             [(#\c #\C)
              (write-string (string (car args)) o)
              (next i (cdr args))]
             [else
              (cond
               [(char-whitespace? c)
                ;; Skip whitespace, but no more than one newline/return:
                (let ws-loop ([i i] [saw-newline? #f])
                  (cond
                   [(= i len) (loop i i args)]
                   [else
                    (define c (string-ref fmt i))
                    (case c
                      [(#\newline)
                       (if saw-newline?
                           (loop i i args)
                           (ws-loop (add1 i) #t))]
                      [(#\return)
                       (if saw-newline?
                           (loop i i args)
                           (ws-loop (if (and ((add1 i) . < . len)
                                             (char=? #\newline (string-ref fmt (add1 i))))
                                        (+ i 2)
                                        (add1 i))
                                    #t))]
                      [else (if (char-whitespace? c)
                                (ws-loop (add1 i) saw-newline?)
                                (loop i i args))])]))])]))]
        [else
         (loop start-i (add1 i) args)])]))
  
  (void))

;; ----------------------------------------

(define (raise-error str)
  (raise (exn:fail:contract str (current-continuation-marks))))

(define (check-conclusions who expected-count args error-thunk fmt all-args)
  (unless (null? args)
    (raise-error (string-append
                  (symbol->string who)
                  ": "
                  "format string requires "
                  (number->string expected-count)
                  " arguments, given "
                  (number->string (length all-args))
                  (arguments->string (cons fmt all-args)))))
  (when error-thunk (error-thunk)))

(define (ill-formed-error who explanation fmt args)
  (raise-error (string-append
                (symbol->string who)
                ": "
                "ill-formed pattern string\n"
                "  explanation: " explanation
                (arguments->string (cons fmt args)))))

(define (arg-type-error who what val fmt args)
  (raise-error (string-append
                (symbol->string who)
                ": "
                "format string requires a " what ", given something else\n"
                "  bad argument: " (value->string val)
                (arguments->string (cons fmt args)))))

(define (value->string v)
  ((error-value->string-handler) v (error-print-width)))

(define (arguments->string args)
  "")
