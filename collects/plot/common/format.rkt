#lang racket/base

;; Functions to format numbers, and data structures containing numbers.

(require racket/string racket/list racket/pretty racket/contract
         "math.rkt"
         "contract.rkt" "contract-doc.rkt")

(provide (all-defined-out))

;; Like real->decimal-string, but removes trailing zeros
(defproc (real->string/trunc [x real?] [e exact-nonnegative-integer?]) string?
  (define str (real->decimal-string x e))
  (let loop ([x  (string-length str)])
    (cond [(zero? x)  "0"]
          [(char=? #\0 (string-ref str (sub1 x)))  (loop (sub1 x))]
          [(char=? #\. (string-ref str (sub1 x)))  (substring str 0 (sub1 x))]
          [else  (substring str 0 x)])))

;; Returns the number of digits needed to distinguish numbers [x-min..x-max]
(defproc (digits-for-range [x-min real?] [x-max real?]
                           [extra-digits exact-nonnegative-integer? 3]) exact-nonnegative-integer?
  (define range (abs (- x-max x-min)))
  (+ extra-digits (if (zero? range) 0 (max 0 (- (floor-log10 range))))))

(defproc (->plot-label [a any/c] [digits exact-nonnegative-integer? 7]) string?
  (let loop ([a a])
    (cond [(string? a)  a]
          [(symbol? a)  (symbol->string a)]
          [(real? a)    (real->string/trunc a digits)]
          [(list? a)    (string-append "(" (string-join (map loop a) " ") ")")]
          [(cons? a)    (string-append "(" (loop (car a)) " . " (loop (cdr a)) ")")]
          [(boolean? a) (if a "true" "false")]
          [(char? a)    (list->string (list a))]
          [else  (pretty-format a)])))
