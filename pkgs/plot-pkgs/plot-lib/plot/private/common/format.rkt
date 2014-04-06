#lang racket/base

;; Functions to format numbers, and data structures containing numbers.

(require racket/string racket/list racket/pretty racket/contract racket/match
         unstable/latent-contract/defthing
         "contract.rkt"
         "math.rkt")

(provide (all-defined-out))

(define (string-map f str)
  (list->string (map f (string->list str))))

(defproc (integer->superscript [x exact-integer?]) string?
  (string-map (λ (c) (case c
                       [(#\0)  #\u2070]
                       [(#\1)  #\u00b9]
                       [(#\2)  #\u00b2]
                       [(#\3)  #\u00b3]
                       [(#\4)  #\u2074]
                       [(#\5)  #\u2075]
                       [(#\6)  #\u2076]
                       [(#\7)  #\u2077]
                       [(#\8)  #\u2078]
                       [(#\9)  #\u2079]
                       [(#\+)  #\u207a]
                       [(#\-)  #\u207b]
                       [else   c]))
              (number->string x)))

(defproc (real->decimal-string* [x  real?]
                                [min-digits exact-nonnegative-integer?]
                                [max-digits exact-nonnegative-integer? min-digits]) string?
  (when (min-digits . > . max-digits)
    (error 'real->decimal-string* "expected min-digits <= max-digits; given ~e and ~e"
           min-digits max-digits))
  (define str (real->decimal-string x max-digits))
  (let loop ([i  (string-length str)] [j (- max-digits min-digits)])
    (cond [(zero? j)  (substring str 0 i)]
          [(zero? i)  "0"]  ; shouldn't happen, as real->decimal-string guarantees a "0." prefix
          [(char=? #\0 (string-ref str (- i 1)))  (loop (- i 1) (- j 1))]
          [else  (substring str 0 i)])))

(define (remove-trailing-zeros str)
  (let loop ([i  (string-length str)])
    (cond [(zero? i)  "0"]
          [(char=? #\0 (string-ref str (sub1 i)))  (loop (sub1 i))]
          [(char=? #\. (string-ref str (sub1 i)))  (substring str 0 (sub1 i))]
          [else  (substring str 0 i)])))

;; Returns the number of fractional digits needed to distinguish numbers [x-min..x-max]
(defproc (digits-for-range [x-min real?] [x-max real?]
                           [base (and/c exact-integer? (>=/c 2)) 10]
                           [extra-digits exact-integer? 3]) exact-integer?
  (define range (abs (- x-max x-min)))
  (+ extra-digits (if (zero? range) 0 (- (floor-log/base base range)))))

(define (int-str->e-str str)
  (define n (string-length str))
  (cond [(or (= 0 n) (string=? str "0"))  "0"]
        [else
         (define fst (substring str 0 1))
         (define rst (substring str 1 n))
         (format "~a×10~a"
                 (remove-trailing-zeros (format "~a.~a" fst rst))
                 (integer->superscript (sub1 n)))]))

(define (frac-str->e-str str)
  (define n (string-length str))
  (let loop ([i 0])
    (cond [(= i n)  "0"]
          [(char=? #\0 (string-ref str i))  (loop (add1 i))]
          [else  (define fst (substring str i (add1 i)))
                 (define rst (substring str (add1 i) n))
                 (cond [(= 0 (string-length rst))
                        (format "~a×10~a" fst (integer->superscript (- (add1 i))))]
                       [else
                        (format "~a.~a×10~a" fst rst (integer->superscript (- (add1 i))))])])))

(define (zero-string n)
  (make-string n #\0))

(defproc (real->plot-label [x real?] [digits exact-integer?] [scientific? boolean? #t]) any
  (cond
    [(zero? x)  "0"]
    [(eqv? x +nan.0)  "+nan.0"]
    [(eqv? x +inf.0)  "+inf.0"]
    [(eqv? x -inf.0)  "-inf.0"]
    [else
     (define front-sign (if (x . < . 0) "-" ""))
     (define mid-sign (if (x . < . 0) "-" "+"))
     (let* ([x  (abs (inexact->exact x))])
       ;; Round away any extra digits
       (define round-fac (expt 10 digits))
       (define y (/ (round (* x round-fac)) round-fac))
       ;; Parse the output of real->decimal-string
       (define-values (int-str frac-str)
         (match-let ([(list _ int-str frac-str)
                      (regexp-match #rx"(.*)\\.(.*)" (real->decimal-string y (max 0 digits)))])
           (values int-str (remove-trailing-zeros frac-str))))
       (define int-zero? (string=? int-str "0"))
       (define frac-zero? (string=? frac-str "0"))
       (cond
         [scientific?
          ;; Get scientific notation for the integer and fractional parts
          (define int-e-str (int-str->e-str int-str))
          (define frac-e-str (frac-str->e-str frac-str))
          ;(printf "int-str = ~v, frac-str = ~v~n" int-str frac-str)
          ;(printf "int-e-str = ~v, frac-e-str = ~v~n" int-e-str frac-e-str)
          (define int-e-zero? (string=? int-e-str "0"))
          (define frac-e-zero? (string=? frac-e-str "0"))
          ;; Build a list of possible output strings
          (define strs
            (list (cond [(and int-zero? frac-zero?)  "0"]
                        [int-zero?   (format "~a.~a" front-sign frac-str)]
                        [frac-zero?  (format "~a~a" front-sign int-str)]
                        [else        (format "~a~a.~a" front-sign int-str frac-str)])
                  (cond [(and int-e-zero? frac-zero?)  "0"]
                        [int-e-zero?  (format "~a.~a" front-sign frac-str)]
                        [frac-zero?   (format "~a~a" front-sign int-e-str)]
                        [else         (format "~a(~a)~a.~a" front-sign int-e-str mid-sign frac-str)])
                  (cond [(and int-zero? frac-e-zero?)  "0"]
                        [int-zero?     (format "~a~a" front-sign frac-e-str)]
                        [frac-e-zero?  (format "~a~a" front-sign int-str)]
                        [else          (format "~a~a~a(~a)" front-sign int-str mid-sign frac-e-str)])
                  (cond [(and int-e-zero? frac-e-zero?)  "0"]
                        [int-e-zero?   (format "~a~a" front-sign frac-e-str)]
                        [frac-e-zero?  (format "~a~a" front-sign int-e-str)]
                        [else
                         (format "~a(~a)~a(~a)" front-sign int-e-str mid-sign frac-e-str)])))
          ;; Return the shortest possible output string
          (argmin string-length strs)]
         [else
          (cond [(and int-zero? frac-zero?)  "0"]
                [int-zero?   (format "~a.~a" front-sign frac-str)]
                [frac-zero?  (format "~a~a" front-sign int-str)]
                [else        (format "~a~a.~a" front-sign int-str frac-str)])]))]))

(define (format-special x)
  (case x
    [(#f)  "#f"]
    [(+nan.0)  "+nan.0"]
    [(+inf.0)  "+inf.0"]
    [(-inf.0)  "-inf.0"]
    [else  "<unknown>"]))

(defproc (ivl->plot-label [i ivl?] [extra-digits exact-integer? 3]) string?
  (match-define (ivl a b) i)
  (cond [(and (not (rational? a)) (not (rational? b)))
         (format "[~a,~a]" (format-special a) (format-special b))]
        [(not (rational? a))  (format "[~a,~a]" (format-special a) (real->plot-label b 15))]
        [(not (rational? b))  (format "[~a,~a]" (real->plot-label a 15) (format-special b))]
        [else
         (define digits (digits-for-range a b 10 extra-digits))
         (format "[~a,~a]"
                 (real->plot-label a digits)
                 (real->plot-label b digits))]))

(defproc (->plot-label [a any/c] [digits exact-integer? 7]) string?
  (let loop ([a a])
    (cond [(string? a)   a]
          [(symbol? a)   (symbol->string a)]
          [(real? a)     (real->plot-label a digits)]
          [(ivl? a)      (ivl->plot-label a)]
          [(list? a)     (string-append "(" (string-join (map loop a)) ")")]
          [(cons? a)     (string-append "(" (loop (car a)) " . " (loop (cdr a)) ")")]
          [(boolean? a)  (if a "true" "false")]
          [(char? a)     (list->string (list a))]
          [else  (pretty-format a)])))

;; Like real->decimal-string, but removes trailing zeros
(defproc (real->string/trunc [x real?] [e exact-integer?]) string?
  (remove-trailing-zeros (real->decimal-string x (max 0 e))))

;; ===================================================================================================
;; Format strings

(defproc (parse-format-string [str string?]) (listof (or/c string? symbol?))
  (define n (string-length str))
  (let loop ([i 0] [fmt-list  empty])
    (cond [(i . >= . n)  (reverse fmt-list)]
          [(i . = . (- n 1))  (reverse (cons (substring str i (+ i 1)) fmt-list))]
          [(char=? #\~ (string-ref str i))
           (loop (+ i 2) (cons (string->symbol (substring str i (+ i 2))) fmt-list))]
          [else  (loop (+ i 1) (cons (substring str i (+ i 1)) fmt-list))])))

(defproc (apply-formatter [formatter (symbol? any/c . -> . (or/c string? #f))]
                          [fmt-list (listof (or/c string? symbol?))]
                          [d any/c]) (listof string?)
  (for/list ([fmt  (in-list fmt-list)])
    (cond [(eq? fmt '~~)  "~"]
          [(symbol? fmt)  (let ([val  (formatter fmt d)])
                            (if val val (symbol->string fmt)))]
          [(string? fmt)  fmt])))
