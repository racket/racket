#lang racket/base

;; Functions to format numbers, and data structures containing numbers.

(require racket/string racket/list racket/pretty racket/contract racket/match
         "math.rkt"
         "contract.rkt" "contract-doc.rkt")

(provide digits-for-range real->plot-label ->plot-label real->string/trunc)

(define (remove-trailing-zeros str)
  (let loop ([i  (string-length str)])
    (cond [(zero? i)  "0"]
          [(char=? #\0 (string-ref str (sub1 i)))  (loop (sub1 i))]
          [(char=? #\. (string-ref str (sub1 i)))  (substring str 0 (sub1 i))]
          [else  (substring str 0 i)])))

;; Returns the number of fractional digits needed to distinguish numbers [x-min..x-max]
(defproc (digits-for-range [x-min real?] [x-max real?]
                           [extra-digits exact-integer? 3]) exact-integer?
  (define range (abs (- x-max x-min)))
  (+ extra-digits (if (zero? range) 0 (- (floor-log10 range)))))

(define (int-str->e-str str)
  (define n (string-length str))
  (cond [(or (= 0 n) (string=? str "0"))  "0"]
        [else
         (define fst (substring str 0 1))
         (define rst (substring str 1 n))
         (format "~ae~a" (remove-trailing-zeros (format "~a.~a" fst rst)) (sub1 n))]))

(begin
  (require rackunit)
  (check-equal? (int-str->e-str "") "0")
  (check-equal? (int-str->e-str "0") "0")
  (check-equal? (int-str->e-str "10") "1e1"))

(define (frac-str->e-str str)
  (define n (string-length str))
  (let loop ([i 0])
    (cond [(= i n)  "0"]
          [(char=? #\0 (string-ref str i))  (loop (add1 i))]
          [else
           (define fst (substring str i (add1 i)))
           (define rst (substring str (add1 i) n))
           (cond [(= 0 (string-length rst))  (format "~ae~a" fst (- (add1 i)))]
                 [else  (format "~a.~ae~a" fst rst (- (add1 i)))])])))

(begin
  (require rackunit)
  (check-equal? (frac-str->e-str "") "0")
  (check-equal? (frac-str->e-str "0") "0")
  (check-equal? (frac-str->e-str "00") "0")
  (check-equal? (frac-str->e-str "1") "1e-1")
  (check-equal? (frac-str->e-str "01") "1e-2"))

(define (zero-string n)
  (list->string (build-list n (λ _ #\0))))

(defproc (real->plot-label [x real?] [digits exact-integer?]) any
  (cond
    [(zero? x)  "0"]
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
       ;; Get scientific notation for the integer and fractional parts
       (define int-e-str (int-str->e-str int-str))
       (define frac-e-str (frac-str->e-str frac-str))
       ;(printf "int-str = ~v, frac-str = ~v~n" int-str frac-str)
       ;(printf "int-e-str = ~v, frac-e-str = ~v~n" int-e-str frac-e-str)
       (define int-zero? (string=? int-str "0"))
       (define frac-zero? (string=? frac-str "0"))
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
       (argmin string-length strs))]))

(defproc (->plot-label [a any/c] [digits exact-nonnegative-integer? 7]) string?
  (let loop ([a a])
    (cond [(string? a)   a]
          [(symbol? a)   (symbol->string a)]
          [(real? a)     (real->plot-label a digits)]
          [(list? a)     (string-append "(" (string-join (map loop a) " ") ")")]
          [(cons? a)     (string-append "(" (loop (car a)) " . " (loop (cdr a)) ")")]
          [(boolean? a)  (if a "true" "false")]
          [(char? a)     (list->string (list a))]
          [else  (pretty-format a)])))

;; Like real->decimal-string, but removes trailing zeros
(defproc (real->string/trunc [x real?] [e exact-integer?]) string?
  (remove-trailing-zeros (real->decimal-string x (max 0 e))))
