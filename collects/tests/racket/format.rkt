#lang racket/base
(require rackunit
         racket/list
         racket/math
         racket/format)

(define-syntax-rule (tc expr expected)
  (test-equal? (format "~s" 'expr) expr expected))

(define-syntax-rule (tcrx expr len rx)
  (test-case (format "~s" 'expr)
    (let ([v expr])
      (when len (check-equal? (string-length v) len))
      (check-regexp-match rx v))))

;; ~a

(tc (~a "north")
    "north")
(tc (~a 'south)
    "south")
(tc (~a #"east")
    "east")
(tc (~a #\w "e" 'st)
    "west")
(tc (~a (list "red" 'green #"blue"))
    "(red green blue)")
(tc (~a 17)
    "17")
(tc (~a #e1e20)
    (number->string #e1e20))
(tc (~a pi)
    (number->string pi))
(tc (~a (expt 6.1 87))
    (number->string (expt 6.1 87)))

(tc (~a "a" "b" "c" #:width 5)
    "abc  ")

(tc (~a "abcde" #:max-width 5)
    "abcde")
(tc (~a "abcde" #:max-width 4)
    "abcd")
(tc (~a "abcde" #:max-width 4 #:limit-marker "...")
    "a...")
(tc (~a "abcde" #:max-width 4 #:limit-marker "*")
    "abc*")
(tc (~a "abcde" #:max-width 4 #:limit-marker "")
    "abcd")
(tc (~a "The quick brown fox" #:max-width 15 #:limit-marker "")
    "The quick brown")
(tc (~a "The quick brown fox" #:max-width 15 #:limit-marker "...")
    "The quick br...")

(tcrx (~a "apple" #:min-width 20 #:align 'left)
      20 #rx"^apple( )*$")
(tcrx (~a "pear" #:min-width 20 #:align 'left #:right-pad-string " x")
      20 #rx"^pear(x)?( x)*$")
(tcrx (~a "plum" #:min-width 20 #:align 'right #:left-pad-string "x ")
      20 #rx"^(x )*(x)?plum$")
(tcrx (~a "orange" #:min-width 20 #:align 'center
           #:left-pad-string "- " #:right-pad-string " -")
      20 #rx"^(- )*(-)?orange(-)?( -)*$")

(tc (~a "short" #:width 6)
    "short ")
(tc (~a "loquacious" #:width 6 #:limit-marker "...")
    "loq...")

;; ~v

(tc (~v "north")
    "\"north\"")
(tc (~v 'south)
    "'south")
(tc (~v #"east")
    "#\"east\"")
(tc (~v #\w)
    "#\\w")
(tc (~v (list "red" 'green #"blue"))
    "'(\"red\" green #\"blue\")")

(tc (~v '(123456) #:max-width 5)
    "'(...")

;; ~s

(tc (~s "north")
    "\"north\"")
(tc (~s 'south)
    "south")
(tc (~s #"east")
    "#\"east\"")
(tc (~s #\w)
    "#\\w")
(tc (~s (list "red" 'green #"blue"))
    "(\"red\" green #\"blue\")")

(tc (~s 123456 #:max-width 5)
    "12...")

;; ~r

(tc (~r 0)
    "0")
(tc (~r pi)
    "3.141593")
(tc (~r pi #:precision 4)
    "3.1416")
(tc (~r pi #:precision 0)
    "3")
(tc (~r 1.5 #:precision 4)
    "1.5")
(tc (~r 1.5 #:precision '(= 4))
    "1.5000")
(tc (~r 50 #:precision 2)
    "50")
(tc (~r 50 #:precision '(= 2))
    "50.00")
(tc (~r 50 #:precision '(= 0))
    "50.")

(tc (~r 17)
    "17")
(tc (~r 17 #:min-width 4)
    "  17")
(tc (~r -42 #:min-width 4)
    "-  42")
(tc (~r 1.5 #:min-width 4)
    " 1.5")
(tc (~r 1.5 #:precision 4 #:min-width 10)
    "       1.5")
(tc (~r 1.5 #:precision '(= 4) #:min-width 10)
    "    1.5000")

(tc (~r -42 #:min-width 4 #:pad-string "0")
    "-0042")

(tc (~r 17 #:min-width 4 #:pad-string "0")
    "0017")
(tc (~r -42 #:min-width 4 #:pad-string "0")
    "-0042")

(tc (for/list ([x '(17 0 -42)]) (~r x))
    '("17" "0" "-42"))
(tc (for/list ([x '(17 0 -42)]) (~r x #:sign '+))
    '("+17" "0" "-42"))
(tc (for/list ([x '(17 0 -42)]) (~r x #:sign '++))
    '("+17" "+0" "-42"))
(tc (for/list ([x '(17 0 -42)]) (~r x #:sign 'parens))
    '("17" "0" "(42)"))
(tc (let ([sign-table '(("" " up") "an even " ("" " down"))])
      (for/list ([x '(17 0 -42)]) (~r x #:sign sign-table)))
    '("17 up" "an even 0" "42 down"))

(tc (~r 100 #:base 7)
    "202")
(tc (~r 102 #:base 7)
    "204")
(tc (~r 4.5 #:base 2)
    "100.1")
(tc (~r 3735928559 #:base 16)
    "deadbeef")
(tc (~r 3735928559 #:base '(up 16))
    "DEADBEEF")
(tc (~r (+ 102 1/7 2/49 3/343) #:base 7)
    "204.123")

(tc (~r 999 #:precision 3)
    "999")
(tc (~r 1000 #:precision 3)
    "1000")

;; ~r #:notation 'positional

(tc (~r #:notation 'positional pi)
    "3.141593")
(tc (~r #:notation 'positional pi #:precision 4)
    "3.1416")
(tc (~r #:notation 'positional pi #:precision 0)
    "3")
(tc (~r #:notation 'positional 1.5 #:precision 4)
    "1.5")
(tc (~r #:notation 'positional 1.5 #:precision '(= 4))
    "1.5000")
(tc (~r #:notation 'positional 50 #:precision 2)
    "50")
(tc (~r #:notation 'positional 50 #:precision '(= 2))
    "50.00")
(tc (~r #:notation 'positional 50 #:precision '(= 0))
    "50.")

(tc (~r #:notation 'positional 17)
    "17")
(tc (~r #:notation 'positional 17 #:min-width 4)
    "  17")
(tc (~r #:notation 'positional -42 #:min-width 4)
    "-  42")
(tc (~r #:notation 'positional 1.5 #:min-width 4)
    " 1.5")
(tc (~r #:notation 'positional 1.5 #:precision 4 #:min-width 10)
    "       1.5")
(tc (~r #:notation 'positional 1.5 #:precision '(= 4) #:min-width 10)
    "    1.5000")

(tc (~r #:notation 'positional -42 #:min-width 4 #:pad-string "0")
    "-0042")

(tc (~r #:notation 'positional 17 #:min-width 4 #:pad-string "0")
    "0017")
(tc (~r #:notation 'positional -42 #:min-width 4 #:pad-string "0")
    "-0042")

(tc (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x))
    '("17" "0" "-42"))
(tc (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x #:sign '+))
    '("+17" "0" "-42"))
(tc (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x #:sign '++))
    '("+17" "+0" "-42"))
(tc (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x #:sign 'parens))
    '("17" "0" "(42)"))
(tc (let ([sign-table '(("" " up") "an even " ("" " down"))])
      (for/list ([x '(17 0 -42)]) (~r #:notation 'positional x #:sign sign-table)))
    '("17 up" "an even 0" "42 down"))

(tc (~r #:notation 'positional 102 #:base 7)
    "204")
(tc (~r #:notation 'positional 4.5 #:base 2)
    "100.1")
(tc (~r #:notation 'positional 3735928559 #:base 16)
    "deadbeef")
(tc (~r #:notation 'positional 3735928559 #:base '(up 16))
    "DEADBEEF")

(tc (~r #:notation 'positional 0)
    "0")
(tc (~r #:notation 'positional 0 #:precision 4)
    "0")
(tc (~r #:notation 'positional 0 #:precision '(= 4))
    "0.0000")

;; ~r #:notation 'exponential

(tc (~r 12345 #:precision 3 #:notation 'exponential)
    "1.235e+04")
(tc (~r 12345 #:precision 2 #:notation 'exponential)
    "1.23e+04")
(tc (~r 10000 #:precision 2 #:notation 'exponential)
    "1e+04")
(tc (~r 10000 #:precision '(= 2) #:notation 'exponential)
    "1.00e+04")

(tc (~r 12345 #:precision 4 #:min-width 12 #:notation 'exponential)
    "  1.2345e+04")

(tc (~r #:notation 'exponential 1000)
    "1e+03")
(tc (~r #:notation 'exponential 0.9876)
    "9.876e-01")

(tc (~r #:notation 'exponential 100 #:base 2)
    "1.1001*2^+06")

(tc (~r #:notation 'exponential 1234 #:format-exponent "E")
    "1.234E+03")

(tc (~r #:notation 'exponential 12345 #:precision 3)
    ;; note rounding!
    "1.235e+04")
(tc (~r #:notation 'exponential 12345 #:precision 2)
    "1.23e+04")
(tc (~r #:notation 'exponential 10000 #:precision 2)
    "1e+04")
(tc (~r #:notation 'exponential 10000 #:precision '(= 2))
    "1.00e+04")

(tc (~r #:notation 'exponential 12345 #:min-width 12)
    "  1.2345e+04")

(tc (~r 3735928559 #:base '(up 16) #:precision 6 #:notation 'exponential)
    ;; note rounding!
    "D.EADBEF*16^+07")

(tc (~r 33.99508664763296 #:precision 1 #:min-width 5)
    "   34")
(tc (~r 33.99508664763296 #:precision 2 #:min-width 7)
    "     34")

(tc (~r 33.99508664763296 #:precision 1)
    "34")
(tc (~r 33.99508664763296 #:precision '(= 1))
    "34.0")
(tc (~r 33.99508664763296 #:precision '(= 2))
    "34.00")
(tc (~r 33.99508664763296 #:precision '(= 3))
    "33.995")

(tc (~r -33.99508664763296 #:precision 1)
    "-34")
(tc (~r -33.99508664763296 #:precision '(= 1))
    "-34.0")
(tc (~r -33.99508664763296 #:precision '(= 2))
    "-34.00")
(tc (~r -33.99508664763296 #:precision '(= 3))
    "-33.995")

(tc (~r #:notation 'exponential 0)
    "0e+00")
(tc (~r #:notation 'exponential 0 #:precision 4)
    "0e+00")
(tc (~r #:notation 'exponential 0 #:precision '(= 4))
    "0.0000e+00")

;; some random testing for exponential notation
;;  - only positive numbers
;;  - limited number of digits, exponent range

(define (random-in low hi) ;; closed
  (+ low (random (- hi low -1))))

(define (digits->int digits base)
  (for/fold ([acc 0]) ([digit (in-list digits)])
    (+ (* base acc) digit)))

(define (random-exponential-check #:base [base0 #f] #:precision [precision0 #f])
  (define base (or base0 (random-in 2 36)))
  (define precision (or precision0 (random-in 0 5)))
  (define digit0 (random-in 1 (sub1 base)))
  (define digits (for/list ([i (in-range precision)])
                   (random-in 0 (sub1 base))))
  (define exponent (random-in -50 50))
  (define exact-num
    (* (digits->int (cons digit0 digits) base)
       (expt base (- exponent precision))))
  (define inexact-num (exact->inexact exact-num))
  (define check-exp (make-exp-checker base precision digit0 digits exponent))
  (define (fmt n exactly?)
    (~r n #:notation 'exponential #:base base #:format-exponent ";"
        #:precision (if exactly? `(= ,precision) precision)))
  (check-exp (fmt exact-num #t) #f)
  (check-exp (fmt exact-num #f) #f)
  (check-exp (fmt inexact-num #t) #t)
  (check-exp (fmt inexact-num #f) #f))

(define (make-exp-checker base precision digit0 digits exponent)
  (lambda (s exactly?)
    (with-handlers ([void
                     (lambda (e)
                       (eprintf "failed on base=~s, prec=~s, digits=~s,~s, exp=~s; got ~s\n"
                                base precision digit0 digits exponent s)
                       (raise e))])
      (cond [(regexp-match #rx"^([a-z0-9])(?:\\.([a-z0-9]*))?;([+-][0-9]+)$" s)
             => (lambda (m)
                  (check-pred pair? m)
                  ;; Check leading digit is good.
                  (check-equal? (second m) (~r digit0 #:base base))
                  (define got-digits (map string (string->list (or (third m) ""))))
                  (define want-digits (for/list ([d digits]) (~r d #:base base)))
                  (check (if exactly? = <=) (length got-digits) (length want-digits))
                  ;; Check digits we got are good
                  (for ([got got-digits]
                        [want want-digits])
                    (check-equal? got want))
                  ;; If we didn't get as many digits as wanted, check rest are 0
                  (for ([want-more (drop want-digits (length got-digits))])
                    (check-equal? "0" want-more))
                  ;; Check exponent
                  (check-equal? (string->number (fourth m)) exponent))]
            [else (error 'exp-checker "bad: ~s" s)]))))

(for ([i (in-range 100)])
  (random-exponential-check #:base 10 #:precision 10))

(for ([i (in-range 1000)])
  (random-exponential-check))
