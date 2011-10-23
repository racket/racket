#lang racket/base

;; Data structure that represents a tick, and functions that produce ticks.

(require racket/string racket/list racket/contract racket/pretty racket/match
         "math.rkt"
         "format.rkt"
         "utils.rkt"
         "contract.rkt"
         "contract-doc.rkt"
         "date-time.rkt"
         "axis-transform.rkt"
         "currency.rkt")

(provide (struct-out pre-tick) (struct-out tick) (struct-out ticks) ticks-layout/c ticks-format/c
         ;; No ticks
         no-ticks-layout no-ticks
         ;; Linear ticks
         linear-ticks-layout linear-ticks-format linear-ticks
         ;; Uniform ticks
         uniform-ticks-layout uniform-ticks
         ;; Log-scale ticks
         log-ticks-layout log-ticks-format log-ticks
         ;; Date ticks
         date-ticks-formats 24h-descending-date-ticks-formats 12h-descending-date-ticks-formats
         date-ticks-layout date-ticks-format date-ticks
         ;; Time ticks
         time-ticks-formats descending-time-ticks-formats
         time-ticks-layout time-ticks-format time-ticks
         ;; Bit/byte ticks
         bit/byte-ticks-format bit/byte-ticks
         ;; Currency ticks and formats
         currency-ticks-scales us-currency-scales uk-currency-scales eu-currency-scales
         currency-ticks-formats us-currency-formats uk-currency-formats eu-currency-formats
         currency-ticks-layout currency-ticks-format currency-ticks
         ;; Fractions
         fraction-ticks-format fraction-ticks
         ;; Combinators
         ticks-scale ticks-add linear-scale
         )

(define-struct/contract pre-tick ([value real?] [major? boolean?]) #:transparent)
(define-struct/contract (tick pre-tick) ([label string?]) #:transparent)

(defcontract ticks-layout/c
  (real? real? exact-positive-integer? axis-transform/c . -> . (listof pre-tick?)))

(defcontract ticks-format/c
  (real? real? (listof pre-tick?) . -> . (listof string?)))

(define-struct/contract ticks ([layout ticks-layout/c] [format ticks-format/c]) #:transparent
  #:property prop:procedure
  (λ (t x-min x-max max-ticks transform)
    (match-define (ticks layout format) t)
    (define ts (layout x-min x-max max-ticks transform))
    (match-define (list (pre-tick xs majors) ...) ts)
    (map tick xs majors (format x-min x-max ts))))

;; ===================================================================================================
;; Helpers

(define-syntax-rule (with-exact-bounds x-min x-max body ...)
  (cond [(x-min . >= . x-max)
         (error 'bounds-check "expected min < max; given min = ~e and max = ~e" x-min x-max)]
        [else  (let ([x-min  (inexact->exact x-min)]
                     [x-max  (inexact->exact x-max)])
                 body ...)]))

(define (linear-seq-args x-min x-max step)
  (define start (* (ceiling (/ x-min step)) step))
  (define end (* (floor (/ x-max step)) step))
  (define num (+ 1 (inexact->exact (round (/ (- end start) step)))))
  (values start end num))

(define (linear-major-values/step x-min x-max step)
  (define-values (start end num) (linear-seq-args x-min x-max step))
  (linear-seq start end num))

(defproc (linear-minor-values/step [major-xs (listof real?)] [major-step real?]
                                   [minor-ticks exact-nonnegative-integer?]) (listof real?)
  (cond [(or (zero? minor-ticks) (empty? major-xs))  empty]
        [else
         (define major-start (first major-xs))
         (define minor-step (/ major-step (+ minor-ticks 1)))
         (for*/list ([x  (in-list (cons (- major-start major-step) major-xs))]
                     [i  (in-range 1 (+ minor-ticks 1))])
           (+ x (* i minor-step)))]))

(defproc (tick-values->pre-ticks [major-xs (listof real?)] [minor-xs (listof real?)]
                                 ) (listof pre-tick?)
  (define major-ts (map (λ (x) (pre-tick x #t)) major-xs))
  (define minor-ts (map (λ (x) (pre-tick x #f)) minor-xs))
  (sort (append major-ts minor-ts) < #:key pre-tick-value))

;; ===================================================================================================
;; Linear ticks (default tick function, evenly spaced)

(defproc (linear-tick-step+divisor [x-min real?] [x-max real?]
                                   [max-ticks exact-positive-integer?]
                                   [base (and/c exact-integer? (>=/c 2))]
                                   [divisors (listof exact-positive-integer?)]
                                   ) (values real? exact-positive-integer?)
  (define range (- x-max x-min))
  (define mag (expt base (floor-log/base base range)))
  (define ds (sort divisors >))
  (let/ec break
    (for* ([e  (in-range (floor-log/base base max-ticks) -2 -1)]
           [d  (in-list ds)])
      ;(printf "new-d = ~v~n" (* d (expt base e)))
      (define step (/ mag d (expt base e)))
      (define-values (_start _end num) (linear-seq-args x-min x-max step))
      (when (num . <= . max-ticks)
        (break step d)))
    ;(printf "default!~n")
    (values (/ range max-ticks) max-ticks)))

(defproc (linear-tick-values [x-min real?] [x-max real?]
                             [max-ticks exact-positive-integer?]
                             [base (and/c exact-integer? (>=/c 2))]
                             [divisors (listof exact-positive-integer?)]
                             ) (values (listof real?) (listof real?))
  (with-exact-bounds
   x-min x-max
   (define-values (step d) (linear-tick-step+divisor x-min x-max max-ticks base divisors))
   (define major-xs (linear-major-values/step x-min x-max step))
   (define major-ticks (length major-xs))
   
   (define ns (filter (λ (n) (zero? (remainder (* n d) base))) divisors))
   (define n
     (cond [(empty? ns)  1]
           [else  (argmin (λ (n) (abs (- (* n major-ticks) max-ticks))) (sort ns <))]))
   (define minor-xs (linear-minor-values/step major-xs step (- n 1)))
   (values major-xs (filter (λ (x) (<= x-min x x-max)) minor-xs))))

(defproc (linear-ticks-layout [#:base base (and/c exact-integer? (>=/c 2)) 10]
                              [#:divisors divisors (listof exact-positive-integer?) '(1 2 5)]
                              ) ticks-layout/c
  (λ (x-min x-max max-ticks transform)
    (define-values (major-xs minor-xs) (linear-tick-values x-min x-max max-ticks base divisors))
    (tick-values->pre-ticks major-xs minor-xs)))

(defproc (linear-ticks-format) ticks-format/c
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define digits (digits-for-range x-min x-max))
     (for/list ([t  (in-list ts)])
       (real->plot-label (pre-tick-value t) digits)))))

(defproc (linear-ticks [#:base base (and/c exact-integer? (>=/c 2)) 10]
                       [#:divisors divisors (listof exact-positive-integer?) '(1 2 5)]) ticks?
  (ticks (linear-ticks-layout #:base base #:divisors divisors)
         (linear-ticks-format)))

;; ===================================================================================================
;; No ticks

(defproc (no-ticks-layout) ticks-layout/c
  (λ (x-min x-max max-ticks transform) empty))

(defproc (no-ticks) ticks?
  (ticks (no-ticks-layout) (linear-ticks-format)))

;; ===================================================================================================
;; Uniform spacing ticks

(defproc (uniform-ticks-layout [#:layout layout ticks-layout/c (linear-ticks-layout)]) ticks-layout/c
  (λ (x-min x-max max-ticks transform)
    (define ts (layout x-min x-max max-ticks transform))
    (define xs (map pre-tick-value ts))
    (define majors (map pre-tick-major? ts))
    (define new-xs (map (invertible-function-finv (apply-transform transform x-min x-max)) xs))
    (map pre-tick new-xs majors)))

(defproc (uniform-ticks [#:layout layout ticks-layout/c (linear-ticks-layout)]) ticks?
  (ticks (uniform-ticks-layout #:layout layout)
         (linear-ticks-format)))

;; ===================================================================================================
;; Exponential ticks (use for log scale)

(defproc (log-ticks-layout [#:base base (and/c exact-integer? (>=/c 2)) 10]) ticks-layout/c
  (λ (x-min x-max max-ticks transform)
    (with-exact-bounds
     x-min x-max
     (when ((exact->inexact x-min) . <= . 0)
       (raise-type-error 'log-ticks-layout "positive real" 0 x-min x-max))
     (define log-start (floor-log/base base x-min))
     (define log-end (ceiling-log/base base x-max))
     (define log-xs (for/list ([i  (in-range log-start (add1 log-end))]) i))
     (define skip (max 1 (floor (/ (+ (length log-xs) 2) 5))))
     (filter (λ (t) (<= x-min (pre-tick-value t) x-max))
             (append*
              (for/list ([log-x  (in-list log-xs)]
                         [m      (in-cycle (in-range skip))])
                (define x (expt base log-x))
                (cond [(= skip 1)  (for/list ([i  (in-range 0 (sub1 base) skip)])
                                     (pre-tick (+ x (* i x))
                                               (and (zero? i) (zero? m))))]
                      [else  (list (cond [(zero? m)  (pre-tick x #t)]
                                         [else  (pre-tick x #f)]))])))))))

(defproc (log-ticks-format [#:base base (and/c exact-integer? (>=/c 2)) 10]) ticks-format/c
  (define base-str (number->string base))
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define epsilon (expt 10 (- (digits-for-range x-min x-max))))
     (define base-digits (digits-for-range 0 base))
     (for/list ([t  (in-list ts)])
       (define x (pre-tick-value t))
       (define log-x (floor-log/base base x))
       (define round? ((abs (- x (expt base log-x))) . < . epsilon))
       (define major-str (format "~a~a" base-str (integer->superscript log-x)))
       (cond [round?  major-str]
             [else    (format "~a×~a"
                              (real->plot-label (/ x (expt base log-x)) base-digits)
                              major-str)])))))

(defproc (log-ticks [#:base base (and/c exact-integer? (>=/c 2)) 10]) ticks?
  (ticks (log-ticks-layout #:base base)
         (log-ticks-format #:base base)))

;; ===================================================================================================
;; Date/time helpers

(defproc (find-linear-tick-step [x-min real?] [x-max real?] [max-ticks exact-positive-integer?]
                                [steps (listof real?)]) real?
  (with-exact-bounds
   x-min x-max
   (let/ec break
     (for ([step  (in-list (sort steps <))])
       (define-values (_start _end num) (linear-seq-args x-min x-max step))
       (when (num . <= . max-ticks)
         (break step)))
     #f)))

(define (count-unchanging-fields formatter fmt-list xs)
  (let ([fmt-list  (filter symbol? fmt-list)])
    (define formatted-dates (for/list ([x  (in-list xs)])
                              (apply-formatter formatter fmt-list x)))
    (count equal?* (transpose formatted-dates))))

(define (choose-format-list formatter fmt-lists xs)
  (let ([fmt-lists  (sort fmt-lists >
                          #:key (λ (fmt-list) (count symbol? fmt-list))
                          #:cache-keys? #t)])
    (argmin (λ (fmt-list) (count-unchanging-fields formatter fmt-list xs))
            fmt-lists)))

;; ===================================================================================================
;; Date ticks

(define 12h-descending-date-ticks-formats
  '("~Y-~m-~d ~I:~M:~f~p"
    "~Y-~m-~d ~I:~M~p"
    "~Y-~m-~d ~I~p"
    "~Y-~m-~d"
    "~Y-~m"
    "~Y"
    
    "~m-~d ~I:~M:~f~p"
    "~m-~d ~I:~M~p"
    "~m-~d ~I~p"
    "~m-~d"
    
    "~I:~M:~f~p"
    "~I:~M~p"
    
    "~M:~fs"
    
    "~fs"))

(define 24h-descending-date-ticks-formats
  '("~Y-~m-~d ~H:~M:~f"
    "~Y-~m-~d ~H:~M"
    "~Y-~m-~d ~Hh"
    "~Y-~m-~d"
    "~Y-~m"
    "~Y"
    
    "~m-~d ~H:~M:~f"
    "~m-~d ~H:~M"
    "~m-~d ~Hh"
    "~m-~d"
    
    "~H:~M:~f"
    "~H:~M"
    
    "~M:~fs"
    
    "~fs"))

(defparam date-ticks-formats (listof string?) 24h-descending-date-ticks-formats)

;; Tick steps to try, in seconds
(define date-steps
  (list 1 2 5 10 15 20 30 40 45
        seconds-per-minute
        (* 2 seconds-per-minute)
        (* 5 seconds-per-minute)
        (* 10 seconds-per-minute)
        (* 15 seconds-per-minute)
        (* 20 seconds-per-minute)
        (* 30 seconds-per-minute)
        seconds-per-hour
        (* 2 seconds-per-hour)
        (* 3 seconds-per-hour)
        (* 4 seconds-per-hour)
        (* 6 seconds-per-hour)
        (* 8 seconds-per-hour)
        (* 12 seconds-per-hour)
        seconds-per-day
        (* 2 seconds-per-day)
        (* 5 seconds-per-day)
        (* 10 seconds-per-day)
        seconds-per-week
        (* 2 seconds-per-week)
        avg-seconds-per-month
        (* 2 avg-seconds-per-month)
        (* 3 avg-seconds-per-month)
        (* 4 avg-seconds-per-month)
        (* 6 avg-seconds-per-month)
        (* 8 avg-seconds-per-month)
        (* 9 avg-seconds-per-month)
        avg-seconds-per-year
        (* 2 avg-seconds-per-year)
        (* 5 avg-seconds-per-year)))

(define (date-tick-values x-min x-max max-ticks)
  (with-exact-bounds
   x-min x-max
   (define range (- x-max x-min))
   (define step
     (cond [(range . < . (* max-ticks (first date-steps)))
            (define-values (step _)
              (linear-tick-step+divisor x-min x-max max-ticks 10 '(1 2 5)))
            step]
           [(range . > . (* max-ticks (last date-steps)))
            (define-values (step _)
              (linear-tick-step+divisor (/ x-min avg-seconds-per-year)
                                        (/ x-max avg-seconds-per-year)
                                        max-ticks 10 '(1 2 5)))
            (* step avg-seconds-per-year)]
           [else  (find-linear-tick-step x-min x-max max-ticks date-steps)]))
   (define date-round
     (cond [(step . >= . avg-seconds-per-year)   utc-seconds-round-year]
           [(step . >= . avg-seconds-per-month)  utc-seconds-round-month]
           [else  (λ (d) d)]))
   (define major-xs (linear-major-values/step x-min x-max step))
   (values (map date-round major-xs) empty)))

(defproc (date-ticks-layout) ticks-layout/c
  (λ (x-min x-max max-ticks transform)
    (define-values (major-xs minor-xs) (date-tick-values x-min x-max max-ticks))
    (tick-values->pre-ticks major-xs minor-xs)))

(defproc (date-ticks-format [#:formats formats (listof string?) (date-ticks-formats)]) ticks-format/c
  (define fmt-lists (map parse-format-string formats))
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define formatter (plot-date-formatter x-min x-max))
     (define xs (map pre-tick-value ts))
     (define fmt-list (choose-format-list formatter fmt-lists xs))
     (for/list ([x  (in-list xs)])
       (string-append* (apply-formatter formatter fmt-list x))))))

(defproc (date-ticks [#:formats formats (listof string?) (date-ticks-formats)]) ticks?
  (ticks (date-ticks-layout)
         (date-ticks-format #:formats formats)))

;; ===================================================================================================
;; Time ticks

(define descending-time-ticks-formats
  '("~dd ~H:~M:~f"
    "~dd ~H:~M"
    "~dd ~Hh"
    "~dd"
    
    "~H:~M:~f"
    "~H:~M"
    "~Hh"
    
    "~M:~f"
    "~Mm"
    
    "~ss"))

(defparam time-ticks-formats (listof string?) descending-time-ticks-formats)

;; Tick steps to try, in seconds
(define time-steps
  (list 1 2 5 10 15 20 30 40 45
        seconds-per-minute
        (* 2 seconds-per-minute)
        (* 5 seconds-per-minute)
        (* 10 seconds-per-minute)
        (* 15 seconds-per-minute)
        (* 20 seconds-per-minute)
        (* 30 seconds-per-minute)
        (* 45 seconds-per-minute)
        seconds-per-hour
        (* 2 seconds-per-hour)
        (* 3 seconds-per-hour)
        (* 4 seconds-per-hour)
        (* 6 seconds-per-hour)
        (* 8 seconds-per-hour)
        (* 12 seconds-per-hour)
        (* 18 seconds-per-hour)
        seconds-per-day
        (* 2 seconds-per-day)
        (* 5 seconds-per-day)
        (* 10 seconds-per-day)
        (* 15 seconds-per-day)
        (* 30 seconds-per-day)
        (* 60 seconds-per-day)
        (* 90 seconds-per-day)))

(define (time-tick-values x-min x-max max-ticks)
  (with-exact-bounds
   x-min x-max
   (define range (- x-max x-min))
   (define step
     (cond [(range . < . (* max-ticks (first time-steps)))
            (define-values (step _)
              (linear-tick-step+divisor x-min x-max max-ticks 10 '(1 2 5)))
            step]
           [(range . > . (* max-ticks (last time-steps)))
            (define-values (step _)
              (linear-tick-step+divisor (/ x-min seconds-per-day)
                                        (/ x-max seconds-per-day)
                                        max-ticks 10 '(1 2 5)))
            (* step seconds-per-day)]
           [else
            (find-linear-tick-step x-min x-max max-ticks time-steps)]))
   (define major-xs (linear-major-values/step x-min x-max step))
   (values major-xs empty)))

(defproc (time-ticks-layout) ticks-layout/c
  (λ (x-min x-max max-ticks transform)
    (define-values (major-xs minor-xs) (time-tick-values x-min x-max max-ticks))
    (tick-values->pre-ticks major-xs minor-xs)))

(defproc (time-ticks-format [#:formats formats (listof string?) (time-ticks-formats)]) ticks-format/c
  (define fmt-lists (map parse-format-string formats))
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define formatter (plot-time-formatter x-min x-max))
     (define xs (map pre-tick-value ts))
     (define fmt-list (choose-format-list formatter fmt-lists xs))
     (for/list ([x  (in-list xs)])
       (string-append* (apply-formatter formatter fmt-list x))))))

(defproc (time-ticks [#:formats formats (listof string?) (time-ticks-formats)]) ticks?
  (ticks (time-ticks-layout)
         (time-ticks-format #:formats formats)))

;; ===================================================================================================
;; Byte and bit ticks

;; "", Kilo, Mega, Giga, Tera, Peta, Exa, Zeta, Yotta
(define byte-suffixes #("B" "KB" "MB" "GB" "TB" "PB" "EB" "ZB" "YB"))
(define bit-suffixes #("b" "Kb" "Mb" "Gb" "Tb" "Pb" "Eb" "Zb" "Yb"))

(defproc (bit/byte-ticks-format [#:size size (or/c 'byte 'bit) 'byte]
                                [#:kind kind (or/c 'CS 'SI) 'CS]) ticks-format/c
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define suffixes (if (eq? size 'bit) bit-suffixes byte-suffixes))
     (define-values (base pow) (case kind
                                 [(SI)  (values 10 3)]
                                 [else  (values 2 10)]))
     (define x-largest (max* (abs x-min) (abs x-max)))
     (define b (floor-log/base (expt base pow) x-largest))
     (define format-str
       (cond [(and (b . >= . 0) (b . < . (vector-length suffixes)))
              (format "~a ~a" "~a" (vector-ref suffixes b))]
             [else
              (format "~a×~a~a ~a" "~a"
                      base (integer->superscript (* b pow)) (vector-ref suffixes 0))]))
     (define unit (expt base (* b pow)))
     (define digits (digits-for-range (/ x-min unit) (/ x-max unit)))
     (for/list ([t  (in-list ts)])
       (define unit-x (/ (pre-tick-value t) unit))
       (format format-str (real->plot-label unit-x digits #f))))))

(defproc (bit/byte-ticks [#:size size (or/c 'byte 'bit) 'byte]
                         [#:kind kind (or/c 'CS 'SI) 'CS]) ticks?
  (define layout
    (case kind
      [(SI)  (linear-ticks-layout #:base 10 #:divisors '(1 2 5))]
      [else  (linear-ticks-layout #:base 2 #:divisors '(1 2))]))
  (ticks layout (bit/byte-ticks-format #:size size #:kind kind)))

;; ===================================================================================================
;; Currency

;; US "short scale" suffixes
(define us-currency-scales '("" "K" "M" "B" "T"))
;; The UK officially uses the short scale now
;; Million is abbreviated "m" instead of "mn" because "mn" stands for minutes; also, the Daily
;; Telegraph Style Guide totally says to use "m"
(define uk-currency-scales '("" "k" "m" "bn" "tr"))
;; European countries use the long scale: million, milliard, billion
(define eu-currency-scales '("" "K" "M" "Md" "B"))
;; The larger the scale suffixes get, the less standardized they are; so we stop at trillion (short)

;; US negative amounts are in parenthesis:
(define us-currency-formats '("~$~w.~f~s" "(~$~w.~f~s)" "~$0"))
;; The UK is more reasonable, using a negative sign for negative amounts:
(define uk-currency-formats '("~$~w.~f ~s" "-~$~w.~f ~s" "~$0"))
;; The more common EU format (e.g. France, Germany, Italy, Spain):
(define eu-currency-formats '("~w,~f ~s~$" "-~w,~f ~s~$" "0 ~$"))

(defparam currency-ticks-scales (listof string?) us-currency-scales)
(defparam currency-ticks-formats (list/c string? string? string?) us-currency-formats)

(struct amount-data (sign whole fractional unit suffix) #:transparent)

(define (currency-formatter x-min x-max)
  (λ (fmt data)
    (case fmt
      [(~$)  (amount-data-sign data)]
      [(~w)  (number->string (amount-data-whole data))]
      [(~f)  (match-define (amount-data _sign _whole f unit _suffix) data)
             (define digits (digits-for-range (/ x-min unit) (/ x-max unit)))
             (cond [(= 1 unit)  (substring (real->decimal-string* f 2 (max 2 digits)) 2)]
                   [(zero? f)   "0"]
                   [else        (substring (real->decimal-string* f 1 (max 1 digits)) 2)])]
      [(~s)  (amount-data-suffix data)]
      [else  #f])))

(defproc (currency-ticks-format [#:kind kind (or/c string? symbol?) 'USD]
                                [#:scales scales (listof string?) (currency-ticks-scales)]
                                [#:formats formats (list/c string? string? string?)
                                           (currency-ticks-formats)]
                                ) ticks-format/c
  (match-define (list positive-format-string negative-format-string zero-format-string) formats)
  (define positive-format-list (parse-format-string positive-format-string))
  (define negative-format-list (parse-format-string negative-format-string))
  (define zero-format-list (parse-format-string zero-format-string))
  (define suffixes (list->vector scales))
  (define n (vector-length suffixes))
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define formatter (currency-formatter x-min x-max))
     (define sign (cond [(string? kind)  kind]
                        [else  (hash-ref currency-code->sign kind (λ () (symbol->string kind)))]))
     (define x-largest (max* (abs x-min) (abs x-max)))
     (define b (let ([b  (floor-log/base 1000 x-largest)])
                 (if (b . < . 0) (+ b 1) b)))
     (define suffix
       (cond [(and (b . >= . 0) (b . < . n))  (vector-ref suffixes b)]
             [else  (format "×10~a" (integer->superscript (* b 3)))]))
     (define unit
       (cond [(= 0 (string-length suffix))  1]
             [else  (expt 1000 b)]))
     (for/list ([t  (in-list ts)])
       (define x (pre-tick-value t))
       (define format-list (cond [(positive? x)  positive-format-list]
                                 [(negative? x)  negative-format-list]
                                 [else           zero-format-list]))
       (define unit-x (/ (abs x) unit))
       (string-append*
        (apply-formatter formatter format-list
                         (amount-data sign (floor unit-x) (- unit-x (floor unit-x)) unit suffix)))))))

(defproc (currency-ticks-layout) ticks-layout/c
  (linear-ticks-layout #:base 10 #:divisors '(1 2 4 5)))

(defproc (currency-ticks [#:kind kind (or/c string? symbol?) 'USD]
                         [#:scales scales (listof string?) (currency-ticks-scales)]
                         [#:formats formats (list/c string? string? string?) (currency-ticks-formats)]
                         ) ticks?
  (ticks (currency-ticks-layout)
         (currency-ticks-format #:kind kind #:scales scales #:formats formats)))

;; ===================================================================================================
;; Fractions

(defparam fraction-ticks-base (and/c exact-integer? (>=/c 2)) 10)
(defparam fraction-ticks-divisors (listof exact-positive-integer?) '(1 2 3 4 5))

(define (format-fraction x)
  (cond [(inexact? x)  (format-fraction (inexact->exact x))]
        [(x . < . 0)  (format "-~a" (format-fraction (- x)))]
        [(x . = . 0)  "0"]
        [(x . < . 1)  (format "~a/~a" (numerator x) (denominator x))]
        [else
         (define d (denominator x))
         (cond [(d . = . 1)  (format "~a" (numerator x))]
               [else
                (define w (floor x))
                (let ([x  (- x w)])
                  (format "~a ~a/~a" w (numerator x) (denominator x)))])]))

(defproc (fraction-ticks-format) ticks-format/c
  (λ (x-min x-max ts)
    (for/list ([t  (in-list ts)])
      (format-fraction (pre-tick-value t)))))

(defproc (fraction-ticks [#:base base (and/c exact-integer? (>=/c 2)) (fraction-ticks-base)]
                         [#:divisors divisors (listof exact-positive-integer?)
                                     (fraction-ticks-divisors)]) ticks?
  (ticks (linear-ticks #:base base #:divisors divisors)
         (fraction-ticks-format)))

;; ===================================================================================================
;; Tick combinators

(defproc (ticks-scale [t ticks?] [fun invertible-function?]) ticks?
  (match-define (invertible-function f g) fun)
  (match-define (ticks layout format) t)
  (ticks (λ (x-min x-max max-ticks transform)
           (define ts (layout (f x-min) (f x-max) max-ticks transform))
           (for/list ([t  (in-list ts)])
             (match-define (pre-tick x major?) t)
             (pre-tick (g x) major?)))
         (λ (x-min x-max ts)
           (format (f x-min) (f x-max) (map (λ (t)
                                              (match-define (pre-tick x major?) t)
                                              (pre-tick (f x) major?))
                                            ts)))))

(defproc (ticks-add [t ticks?] [xs (listof real?)] [major? boolean? #t]) ticks?
  (match-define (ticks layout format) t)
  (ticks (λ (x-min x-max max-ticks transform)
           (append (layout x-min x-max max-ticks transform)
                   (for/list ([x  (in-list xs)])
                     (pre-tick x major?))))
         format))

(defproc (linear-scale [m real?] [b real? 0]) invertible-function?
  (invertible-function (λ (x) (+ (* m x) b))
                       (λ (y) (/ (- y b) m))))
