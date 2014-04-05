#lang racket/base

;; Data structure that represents a tick, and functions that produce ticks.

(require racket/string racket/list racket/contract racket/pretty racket/match racket/sequence
         unstable/latent-contract/defthing
         "math.rkt"
         "contract.rkt"
         "format.rkt"
         "utils.rkt"
         "axis-transform.rkt"
         "sample.rkt"
         "date-time.rkt"
         "currency.rkt")

(provide (all-defined-out))

(struct pre-tick (value major?) #:transparent)
(struct tick pre-tick (label) #:transparent)

(struct ticks (layout format) #:transparent
  #:property prop:procedure
  (λ (t x-min x-max)
    (match-define (ticks layout format) t)
    (define ts (map pre-tick-inexact->exact (layout x-min x-max)))
    (match-define (list (pre-tick xs majors) ...) ts)
    (map tick xs majors (format x-min x-max ts))))

(defcontract ticks-layout/c (real? real? . -> . (listof pre-tick?)))
(defcontract ticks-format/c (real? real? (listof pre-tick?) . -> . (listof string?)))

(defparam ticks-default-number exact-positive-integer? 4)

;; ===================================================================================================
;; Helpers

(define-syntax-rule (with-exact-bounds x-min x-max body ...)
  (cond [(x-min . > . x-max)
         (error 'bounds-check "expected min <= max; given min = ~e and max = ~e" x-min x-max)]
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

(defproc (linear-tick-step [x-min real?] [x-max real?]
                           [num-ticks exact-positive-integer?]
                           [base (and/c exact-integer? (>=/c 2))]
                           [divisors (listof exact-positive-integer?)]) real?
  (define range (- x-max x-min))
  (define mag (expt base (floor-log/base base range)))
  (define epsilon (expt 10 (- (digits-for-range x-min x-max))))
  (define e-start (floor-log/base base num-ticks))
  (define-values (step diff)
    (for*/fold ([step #f] [diff +inf.0]) ([e  (in-range e-start -2 -1)]
                                          [d  (in-list (sort divisors <))])
      ;; when num-ticks > base, we sometimes must divide by (expt base e) instead of just base
      (define new-step (/ mag d (expt base e)))
      ;; find the start, end and number of ticks with this step size
      (define-values (new-start new-end new-num) (linear-seq-args x-min x-max new-step))
      ;; endpoints don't count in the number of ticks (a concession for contour-ticks, which
      ;; seems to work well outside of contour plots anyway)
      (let* ([new-num  (if ((abs (- new-start x-min)) . < . epsilon) (- new-num 1) new-num)]
             [new-num  (if ((abs (- new-end x-max)) . < . epsilon) (- new-num 1) new-num)])
        ;; keep the step size that generates the number of ticks closest to num-ticks
        (define new-diff (abs (- new-num num-ticks)))
        (cond [(new-diff . <= . diff)  (values new-step new-diff)]
              [else  (values step diff)]))))
  (if step step (/ range num-ticks)))

(defproc (linear-tick-values [x-min real?] [x-max real?]
                             [num-ticks exact-positive-integer?]
                             [base (and/c exact-integer? (>=/c 2))]
                             [divisors (listof exact-positive-integer?)]
                             ) (values (listof real?) (listof real?))
  (with-exact-bounds
   x-min x-max
   (cond
     [(= x-min x-max)  (values empty empty)]
     [else
      (define major-step (linear-tick-step x-min x-max num-ticks base divisors))
      (define major-xs (linear-major-values/step x-min x-max major-step))
      (define num-major-ticks (length major-xs))
      
      (define minor-xs
        (let loop ([mult 2])
          (cond [(mult . > . 4)  empty]
                [else
                 (define minor-step (linear-tick-step x-min x-max (* mult num-ticks) base divisors))
                 (define minor-xs (linear-major-values/step x-min x-max minor-step))
                 (cond [(empty? (remove* minor-xs major-xs))
                        ;; this covers the major ticks as well; check for additional minor ticks
                        (define real-minor-xs (remove* major-xs minor-xs))
                        (cond [(empty? real-minor-xs)  (loop (+ 1 mult))]
                              [else  real-minor-xs])]
                       [else  (loop (+ 1 mult))])])))
      
      (values major-xs minor-xs)])))

(defproc (linear-ticks-layout [#:number number exact-positive-integer? (ticks-default-number)]
                              [#:base base (and/c exact-integer? (>=/c 2)) 10]
                              [#:divisors divisors (listof exact-positive-integer?) '(1 2 4 5)]
                              ) ticks-layout/c
  (λ (x-min x-max)
    (define-values (major-xs minor-xs) (linear-tick-values x-min x-max number base divisors))
    (tick-values->pre-ticks major-xs minor-xs)))

(defproc (linear-ticks-format) ticks-format/c
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define digits (digits-for-range x-min x-max))
     (for/list ([t  (in-list ts)])
       (real->plot-label (pre-tick-value t) digits)))))

(defproc (linear-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                       [#:base base (and/c exact-integer? (>=/c 2)) 10]
                       [#:divisors divisors (listof exact-positive-integer?) '(1 2 4 5)]
                       ) ticks? #:document-body
  (ticks (linear-ticks-layout #:number number #:base base
                              #:divisors divisors)
         (linear-ticks-format)))

;; ===================================================================================================
;; No ticks

(defthing no-ticks-layout ticks-layout/c #:document-value
  (λ (x-min x-max) empty))

(defthing no-ticks-format ticks-format/c #:document-value
  (λ (x-min x-max pre-ticks)
    (map (λ (_) "") pre-ticks)))

(defthing no-ticks ticks? #:document-value
  (ticks no-ticks-layout no-ticks-format))

;; ===================================================================================================
;; Exponential ticks (for log scale)

(defproc (log-ticks-layout [#:number number exact-positive-integer? (ticks-default-number)]
                           [#:base base (and/c exact-integer? (>=/c 2)) 10]
                           ) ticks-layout/c
  (λ (x-min x-max)
    (with-exact-bounds
     x-min x-max
     (when ((exact->inexact x-min) . <= . 0)
       (raise-type-error 'log-ticks-layout "positive real" 0 x-min x-max))
     (define log-start (ceiling-log/base base x-min))
     (define log-end (floor-log/base base x-max))
     (define skip (max 1 (ceiling (/ (+ 1 (- log-end log-start)) number))))
     (filter (λ (t) (<= x-min (pre-tick-value t) x-max))
             (append*
              (for/list ([log-x  (in-range (- log-start 1) (+ log-end 2))]
                         [m      (in-cycle (in-range skip))])
                (define x (expt base log-x))
                (cond [(= skip 1)  (for/list ([i  (in-range 0 (sub1 base))])
                                     (pre-tick (+ x (* i x))
                                               (and (zero? i) (zero? m))))]
                      [else  (list (pre-tick x (zero? m)))])))))))

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
       (define (major-str)
         (if (zero? log-x) "1" (format "~a~a" base-str (integer->superscript log-x))))
       (cond [((abs (- x (expt base log-x))) . < . epsilon)  (major-str)]
             [(zero? log-x)  (real->plot-label x base-digits)]
             [else  (format "~a×~a"
                            (real->plot-label (/ x (expt base log-x)) base-digits)
                            (major-str))])))))

(defproc (log-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                    [#:base base (and/c exact-integer? (>=/c 2)) 10]
                    ) ticks? #:document-body
  (ticks (log-ticks-layout #:number number #:base base)
         (log-ticks-format #:base base)))

;; ===================================================================================================
;; Date/time helpers

(defproc (find-linear-tick-step [x-min real?] [x-max real?]
                                [num-ticks exact-positive-integer?]
                                [steps (listof real?)]) real?
  (with-exact-bounds
   x-min x-max
   (define epsilon (expt 10 (- (digits-for-range x-min x-max))))
   (define-values (step diff)
    (for/fold ([step #f] [diff +inf.0]) ([new-step  (in-list (sort steps <))])
      (define-values (new-start new-end new-num) (linear-seq-args x-min x-max new-step))
      ;; endpoints don't count in number of ticks (see linear-tick-step)
      (let* ([new-num  (if ((abs (- new-start x-min)) . < . epsilon) (- new-num 1) new-num)]
             [new-num  (if ((abs (- new-end x-max)) . < . epsilon) (- new-num 1) new-num)])
        (define new-diff (abs (- new-num num-ticks)))
        (cond [(new-diff . <= . diff)  (values new-step new-diff)]
              [else  (values step diff)]))))
   step))

(define (count-changing-fields formatter fmt-list xs)
  (let ([fmt-list  (filter symbol? fmt-list)])
    (define formatted-dates (for/list ([x  (in-list xs)])
                              (apply-formatter formatter fmt-list x)))
    (count (λ (fields) (not (apply equal?* fields)))
           (transpose formatted-dates))))

;; Find the shortest format string that has the maximum number of changing fields
(define (choose-format-list formatter fmt-lists xs)
  (let ([fmt-lists  (sort fmt-lists <
                          #:key (λ (fmt-list) (count symbol? fmt-list))
                          #:cache-keys? #t)])
    (argmax (λ (fmt-list) (count-changing-fields formatter fmt-list xs))
            fmt-lists)))

;; ===================================================================================================
;; Date ticks

(defthing 24h-descending-date-ticks-formats (listof string?) #:document-value
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
    "~Hh"
    "~M:~fs"
    "~Mm"
    "~fs"))

(defthing 12h-descending-date-ticks-formats (listof string?) #:document-value
  '("~Y-~m-~d ~I:~M:~f ~p"
    "~Y-~m-~d ~I:~M ~p"
    "~Y-~m-~d ~I ~p"
    "~Y-~m-~d"
    "~Y-~m"
    "~Y"
    "~m-~d ~I:~M:~f ~p"
    "~m-~d ~I:~M ~p"
    "~m-~d ~I ~p"
    "~m-~d"
    "~I:~M:~f ~p"
    "~I:~M ~p"
    "~I ~p"
    "~M:~fs"
    "~Mm"
    "~fs"))

(defparam date-ticks-formats (listof string?) 24h-descending-date-ticks-formats)

;; Tick steps to try, in seconds
(define date-steps
  (list 1 2 4 5 10 15 20 30 40 45
        seconds-per-minute
        (* 2 seconds-per-minute)
        (* 4 seconds-per-minute)
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
        (* 4 seconds-per-day)
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
        (* 4 avg-seconds-per-year)
        (* 5 avg-seconds-per-year)))

(define (date-tick-values x-min x-max num-ticks)
  (with-exact-bounds
   x-min x-max
   (cond [(= x-min x-max)  (values empty empty)]
         [else
          (define range (- x-max x-min))
          (define step
            (cond [(range . < . (* num-ticks (first date-steps)))
                   (linear-tick-step x-min x-max num-ticks 10 '(1 2 4 5))]
                  [(range . > . (* num-ticks (last date-steps)))
                   (* avg-seconds-per-year
                      (linear-tick-step (/ x-min avg-seconds-per-year) (/ x-max avg-seconds-per-year)
                                        num-ticks 10 '(1 2 4 5)))]
                  [else  (find-linear-tick-step x-min x-max num-ticks date-steps)]))
          (define date-round
            (cond [(step . >= . avg-seconds-per-year)   utc-seconds-round-year]
                  [(step . >= . avg-seconds-per-month)  utc-seconds-round-month]
                  [else  (λ (d) d)]))
          (define major-xs (linear-major-values/step x-min x-max step))
          (values (map date-round major-xs) empty)])))

(defproc (date-ticks-layout [#:number number exact-positive-integer? (ticks-default-number)]
                            ) ticks-layout/c
  (λ (x-min x-max)
    (define-values (major-xs minor-xs) (date-tick-values x-min x-max number))
    (tick-values->pre-ticks major-xs minor-xs)))

(defproc (date-ticks-format [#:formats formats (listof string?) (date-ticks-formats)]) ticks-format/c
  (define fmt-lists (map parse-format-string formats))
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define formatter (plot-date-formatter x-min x-max))
     (define xs (map pre-tick-value ts))
     (cond [(empty? xs)  empty]
           [else
            (define fmt-list (choose-format-list formatter fmt-lists xs))
            (cons (string-append* (apply-formatter formatter fmt-list (first xs)))
                  (for/list ([last-x  (in-list xs)] [x  (in-list (rest xs))])
                    (define fmt-list (choose-format-list formatter fmt-lists (list last-x x)))
                    (string-append* (apply-formatter formatter fmt-list x))))]))))

(defproc (date-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                     [#:formats formats (listof string?) (date-ticks-formats)]
                     ) ticks? #:document-body
  (ticks (date-ticks-layout #:number number)
         (date-ticks-format #:formats formats)))

;; ===================================================================================================
;; Time ticks

(defthing 24h-descending-time-ticks-formats (listof string?) #:document-value
  '("~dd ~H:~M:~f"
    "~dd ~H:~M"
    "~dd ~Hh"
    "~dd"
    "~H:~M:~f"
    "~H:~M"
    "~Hh"
    "~M:~fs"
    "~Mm"
    "~fs"))

(defthing 12h-descending-time-ticks-formats (listof string?) #:document-value
  '("~dd ~I:~M:~f ~p"
    "~dd ~I:~M ~p"
    "~dd ~I ~p"
    "~dd"
    "~I:~M:~f ~p"
    "~I:~M ~p"
    "~I ~p"
    "~M:~fs"
    "~Mm"
    "~fs"))

(defparam time-ticks-formats (listof string?) 24h-descending-time-ticks-formats)

;; Tick steps to try, in seconds
(define time-steps
  (list 1 2 4 5 10 15 20 30 40 45
        seconds-per-minute
        (* 2 seconds-per-minute)
        (* 4 seconds-per-minute)
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
        (* 4 seconds-per-day)
        (* 5 seconds-per-day)
        (* 10 seconds-per-day)
        (* 15 seconds-per-day)
        (* 30 seconds-per-day)
        (* 60 seconds-per-day)
        (* 90 seconds-per-day)))

(define (time-tick-values x-min x-max num-ticks)
  (with-exact-bounds
   x-min x-max
   (cond [(= x-min x-max)  (values empty empty)]
         [else
          (define range (- x-max x-min))
          (define step
            (cond [(range . < . (* num-ticks (first time-steps)))
                   (linear-tick-step x-min x-max num-ticks 10 '(1 2 4 5))]
                  [(range . > . (* num-ticks (last time-steps)))
                   (* seconds-per-day
                      (linear-tick-step (/ x-min seconds-per-day) (/ x-max seconds-per-day)
                                        num-ticks 10 '(1 2 4 5)))]
                  [else
                   (find-linear-tick-step x-min x-max num-ticks time-steps)]))
          (define major-xs (linear-major-values/step x-min x-max step))
          (values major-xs empty)])))

(defproc (time-ticks-layout [#:number number exact-positive-integer? (ticks-default-number)]
                            ) ticks-layout/c
  (λ (x-min x-max)
    (define-values (major-xs minor-xs) (time-tick-values x-min x-max number))
    (tick-values->pre-ticks major-xs minor-xs)))

(defproc (time-ticks-format [#:formats formats (listof string?) (time-ticks-formats)]) ticks-format/c
  (define fmt-lists (map parse-format-string formats))
  (λ (x-min x-max ts)
    (with-exact-bounds
     x-min x-max
     (define formatter (plot-time-formatter x-min x-max))
     (define xs (map pre-tick-value ts))
     (cond [(empty? xs)  empty]
           [else
            (define fmt-list (choose-format-list formatter fmt-lists xs))
            (cons (string-append* (apply-formatter formatter fmt-list (first xs)))
                  (for/list ([last-x  (in-list xs)] [x  (in-list (rest xs))])
                    (define fmt-list (choose-format-list formatter fmt-lists (list last-x x)))
                    (string-append* (apply-formatter formatter fmt-list x))))]))))

(defproc (time-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                     [#:formats formats (listof string?) (time-ticks-formats)]
                     ) ticks? #:document-body
  (ticks (time-ticks-layout #:number number)
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

(defproc (bit/byte-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                         [#:size size (or/c 'byte 'bit) 'byte]
                         [#:kind kind (or/c 'CS 'SI) 'CS]
                         ) ticks? #:document-body
  (define si? (eq? kind 'SI))
  (ticks (linear-ticks-layout #:number number #:base (if si? 10 2)
                              #:divisors (if si? '(1 2 4 5) '(1 2)))
         (bit/byte-ticks-format #:size size #:kind kind)))

;; ===================================================================================================
;; Currency

;; US "short scale" suffixes
(defthing us-currency-scales (listof string?) #:document-value '("" "K" "M" "B" "T"))
;; The UK officially uses the short scale since 1974
;; Million is abbreviated "m" instead of "mn" because "mn" stands for minutes
(defthing uk-currency-scales (listof string?) #:document-value '("" "k" "m" "bn" "tr"))
;; European countries use the long scale: million, milliard, billion
(defthing eu-currency-scales (listof string?) #:document-value '("" "K" "M" "Md" "B"))
;; The larger the scale suffixes get, the less standardized they are; so we stop at billion (long)

;; US negative amounts are in parenthesis:
(defthing us-currency-formats (list/c string? string? string?) #:document-value
  '("~$~w.~f~s" "(~$~w.~f~s)" "~$0"))
;; The UK is more reasonable, using a negative sign for negative amounts:
(defthing uk-currency-formats (list/c string? string? string?) #:document-value
  '("~$~w.~f~s" "-~$~w.~f~s" "~$0"))
;; The more common EU format (e.g. France, Germany, Italy, Spain):
(defthing eu-currency-formats (list/c string? string? string?) #:document-value
  '("~w,~f ~s~$" "-~w,~f ~s~$" "0 ~$"))

(defparam currency-ticks-scales (listof string?) us-currency-scales)
(defparam currency-ticks-formats (list/c string? string? string?) us-currency-formats)

(struct amount-data (sign amount unit suffix) #:transparent)

(define (currency-formatter x-min x-max)
  (λ (fmt data)
    (case fmt
      [(~$)  (amount-data-sign data)]
      [(~s)  (amount-data-suffix data)]
      [(~w ~f)
       (match-define (amount-data _sign amt unit _suffix) data)
       (define digits (digits-for-range (/ x-min unit) (/ x-max unit)))
       (define n (max 2 digits))
       (define 10^n (expt 10 n))
       (define x (/ (round (* (inexact->exact amt) 10^n)) 10^n))
       (define whole (floor x))
       (case fmt
         [(~w)  (number->string whole)]
         [(~f)  (define frac (- x whole))
                (cond [(= 1 unit)    (substring (real->decimal-string* frac 2 n) 2)]
                      [(zero? frac)  "0"]
                      [else          (substring (real->decimal-string* frac 1 n) 2)])])]
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
                         (amount-data sign unit-x unit suffix)))))))

(defproc (currency-ticks [#:number number exact-positive-integer? (ticks-default-number)]
                         [#:kind kind (or/c string? symbol?) 'USD]
                         [#:scales scales (listof string?) (currency-ticks-scales)]
                         [#:formats formats (list/c string? string? string?) (currency-ticks-formats)]
                         ) ticks? #:document-body
  (ticks (linear-ticks-layout #:number number)
         (currency-ticks-format #:kind kind #:scales scales
                                #:formats formats)))

;; ===================================================================================================
;; Fractions

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

(defproc (fraction-ticks-format [#:base base (and/c exact-integer? (>=/c 2)) 10]
                                [#:divisors divisors (listof exact-positive-integer?) '(1 2 3 4 5)]
                                ) ticks-format/c
  (define fracs (remove-duplicates (map (λ (d) (/ d base)) divisors)))
  (λ (x-min x-max ts)
    (define digits (digits-for-range x-min x-max base (ceiling-log/base base 1000)))
    (define fracs (remove-duplicates (map (λ (d) (* (/ base d) (expt base (- digits)))) divisors)))
    (for/list ([t  (in-list ts)])
      (define x (inexact->exact (pre-tick-value t)))
      (define xs
        (for/list ([frac  (in-list fracs)])
          (* frac (round (/ x frac)))))
      (format-fraction (argmin (λ (y) (abs (- x y))) xs)))))

(defproc (fraction-ticks [#:base base (and/c exact-integer? (>=/c 2)) 10]
                         [#:divisors divisors (listof exact-positive-integer?) '(1 2 3 4 5)]
                         ) ticks? #:document-body
  (ticks (linear-ticks #:base base #:divisors divisors)
         (fraction-ticks-format #:base base #:divisors divisors)))

;; ===================================================================================================
;; Tick combinators

(defproc (ticks-mimic [thunk (-> ticks?)]) ticks?
  (ticks (λ (x-min x-max) ((ticks-layout (thunk)) x-min x-max))
         (λ (x-min x-max ts) ((ticks-format (thunk)) x-min x-max ts))))

(defproc (ticks-scale [t ticks?] [fun invertible-function?]) ticks?
  (match-define (invertible-function f g) fun)
  (match-define (ticks layout format) t)
  (ticks (λ (x-min x-max)
           (define ts (layout (f x-min) (f x-max)))
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
  (ticks (λ (x-min x-max)
           (append (layout x-min x-max)
                   (for/list ([x  (in-list xs)])
                     (pre-tick x major?))))
         format))

(defproc (linear-scale [m rational?] [b rational? 0]) invertible-function? #:document-body
  (invertible-function (λ (x) (+ (* m x) b))
                       (λ (y) (/ (- y b) m))))

;; ===================================================================================================
;; Tick utils

(define (same-label? t1 t2) (string=? (tick-label t1) (tick-label t2)))

(define (collapse-equiv-ticks ts near-format-string)
  (match-define (list (tick xs majors labels) ...) ts)
  (define x (/ (apply + xs) (length ts)))
  (define major? (ormap values majors))
  (define label1 (first labels))
  (define label2 (last labels))
  (define label
    (cond [(string=? label1 label2)  label1]
          [else  (format near-format-string label1 label2)]))
  (tick x major? label))

(defproc (collapse-ticks [ts (listof tick?)] [near? (tick? tick? . -> . boolean?)]
                         [near-format-string string? "~a|~a"]) (listof tick?)
  (let ([ts  (sort ts < #:key pre-tick-value)])
    (append*
     (for/list ([ts  (in-list (group-neighbors
                               ts (λ (t1 t2) (or (same-label? t1 t2) (near? t1 t2)))))])
       (define n (length ts))
       (define m (count pre-tick-major? ts))
       (cond [(n . <= . 1)  ts]
             [(m . = . 0)  (list (collapse-equiv-ticks ts near-format-string))]
             [(m . = . 1)  (filter pre-tick-major? ts)]
             [else  (list (collapse-equiv-ticks (filter pre-tick-major? ts) near-format-string))])))))

(defproc (pre-tick-inexact->exact [t pre-tick?]) pre-tick?
  (match-define (pre-tick x major?) t)
  (pre-tick (inexact->exact x) major?))

(defproc (tick-inexact->exact [t tick?]) tick?
  (match-define (tick x major? label) t)
  (tick (inexact->exact x) major? label))

(defproc (contour-ticks [z-ticks ticks?] [z-min real?] [z-max real?]
                        [levels (or/c 'auto exact-positive-integer? (listof real?))]
                        [intervals? boolean?]) (listof tick?)
  (define epsilon (expt 10 (- (digits-for-range z-min z-max))))
  (match-define (ticks layout format) z-ticks)
  ;; initial tick layout
  (define ts
    (cond [(eq? levels 'auto)  (filter pre-tick-major? (layout z-min z-max))]
          [else  (define zs (cond [(list? levels)  (filter (λ (z) (<= z-min z z-max)) levels)]
                                  [else  (linear-seq z-min z-max levels #:start? #f #:end? #f)]))
                 (map (λ (z) (pre-tick z #t)) zs)]))
  (let* (;; remove z-min tick (or the one close to it) if present
         [ts  (if (and (not (empty? ts))
                       ((abs (- z-min (pre-tick-value (first ts)))) . < . epsilon))
                  (rest ts)
                  ts)]
         ;; remove z-max tick (or the one close to it) if present
         [ts  (if (and (not (empty? ts))
                       ((abs (- z-max (pre-tick-value (last ts)))) . < . epsilon))
                  (drop-right ts 1)
                  ts)]
         ;; add z-min and z-max if doing intervals
         [ts  (cond [(not intervals?)  ts]
                    [else  (append (list (pre-tick z-min #t)) ts (list (pre-tick z-max #t)))])])
    ;; format the ticks
    (match-define (list (pre-tick zs majors) ...) ts)
    (define labels (format z-min z-max ts))
    (map tick zs majors labels)))

(defproc (format-tick-labels [x-ticks ticks?] [x-min real?] [x-max real?] [xs (listof real?)]
                             ) (listof string?)
  (match-define (ticks layout format) x-ticks)
  (let* ([tick-xs  (map pre-tick-value (filter pre-tick-major? (layout x-min x-max)))]
         [tick-xs  (remove* xs tick-xs)]
         [tick-xs  (if (empty? tick-xs) empty (list (apply min tick-xs) (apply max tick-xs)))]
         [tick-xs  (sort (append xs tick-xs) <)])
    (define ts (map (λ (x) (pre-tick x #t)) tick-xs))
    (for/list ([x  (in-list tick-xs)]
               [l  (in-list (format x-min x-max ts))]
               #:when (member x xs))
      l)))
