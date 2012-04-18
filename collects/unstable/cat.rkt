#lang racket/base
(require racket/contract/base)

;; cat = "consider as text" :)

;; TO DO:
;;  - avoid unnecessary intermediate strings
;;  - see "Printing Floating-Point Numbers Quickly and Accurately"
;;    by Berger & Dybvig, PLDI 1996 for ideas

;; MAYBE TO DO:
;;  - rename 'cat' -> 'catd' ("cat like display") and make 'cat'
;;    recur into lists, vectors, etc?
;;  - decimal separators (see "man 7 locale", lconv numeric fields)
;;  - 'catmon' : like 'catn' but for monetary amounts (see strfmon)?
;;    (perhaps as separate library)
;;  - prop:cat, separate from prop:custom-write?

(define (non-empty-string? x)
  (and (string? x) (positive? (string-length x))))

(define align-mode/c
  (or/c 'left 'right 'center))
(define padding/c non-empty-string?)

(define sign-mode/c
  (or/c #f '+ '++ 'parens
        (let ([ind/c (or/c string? (list/c string? string?))])
          (list/c ind/c ind/c ind/c))))

(define base/c
  (or/c (integer-in 2 36)
        (list/c 'up (integer-in 2 36))))

;; Precision is one of
;;  - Nat, for "up to N"
;;  - '(= N), for "exactly N"
(define precision/c
  (or/c exact-nonnegative-integer?
        (list/c '= exact-nonnegative-integer?)))

(define cat-n-c
  (->* ()
       (#:width (or/c exact-nonnegative-integer? #f)
        #:limit (or/c exact-nonnegative-integer? +inf.0)
        #:limit-marker string?
        #:pad-to exact-nonnegative-integer?
        #:align align-mode/c
        #:padding padding/c
        #:left-padding padding/c
        #:right-padding padding/c)
       #:rest list?
       string?))

(define cat-1-c
  (->* (any/c)
       (#:width (or/c exact-nonnegative-integer? #f)
        #:limit (or/c exact-nonnegative-integer? +inf.0)
        #:limit-marker string?
        #:pad-to exact-nonnegative-integer?
        #:align align-mode/c
        #:padding padding/c
        #:left-padding padding/c
        #:right-padding padding/c)
       string?))

(provide/contract
 [cat cat-n-c]
 [catw cat-1-c]
 [catp cat-1-c]
 [catn
  (->* (rational?)
       (#:sign sign-mode/c
        #:base base/c
        #:precision precision/c
        #:pos/exp-range (list/c (or/c exact-integer? +inf.0)
                                (or/c exact-integer? -inf.0))
        #:exp-precision precision/c
        #:exp-format-exponent (or/c #f string? (-> exact-integer? string?))
        #:pad-digits-to exact-positive-integer?
        #:digits-padding padding/c)
       string?)]
 [catnp
  (->* (rational?)
       (#:sign sign-mode/c
        #:base base/c
        #:precision precision/c
        #:pad-digits-to exact-positive-integer?
        #:digits-padding padding/c)
       string?)]
 [catne
  (->* (rational?)
       (#:sign sign-mode/c
        #:base base/c
        #:precision precision/c
        #:format-exponent (or/c #f string? (-> exact-integer? string?))
        #:pad-digits-to exact-positive-integer?
        #:digits-padding padding/c)
       string?)])

;; ----------------------------------------

(define (%limit #:limit limit
                #:limit-marker limit-marker
                s)
  (cond [(> (string-length s) limit)
         (string-append (substring s 0 (- limit (string-length limit-marker)))
                        limit-marker)]
        [else s]))

(define (%pad #:pad-to pad-to
              #:align align-mode
              #:left-padding left-padding
              #:right-padding right-padding
              s)
  (cond [(< (string-length s) pad-to)
         (let* ([s-length (string-length s)]
                [to-pad-length (max 0 (- pad-to s-length))])
           (let-values ([(left-pad-length right-pad-length)
                         (case align-mode
                           ((left) (values 0 to-pad-length))
                           ((right) (values to-pad-length 0))
                           ((center)
                            (values (floor (/ to-pad-length 2))
                                    (ceiling (/ to-pad-length 2)))))])
             (string-append
              (build-padding 'left left-padding left-pad-length)
              s
              (build-padding 'right right-padding right-pad-length))))]
        [else s]))

(define (build-padding side padding pad-length)
  (cond [(zero? pad-length) ""]
        [(char? padding)
         (make-string pad-length padding)]
        [(and (string? padding) (= (string-length padding) 1))
         (make-string pad-length (string-ref padding 0))]
        [(string? padding)
         (let* ([pattern padding]
                [pattern-length (string-length pattern)]
                [whole-copies (quotient pad-length pattern-length)]
                [part-length (remainder pad-length pattern-length)]
                [pattern-copies (for/list ([i (in-range whole-copies)]) pattern)])
           (apply string-append
                  ;; For left, start at start of string
                  ;; For right, end at end of string.
                  (case side
                    ((left)
                     (append pattern-copies
                             (list (substring pattern 0 part-length))))
                    ((right)
                     (cons (substring pattern (- pattern-length part-length) pattern-length)
                           pattern-copies)))))]))

(define (do-checks who limit limit-marker width)
  (when (> width limit)
    (error who "pad-to length greater than limit (~s): ~s" limit width))
  (when (> (string-length limit-marker) limit)
    (error who "limit-marker string longer than limit (~s): ~e"
           limit limit-marker)))

;; ----------------------------------------

(define (%cat s
              #:who who
              #:limit limit
              #:limit-marker limit-marker
              #:pad-to pad-to
              #:align align
              #:right-padding right-padding
              #:left-padding left-padding)
  (do-checks who limit limit-marker pad-to)
  (%pad (%limit (if (list? s) (apply string-append s) s)
                #:limit limit
                #:limit-marker limit-marker)
        #:pad-to pad-to
        #:align align
        #:left-padding left-padding
        #:right-padding right-padding))

(define (cat #:width [width #f]
             ;; I was greatly tempted to name this keyword option #:nip instead
             ;; (or maybe #:nip-to)
             #:limit [limit (or width +inf.0)]
             #:limit-marker [limit-marker "..."]
             #:pad-to [pad-to (or width 0)]
             #:align [align 'left]
             #:padding [padding " "]
             #:right-padding [right-padding padding]
             #:left-padding [left-padding padding]
             . vs)
  (%cat (map (lambda (v) (if (string? v) v (format "~a" v))) vs)
        #:who 'cat
        #:limit limit
        #:limit-marker limit-marker
        #:pad-to pad-to
        #:align align
        #:right-padding right-padding
        #:left-padding left-padding))

(define (catw #:width [width #f]
              #:limit [limit (or width +inf.0)]
              #:limit-marker [limit-marker "..."]
              #:pad-to [pad-to (or width 0)]
              #:align [align 'left]
              #:padding [padding " "]
              #:right-padding [right-padding padding]
              #:left-padding [left-padding padding]
              v)
  (%cat (format "~s" v)
        #:who 'catw
        #:limit limit
        #:limit-marker limit-marker
        #:pad-to pad-to
        #:align align
        #:right-padding right-padding
        #:left-padding left-padding))

(define (catp #:width [width #f]
              #:limit [limit (or width +inf.0)]
              #:limit-marker [limit-marker "..."]
              #:pad-to [pad-to (or width 0)]
              #:align [align 'left]
              #:padding [padding " "]
              #:right-padding [right-padding padding]
              #:left-padding [left-padding padding]
              v)
  (%cat (format "~v" v)
        #:who 'cat
        #:limit limit
        #:limit-marker limit-marker
        #:pad-to pad-to
        #:align align
        #:right-padding right-padding
        #:left-padding left-padding))

;; ----

(define (catn N
              #:sign [sign-mode #f]
              #:base [base 10]
              #:precision [precision 3]
              #:pos/exp-range [pos/exp-range #f]
              #:exp-precision [exp-precision 5]
              #:exp-format-exponent [exp-format-exponent #f]
              #:pad-digits-to [pad-digits-to 1]
              #:digits-padding [digits-padding " "])
  (let* ([N-abs (abs N)]
         [positional?
          (or (zero? N-abs)
              (not pos/exp-range)
              (let ([max-neg-exp (car pos/exp-range)]
                    [min-pos-exp (cadr pos/exp-range)])
                (< (expt base max-neg-exp) N-abs (expt base min-pos-exp))))])
    (if positional?
        (catnp N
               #:who 'catn
               #:sign sign-mode
               #:base base
               #:precision precision
               #:pad-digits-to pad-digits-to
               #:digits-padding digits-padding)
        (catne N
               #:who 'catn
               #:sign sign-mode
               #:base base
               #:precision exp-precision
               #:format-exponent exp-format-exponent
               #:pad-digits-to pad-digits-to
               #:digits-padding digits-padding))))

(define (catnp N
               #:who [who 'catnp]
               #:sign [sign-mode #f]
               #:base [base 10]
               #:precision [precision 3]
               #:pad-digits-to [pad-digits-to 1]
               #:digits-padding [digits-padding " "])
  ;; precision: up to (or exactly) this many digits after decimal point
  ;;   precision = 0 means no decimal point
  ;;   precision = '(= 0) means keep decimal point
  ;; pad-digits-to: includes decimal point, doesn't include sign
  (let*-values ([(upper? base) (normalize-base base)]
                [(exactly? precision) (normalize-precision precision)])
    (let* ([N-abs (abs N)]
           [digits-part (%positional N-abs base upper? precision exactly?)]
           [padded-digits-part
            (%pad digits-part
                  #:pad-to pad-digits-to
                  #:align 'right
                  #:left-padding digits-padding
                  #:right-padding #f)])
      (let-values ([(pre-sign-part post-sign-part) (get-sign-parts N sign-mode)])
        (string-append pre-sign-part padded-digits-part post-sign-part)))))

(define (catne N
               #:who [who 'catne]
               #:sign [sign-mode #f]
               #:base [base 10]
               #:precision [precision 5]
               #:format-exponent [format-exponent #f]
               #:pad-digits-to [pad-digits-to 1]
               #:digits-padding [digits-padding " "])
  (let*-values ([(upper? base) (normalize-base base)]
                [(exactly? precision) (normalize-precision precision)])
    (let* ([N-abs (abs N)]
           [digits-part
            (%exponential N-abs base format-exponent precision exactly?)]
           [padded-digits-part
            (%pad digits-part
                  #:pad-to pad-digits-to
                  #:align 'right
                  #:left-padding digits-padding
                  #:right-padding #f)])
      (let-values ([(pre-sign-part post-sign-part) (get-sign-parts N sign-mode)])
        (string-append pre-sign-part padded-digits-part post-sign-part)))))

(define (normalize-base base)
  (if (pair? base)
      (values (eq? (car base) 'up) (cadr base))
      (values #f base)))

(define (normalize-precision precision)
  (if (pair? precision)
      (values #t (cadr precision))
      (values #f precision)))

(define (%positional N-abs base upper? precision exactly?)
  (let* ([Nw (inexact->exact (floor N-abs))]
         [Nf (- N-abs Nw)]
         [whole-part (number->string* Nw base upper?)]
         [frac-part
          (let* ([Nf* (inexact->exact (round (* Nf (expt base precision))))])
            (cond [(and exactly? (= precision 0)) ""]
                  [exactly? (number->fraction-string Nf* base upper? precision)]
                  [(= Nf* 0) #f]
                  [else
                   (let-values ([(needed-precision Nf**)
                                 (let loop ([np precision] [Nf* Nf*])
                                   (let-values ([(q r) (quotient/remainder Nf* base)])
                                     (cond [(zero? r) (loop (sub1 np) q)]
                                           [else (values np Nf*)])))])
                     (number->fraction-string Nf** base upper? needed-precision))]))]
         [digits-part
          (cond [frac-part (string-append whole-part "." frac-part)]
                [else whole-part])])
    digits-part))

(define (%exponential N-abs base format-exponent significand-precision exactly?)
  (define-values (N* e-adjust actual-precision)
    (scale N-abs base significand-precision exactly?))
  ;; hack: from 1234 want "1.234"; convert to "1234", mutate to ".234" after saving "1"
  (let* ([digits (number->string* N* base #f)]
         [leading-digit (string (string-ref digits 0))]
         [exponent (- significand-precision e-adjust)])
    (string-set! digits 0 #\.)
    (string-append leading-digit
                   (if (or exactly? (positive? actual-precision)) digits "")
                   (cond [(procedure? format-exponent)
                          (format-exponent exponent)]
                         [else
                          (string-append
                           (cond [(string? format-exponent) format-exponent]
                                 [(= base 10) "e"]
                                 [else (format "×~s^" base)])
                           (if (negative? exponent) "-" "+")
                           (%pad (number->string (abs exponent))
                                 #:pad-to 2
                                 #:align 'right
                                 #:left-padding "0"
                                 #:right-padding #f))]))))

(define (scale N-abs base significand-precision exactly?)
  (if (zero? N-abs)
      (values 0 0 (if exactly? significand-precision 0))
      (scale/nz N-abs base significand-precision exactly?)))

(define (scale/nz N-abs base significand-precision exactly?)
  (let* ([N (inexact->exact N-abs)]
         [normalized-min (expt base significand-precision)]
         [normalized-max (* base normalized-min)])
    (let*-values ([(N*0 e-adjust0)
                   (let ([e-est (- significand-precision
                                   (inexact->exact (floor (/ (log N-abs) (log base)))))])
                     (values (* N (expt base e-est)) e-est))]
                  [(N* e-adjust)
                   (let loop ([N N*0] [e e-adjust0] [r #f])
                     ;; if r != #f, then N is integer
                     (cond [(< N normalized-min)
                            (loop (* N base) (add1 e) #f)]
                           [(>= N normalized-max)
                            (let-values ([(q r) (quotient/remainder (floor N) base)])
                              (loop q (sub1 e) r))]
                           [else
                            (let ([N* (if r
                                          (if (>= (* 2 r) base) (add1 N) N)
                                          (round* N))])
                              (cond [(< N* normalized-max)
                                     (values N* e)]
                                    [else (loop N* e #f)]))]))]
                  [(N* actual-precision)
                   (if exactly?
                       (values N* significand-precision)
                       (let loop ([N N*] [p significand-precision])
                         (let-values ([(q r) (quotient/remainder N base)])
                           (cond [(zero? r) (loop q (sub1 p))]
                                 [else (values N p)]))))])
      (values N* e-adjust actual-precision))))

;; ----

(define (get-sign-parts N sign-mode)
  (define (get indicator)
    (if (string? indicator)
        (values indicator "")
        (values (car indicator) (cadr indicator))))
  (let ([indicator-table
         (case sign-mode
           ((#f) '(""  ""  "-"))
           ((+)  '("+" ""  "-"))
           ((++) '("+" "+" "-"))
           ((parens) '("" "" ("(" ")")))
           (else sign-mode))])
    (cond [(or (negative? N) (eqv? -0.0 N))
           (get (caddr indicator-table))]
          [(zero? N)
           (get (cadr indicator-table))]
          [else ;; positive
           (get (car indicator-table))])))

(define (number->string* N base upper?)
  (cond [(memv base '(2 8 10 16))
         (let ([s (number->string N base)])
           (if (and (= base 16) upper?)
               (string-upcase s)
               s))]
        [(zero? N)
         (string #\0)]
        [else
         (apply string
                (let loop ([N N] [digits null])
                  (cond [(zero? N) (reverse digits)]
                        [else (let-values ([(q r) (quotient/remainder N base)])
                                (loop q (cons (get-digit r upper?) digits)))])))]))

(define (number->fraction-string N base upper? precision)
  (let ([s (number->string* N base upper?)])
    (string-append (make-string (- precision (string-length s)) #\0) s)))

;; Allow base up to 36!
(define (get-digit d upper?)
  (cond [(< d 10) (integer->char (+ d (char->integer #\0)))]
        [else (integer->char (+ (- d 10) (char->integer (if upper? #\A #\a))))]))

(define (round* x) ;; round is round-to-even :(
  (if (integer? x)
      x
      (+ (truncate x)
         (if (even? (truncate (+ x x))) 0 1))))
