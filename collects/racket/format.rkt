#lang racket/base
(require racket/contract/base
         racket/list)

;; TO DO:
;;  - avoid unnecessary intermediate strings
;;  - see "Printing Floating-Point Numbers Quickly and Accurately"
;;    by Berger & Dybvig, PLDI 1996 for ideas

;; MAYBE TO DO:
;;  - recur into lists, vectors, etc?
;;  - decimal separators (see "man 7 locale", lconv numeric fields)
;;  - something for monetary amounts (see strfmon)?
;;    (perhaps as separate library)
;;  - prop:..., separate from prop:custom-write?

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

(define format/c
  (->* ()
       (#:separator string?
        #:width (or/c exact-nonnegative-integer? #f)
        #:max-width (or/c exact-nonnegative-integer? +inf.0)
        #:min-width exact-nonnegative-integer?
        #:limit-marker string?
        #:align align-mode/c
        #:pad-string padding/c
        #:left-pad-string padding/c
        #:right-pad-string padding/c)
       #:rest list?
       string?))

(provide/contract
 [~a format/c]
 [~s format/c]
 [~v format/c]
 [~e format/c]
 [~.a format/c]
 [~.s format/c]
 [~.v format/c]
 [~r (->* (rational?)
          (#:notation (or/c 'positional 'exponential
                            (-> rational? (or/c 'positional 'exponential)))
           #:sign sign-mode/c
           #:base base/c
           #:precision precision/c
           #:format-exponent (or/c #f string? (-> exact-integer? string?))
           #:min-width exact-positive-integer?
           #:pad-string padding/c)
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

(define ((%cat who fmt [default-sep " "] [default-limit-marker "..."])
         #:width [width #f]
         #:max-width [limit (or width +inf.0)]
         #:limit-marker [limit-marker default-limit-marker]
         #:min-width [pad-to (or width 0)]
         #:align [align 'left]
         #:pad-string [padding " "]
         #:right-pad-string [right-padding padding]
         #:left-pad-string [left-padding padding]
         #:separator [sep default-sep]
         . s)
  (do-checks who limit limit-marker pad-to)
  (%pad (%limit (if (and (pair? s) (null? (cdr s)))
                    (fmt (car s))
                    (apply string-append 
                           (let ([s (map fmt s)])
                             (if (equal? sep "")
                                 s
                                 (add-between s sep)))))
                #:limit limit
                #:limit-marker limit-marker)
        #:pad-to pad-to
        #:align align
        #:left-padding left-padding
        #:right-padding right-padding))

(define ~a (%cat '~a (lambda (v) (if (string? v) v (format "~a" v))) "" ""))
(define ~s (%cat '~s (lambda (v) (format "~s" v))))
(define ~v (%cat '~v (lambda (v) (format "~v" v))))
(define ~e (%cat '~e (lambda (v) (format "~e" v))))

(define ~.a (%cat '~.a (lambda (v) (format "~.a" v)) "" ""))
(define ~.s (%cat '~.s (lambda (v) (format "~.s" v))))
(define ~.v (%cat '~.v (lambda (v) (format "~.v" v))))

;; ----

(define not-supplied (gensym))

(define (extract p) (if (list? p) (cadr p) p))

(define (~r N
            #:sign [sign-mode #f]
            #:base [base 10]
            #:precision [precision 6]
            #:notation [notation 'positional]
            #:format-exponent [exp-format-exponent #f]
            #:min-width [pad-digits-to 1]
            #:pad-string [digits-padding " "])
  (let ([notation (if (procedure? notation) (notation N) notation)])
    (case notation
      ((exponential)
       (catne N
              #:who 'catn
              #:sign sign-mode
              #:base base
              #:precision precision
              #:format-exponent exp-format-exponent
              #:pad-digits-to pad-digits-to
              #:digits-padding digits-padding))
      ((positional)
       (catnp N
              #:who 'catn
              #:sign sign-mode
              #:base base
              #:precision precision
              #:pad-digits-to pad-digits-to
              #:digits-padding digits-padding)))))

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
            (%exponential N-abs base upper? format-exponent precision exactly?)]
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
  (define-values (Nw Nf) (decompose-positional N-abs base precision))
  (let* ([whole-part (number->string* Nw base upper?)]
         [frac-part
          (cond [(and exactly? (= precision 0)) ""]
                [exactly? (number->fraction-string Nf base upper? precision)]
                [(= Nf 0) #f]
                [else
                 (let-values ([(needed-precision Nf*)
                               (reduce-precision base precision Nf)])
                   (number->fraction-string Nf* base upper? needed-precision))])]
         [digits-part
          (cond [frac-part (string-append whole-part "." frac-part)]
                [else whole-part])])
    digits-part))

;; decompose-positional : nonnegative-real positive-nat nat -> (values nat nat)
;; Returns (values whole fraction) where
;;   N-abs is approximately (+ whole (/ fraction (expt base precision)))
(define (decompose-positional N-abs base precision)
  (let* ([Nw (inexact->exact (floor N-abs))]
         [Nf (- N-abs Nw)]
         [base^prec (expt base precision)]
         [Nf* (inexact->exact (round* (* Nf base^prec)))])
    (cond [(< Nf* base^prec)
           (values Nw Nf*)]
          [else
           (values (add1 Nw) 0)])))

;; reduce-precision : nat nat nat -> (values nat nat)
;; Returns (values needed-precision N*) where
;;   (/ N (expt base precision)) = (/ N* (expt base needed-precision))
(define (reduce-precision base precision N)
  (if (zero? N)
      (values 0 0)
      (let loop ([np precision] [N* N])
        (let-values ([(q r) (quotient/remainder N* base)])
          (cond [(zero? r) (loop (sub1 np) q)]
                [else (values np N*)])))))

(define (%exponential N-abs base upper? format-exponent significand-precision exactly?)
  (define-values (N* e-adjust actual-precision)
    (scale N-abs base significand-precision exactly?))
  ;; hack: from 1234 want "1.234"; convert to "1234", mutate to ".234" after saving "1"
  (let* ([digits (number->string* N* base upper?)]
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
    (string-append (make-string (max 0 (- precision (string-length s))) #\0) s)))

;; Allow base up to 36!
(define (get-digit d upper?)
  (cond [(< d 10) (integer->char (+ d (char->integer #\0)))]
        [else (integer->char (+ (- d 10) (char->integer (if upper? #\A #\a))))]))

;; round* : nonnegative-real -> nonnegative-integer (preserving exactness)
;; Implements "round half up" rounding (thus this library formats using
;; "round half away from zero", since it applies round* to absolute values)
(define (round* x) ;; round is round-to-even :(
  (if (integer? x)
      x
      (truncate (+ x 1/2))))
