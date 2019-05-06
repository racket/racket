#lang racket/base
(require racket/private/check
         racket/fixnum
         racket/extflonum
         "parse-case.rkt"
         "parameter.rkt"
         ;; Used only to coerce strings to extflonums
         ;; when extflonums are not fully suported:
         (prefix-in host: "../host/string-to-number.rkt"))

(provide string->number
         unchecked-string->number)

;; The `string->number` parser is responsible for handling Racket's
;; elaborate number syntax (mostly inherited from Scheme). It relies
;; on a host-system `string->number` only for generating
;; psuedo-extflonums when flonums aren't really supported. Otherwise,
;; the parser here performs all checking and arithmetic that the
;; reader needs.

(define/who (string->number s
                            [radix 10]
                            [convert-mode 'number-or-false]
                            [decimal-mode (if (read-decimal-as-inexact)
                                              'decimal-as-inexact
                                              'decimal-as-exact)])
  (check who string? s)
  (check who (lambda (p) (and (exact-integer? radix)
                              (<= 2 radix 16)))
         #:contract "(integer-in 2 16)"
         radix)
  (check who (lambda (p) (or (eq? p 'number-or-false)
                             (eq? p 'read)))
         #:contract "(or/c 'number-or-false 'read)"
         convert-mode)
  (check who (lambda (p) (or (eq? p 'decimal-as-inexact)
                             (eq? p 'decimal-as-exact)))
         #:contract "(or/c 'decimal-as-inexact decimal-as-exact)"
         decimal-mode)
  (unchecked-string->number s radix convert-mode decimal-mode))

(define (unchecked-string->number s radix convert-mode decimal-mode)
  (do-string->number s 0 (string-length s)
                     radix #:radix-set? #f
                     decimal-mode
                     convert-mode))

;; ----------------------------------------

(struct parse-state (exactness        ; see below
                     convert-mode     ; 'number-or-false, 'read, or 'must-read
                     fst              ; rect-prefix, polar-prefix, '+/- if started with sign, or #f
                     other-exactness) ; exactness to use for the imag part or saved real part
  #:authentic)

;; `sgn/z` records a sign in case `n` is zero
(struct rect-prefix (sgn/z n start) #:authentic)
(struct polar-prefix (sgn/z n start) #:authentic)

;; Exactness state is one of
;;   - 'exact      ; found "#e"
;;   - 'inexact    ; found "#i"
;;   - 'decimal-as-exact
;;   - 'decimal-as-inexact
;;   - 'approx     ; => was 'decimal-as-inexact and found "." or "#"
;;   - 'single     ; => was 'decimal-as-inexact and found "f"/"s"
;;   - 'double     ; => was 'decimal-as-inexact and found "e"/"d"/"x"
;;   - 'extflonum  ; => was 'decimal-as-inexact and found "t"
;;   - 'extflonum->inexact  ; => was 'inexact and found "t"
;;   - 'extflonum->exact    ; => was 'exact and found "t"

(define (init-state exactness convert-mode fst)
  (parse-state exactness convert-mode fst exactness))

(define (state-has-first-half? state)
  (define fst (parse-state-fst state))
  (and fst (not (eq? fst '+/-))))

(define (state-set-first-half state fst)
  (struct-copy parse-state state
               [fst fst]
               [exactness (parse-state-other-exactness state)]
               [other-exactness (parse-state-exactness state)]))

(define (state-first-half state)
  (init-state (parse-state-other-exactness state)
              (parse-state-convert-mode state)
              #f))

(define (state-second-half state)
  (init-state (parse-state-exactness state)
              (parse-state-convert-mode state)
              #f))

;; ----------------------------------------

;; When parsing fails, either return an error string or #f. An error
;; string is reported only in 'read mode and when if we're somehow
;; onligated to parse as a number, such as after `#i`. As a
;; convenience, `state` can be just a convert-mode symbol.
(define-syntax-rule (fail state msg arg ...)
  (cond
    [(eq? (state->convert-mode state) 'must-read)
     (format msg arg ...)]
    [else #f]))

(define (state->convert-mode state)
  (if (parse-state? state) (parse-state-convert-mode state) state))

(define (state->dbz-convert-mode state)
  (define convert-mode (parse-state-convert-mode state))
  (if (eq? convert-mode 'read)
      'must-read
      convert-mode))

(define (bad-digit c s state)
  (cond
    [(char=? c #\nul)
     (fail state "nul character in `~.a`" s)]
    [else
     (fail state "bad digit `~a`" c)]))

(define (bad-mixed-decimal-fraction s state)
  (fail state "decimal points and fractions cannot be mixed in `~.a`" s))

(define (bad-misplaced what s state)
  (fail state "misplaced `~a` in `~.a`" what s))

(define (bad-no-digits after s state)
  (fail state "missing digits after `~a` in `~.a`" after s))

(define (bad-extflonum-for-complex i s state)
  (fail state "cannot combine extflonum `~a` into a complex number" i))

;; For chaining a potentially failing parse/conversion with more:
(define-syntax-rule (maybe e k)
  (let ([v e])
    (if (or (not v) (string? v))
        v
        (k v))))

;; ----------------------------------------

;; Lazy exponentiation and devision lets us avoid
;; extremely large bignums when we're trying to
;; compute an inexact number that will just be
;; infinity
(struct lazy-expt (n radix exp)
  #:authentic)
(struct lazy-rational (n d)
  #:authentic)

(define (lazy-number n radix exp)
  (cond
    [(eq? n 'dbz) n]
    [(eq? n 'dbz!) n]
    [else
     (if (and (exp . < . 30)
              (exp . > . -30))
         (* n (expt radix exp))
         (lazy-expt n radix exp))]))

(define (lazy-divide n d d-exactness)
  (cond
    [(eqv? d 0) (if (eq? d-exactness 'exact)
                    'dbz!
                    'dbz)]
    [(or (lazy-expt? n)
         (lazy-expt? d))
     (lazy-rational n d)]
    [else (/ n d)]))

(define (simplify-lazy-divide n0)
  (cond
    [(lazy-rational? n0)
     (define n (lazy-rational-n n0))
     (define d (lazy-rational-d n0))
     (define n-n (if (lazy-expt? n) (lazy-expt-n n) n))
     (define n-exp (if (lazy-expt? n) (lazy-expt-exp n) 0))
     (define d-n (if (lazy-expt? d) (lazy-expt-n d) d))
     (define d-exp (if (lazy-expt? d) (lazy-expt-exp d) 0))
     (define radix (if (lazy-expt? n) (lazy-expt-radix n) (lazy-expt-radix d)))
     (lazy-number (/ n-n d-n) radix (- n-exp d-exp))]
    [else n0]))

(define (force-lazy-exact n0 state s)
  (define n (simplify-lazy-divide n0))
  (cond
    [(or (eq? n 'dbz) (eq? n 'dbz!))
     (fail (state->dbz-convert-mode state) "division by zero in `~.a`" s)]
    [(lazy-expt? n)
     (* (lazy-expt-n n) (expt (lazy-expt-radix n) (lazy-expt-exp n)))]
    [else n]))

(define (force-lazy-inexact sgn/z n0 state s [precision 2048])
  (define n1 (simplify-lazy-divide n0))
  (cond
    [(eq? n0 'dbz) (if (fx= sgn/z -1) -inf.0 +inf.0)]
    [(eq? n0 'dbz!)
     (fail (state->dbz-convert-mode state) "division by zero in `~.a`" s)]
    [(lazy-expt? n1)
     (define n (lazy-expt-n n1))
     (define exp (lazy-expt-exp n1))
     (define radix (lazy-expt-radix n1))
     (define approx-expt (+ (/ (if (integer? n)
                                   (integer-length n)
                                   (- (integer-length (numerator n))
                                      (integer-length (denominator n))))
                               (log radix 2))
                            exp))
     (cond
       [(eqv? n 0) (if (fx= sgn/z -1) (- 0.0) 0.0)]
       [(approx-expt . > . precision) (if (fx= sgn/z -1) -inf.0 +inf.0)]
       [(approx-expt . < . (- precision)) (if (fx= sgn/z -1) (- 0.0) 0.0)]
       [else
        (* n (expt radix exp))])]
    [(eqv? n1 0) (if (fx= sgn/z -1) (- 0.0) 0.0)]
    [else n1]))

(define (fast-inexact state sgn n radix exp sgn2 exp2)
  (case (parse-state-exactness state)
    [(double approx)
     (cond
       [(state-has-first-half? state) #f]
       [(eqv? n 0) (if (fx= sgn 1) 0.0 (- 0.0))]
       [(and (fixnum? n)
             (n . < . (expt 2 50))
             (n . > . (- (expt 2 50))))
        ;; No loss of precision in mantissa from early flonum conversion
        (let ([exp (+ exp (* sgn2 exp2))]
              [m (fx->fl (if (fx= sgn -1)
                             (fx- 0 n)
                             n))])
          (cond
            [(eqv? exp 0) m]
            [(not (fixnum? exp)) #f]
            [else
             (define fradix (if (fx= radix 10)
                                10.0
                                (fx->fl radix)))
             (cond
               [(exp . fx< . 0)
                ;; Stay well away from limits on the exponent to make
                ;; sure there's still no loss of precision. We could
                ;; use `(integer-length n)` to improve the bounds,
                ;; but this seems good enough for the common case.
                (and (exp . fx> . (cond
                                    [(radix . fx<= . 10) -300]
                                    [else -240]))
                     (/ m (expt fradix (fx- 0 exp))))]
               [else (* m (expt fradix exp))])]))]
       [else #f])]
    [else #f]))

;; The `sgn/z` argument lets us produce -0.0 instead of 0.0 as needed
;; when converting an exact zero to inexact. That is, the sign is `-1`
;; when the input has a literal "-", but it's only used when `n` is 0.
(define (finish sgn/z n s state
                ;; Used only when we have to resort to host:string->number:
                #:range [range #f])
  (define fst (parse-state-fst state))
  (cond
    [(or (not fst) (eq? fst '+/-))
     (case (parse-state-exactness state)
       [(single)
        (maybe (force-lazy-inexact sgn/z n state s)
               (lambda (r)
                 (real->single-flonum r)))]
       [(exact)
        (case n
          [(+inf.0 -inf.0 +nan.0)
           (fail state "no exact representation for ~a" n)]
          [else
           (maybe (force-lazy-exact n state s)
                  (lambda (r) (inexact->exact r)))])]
       [(extended)
        (cond
          [(eq? (parse-state-convert-mode state) 'number-or-false)
           #f]
          [(extflonum-available?)
           (maybe (force-lazy-inexact sgn/z n state s 32768)
                  (lambda (r)
                    (real->extfl r)))]
          [else
           (define trim-s (trim-number s
                                       (if range (car range) 0)
                                       (if range (cdr range) (string-length s))))
           (host:string->number trim-s 10 'read)])]
       [(double inexact approx)
        (maybe (force-lazy-inexact sgn/z n state s)
               (lambda (r0)
                 (exact->inexact r0)))]
       [(extflonum->inexact)
        (fail state "cannot convert extflonum to inexact in `~a`" s)]
       [(extflonum->exact)
        (fail state "cannot convert extflonum to exact in `~a`" s)]
       [else (force-lazy-exact n state s)])]
    [(polar-prefix? fst)
     (define pos (polar-prefix-start fst))
     (define m (finish (polar-prefix-sgn/z fst) (polar-prefix-n fst) s (state-first-half state)
                       #:range (cons 0 pos)))
     (define a (finish sgn/z n s (state-second-half state)
                       #:range (cons pos (string-length s))))
     ;; extflonum errors take precedence over errors like divide-by-zero
     (cond
       [(extflonum? m)
        (bad-extflonum-for-complex m s state)]
       [(extflonum? a)
        (bad-extflonum-for-complex a s state)]
       [else
        (maybe m
               (lambda (m)
                 (maybe a
                        (lambda (a)
                          (define cn (make-polar m a))
                          (case (parse-state-exactness state)
                            [(exact) (inexact->exact cn)]
                            [else cn])))))])]
    [fst (fail state "missing `i` for complex number in `~.a`" s)]))

;; Called when we find an "i" that might be at the end of the input
(define (finish-imaginary sgn/z n s start end state)
  (define fst (parse-state-fst state))
  (cond
    [(and (eq? fst '+/-)
          (fx= start end))
     ;; Just an imaginary part, ok since the input started "+" or "-"
     (maybe (finish sgn/z n s state)
            (lambda (i)
              (cond
                [(extflonum? i)
                 (bad-extflonum-for-complex i s state)]
                [else
                 (define zero
                   (case (parse-state-other-exactness state)
                     [(inexact) 0.0]
                     [else 0]))
                 (make-rectangular zero i)])))]
    [(and (rect-prefix? fst)
          (fx= start end))
     (define pos (rect-prefix-start fst))
     (define r (finish (rect-prefix-sgn/z fst) (rect-prefix-n fst) s (state-first-half state)
                       #:range (cons 0 pos)))
     (define i (finish sgn/z n s (state-second-half state)
                       #:range (cons pos (string-length s))))
     ;; extflonum errors take precedence over other errors (such as divide-by-zero)
     (cond
       [(extflonum? r)
        (bad-extflonum-for-complex r s state)]
       [(extflonum? i)
        (bad-extflonum-for-complex r i state)]
       [else
        (maybe r
               (lambda (r)
                 (maybe i
                        (lambda (i)
                          (make-rectangular r i)))))])]
    [else
     (bad-misplaced "i" s state)]))

;; Given a current exactness and an inferred exactness, combine the
;; two specifications
(define (set-exactness state new-exactness #:override? [override? #f])
  (define exactness (parse-state-exactness state))
  (define result-exactness
    (case new-exactness
      [(single double)
       (case exactness
         [(exact) 'exact]
         [(decimal-as-exact) (if override?
                                 new-exactness
                                 'decimal-as-exact)]
         [else new-exactness])]
      [(approx)
       (case exactness
         [(exact inexact decimal-as-exact) exactness]
         [else new-exactness])]
      [(extended)
       ;; extended mode always overrides
       (case exactness
         [(inexact) 'extflonum->inexact]
         [(exact) 'extflonum->exact]
         [else 'extended])]
      [else new-exactness]))
  (if (eq? exactness result-exactness)
      state
      (struct-copy parse-state state
                   [exactness result-exactness])))

(define (set-exactness-by-char state c #:override? [override? #f])
  (set-exactness
   state
   (case c
     [(#\e #\E #\d #\D #\l #\L #\0) 'double]
     [(#\f #\F #\s #\S) 'single]
     [(#\t #\T) 'extended])
   #:override? override?))

;; When we have to use `host:string->number` to deal with extflonums,
;; we need to extract the right part of the string. Remove any '#'
;; from the front and any 'i' at the end.
(define (trim-number s start end)
  (cond
    [(eqv? (string-ref s start) #\#)
     (trim-number s (fx+ 2 start) end)]
    [(eqv? (string-ref s (fx- end 1)) #\i)
     (trim-number s start (fx- end 1))]
    [else (substring s start end)]))

;; ----------------------------------------

;; The parser is implemented as a kind of state machine that is driven
;; by the next input character. The current function mostly represents
;; the state. Some state is in other arguments -- especially the
;; `state` argument, obviously --- to avoid duplicating all functions
;; for similar states, such as parsing a number in the real or
;; imaginary position of a complex number.

;; The `convert-mode` argument here can be 'number-or-false, 'read, or
;; 'must-read, where 'must-read reports an error on parsing failure
;; instead of returning #f. At this level, we mostly detect the
;; special numbers `+inf.0` in combinations, and otherwise dispatch
;; to parsing a complex number, fraction, or exponential.
(define (do-string->number s start end
                           radix #:radix-set? radix-set?
                           exactness ; 'inexact, 'exact, 'decimal-as-inexact, or 'decimal-as-exact
                           convert-mode)
  (parse-case
   s start end radix => c
   [(eof)
    (fail convert-mode "no digits")]
   [(digit)
    (read-integer 1 c s (fx+ 1 start) end radix (init-state exactness convert-mode #f))]
   [(#\#)
    (define next (fx+ 1 start))
    (parse-case
     ;; use `10` instead of `radix`, because we don't want a hex conversion
     s next end 10 => i
     [(eof)
      (fail convert-mode "no character after `#` indicator in `~.a`" s)]
     [(#\e #\E #\i #\I)
      (cond
        [(or (eq? exactness 'exact) (eq? exactness 'inexact))
         (fail convert-mode "misplaced exactness specification at `~.a`" (substring s start end))]
        [else
         (do-string->number s (fx+ 1 next) end
                            radix #:radix-set? radix-set?
                            (if (or (char=? i #\e) (char=? i #\E)) 'exact 'inexact)
                            (if (eq? convert-mode 'read) 'must-read convert-mode))])]
     [(#\b #\B #\o #\O #\d #\D #\x #\X)
      (cond
        [radix-set?
         (fail convert-mode "misplaced radix specification at `~.a`" (substring s start end))]
        [else
         (define radix
           (case i
             [(#\b #\B) 2]
             [(#\o #\O) 8]
             [(#\d #\D) 10]
             [else 16]))
         (do-string->number s (fx+ 1 next) end
                            radix #:radix-set? #t
                            exactness
                            (if (eq? convert-mode 'read) 'must-read convert-mode))])]
     [else
      ;; The reader always complains about a bad leading `#`
      (fail (if (eq? convert-mode 'read) 'must-read convert-mode)
            "bad `#` indicator `~a` at `~.a`" i (substring s start end))])]
   [(#\+)
    (read-signed 1 s (fx+ 1 start) end radix (init-state exactness convert-mode '+/-))]
   [(#\-)
    (read-signed -1 s (fx+ 1 start) end radix (init-state exactness convert-mode '+/-))]
   [(#\.)
    (read-decimal 1 #f 0 s (fx+ 1 start) end radix (set-exactness (init-state exactness convert-mode #f) 'approx))]
   [else
    (bad-digit c s convert-mode)]))

;; consumed a "+" or "-"
(define (read-signed sgn s start end radix state)
  (parse-case
   s start end radix => c
   [(eof) (fail state "no digits in `~.a`" s)]
   [(digit)
    (read-integer sgn c s (fx+ 1 start) end radix state)]
   [(#\.)
    (read-decimal sgn #f 0 s (fx+ 1 start) end radix (set-exactness state 'approx))]
   [(#\i #\I)
    ;; maybe "[+-]inf.0"
    (parse-case
     s (fx+ 1 start) end radix => c2
     [(eof)
      (finish-imaginary sgn sgn s (fx+ 1 start) end state)]
     [(#\n #\N)
      (read-infinity sgn c s (fx+ 2 start) end radix state)]
     [else (bad-digit c s state)])]
   [(#\n #\N)
    ;; maybe "[+-]nan.0"
    (read-nan c s (fx+ 1 start) end radix state)]
   [else
    (bad-digit c s state)]))

;; consumed some digits
(define (read-integer sgn n s start end radix state)
  (define (get-n) (* sgn n))
  (parse-case
   s start end radix => c
   [(eof) (finish sgn (get-n) s state)]
   [(digit)
    (read-integer sgn (+ (* n radix) c) s (fx+ 1 start) end radix state)]
   [(#\.)
    (read-decimal sgn n 0 s (fx+ 1 start) end radix (set-exactness state 'approx))]
   [(#\e #\E #\d #\D #\l #\L #\f #\F #\s #\S #\t #\T)
    (read-exponent sgn (get-n) 0 s (fx+ 1 start) end radix (set-exactness-by-char state c))]
   [(#\/)
    (read-rational sgn (get-n) #f s (fx+ 1 start) end radix state)]
   [(#\#)
    (read-approx sgn n 1 #f s (fx+ 1 start) end radix (set-exactness state 'approx))]
   [(#\+ #\-)
    (read-imag c sgn (get-n) (if (eqv? c #\+) +1 -1) s (fx+ 1 start) end radix state)]
   [(#\@)
    (read-polar sgn (get-n) s (fx+ 1 start) end radix state)]
   [(#\i)
    (finish-imaginary sgn (get-n) s (fx+ 1 start) end state)]
   [else
    (bad-digit c s state)]))

;; consumed digits and "."
(define (read-decimal sgn n exp s start end radix state)
  (define (get-n) (if n
                      (lazy-number (* sgn n) radix (- exp))
                      (bad-no-digits "." s state)))
  (parse-case
   s start end radix => c
   [(eof) (or (and n (fast-inexact state sgn n radix 0 -1 exp))
              (maybe (get-n)
                     (lambda (n)
                       (finish sgn n s state))))]
   [(digit)
    (define next (fx+ 1 start))
    (cond
      [(and (eqv? c #\0)
            (fx= next end))
       ;; avoid extra work when ".0" is used to get an inexact zero
       (read-decimal sgn (or n 0) exp s next end radix state)]
      [else
       (read-decimal sgn (+ (* (or n 0) radix) c) (fx+ 1 exp) s (fx+ 1 start) end radix state)])]
   [(#\.)
    (bad-misplaced "." s state)]
   [(#\e #\E #\d #\D #\l #\L #\f #\F #\s #\S #\t #\T)
    (if n
        (read-exponent sgn (* sgn n) (- exp) s (fx+ 1 start) end radix (set-exactness-by-char state c))
        (bad-no-digits "." s state))]
   [(#\/)
    (bad-mixed-decimal-fraction s state)]
   [(#\#)
    (if n
        (read-approx sgn n (fx- 0 exp) #t s (fx+ 1 start) end radix state)
        (bad-misplaced "#" s state))]
   [(#\+ #\-)
    (if n
        (read-imag c sgn (get-n) (if (eqv? c #\+) +1 -1) s (fx+ 1 start) end radix state)
        (bad-no-digits "." s state))]
   [(#\@)
    (maybe (get-n)
           (lambda (n)
             (read-polar sgn n s (fx+ 1 start) end radix state)))]
   [(#\i)
    (maybe (get-n)
           (lambda (n)
             (finish-imaginary sgn n s (fx+ 1 start) end state)))]
   [else
    (bad-digit c s state)]))

;; consumed digits and maybe "." and some "#"s
(define (read-approx sgn n exp saw-.? s start end radix state)
  (define (get-n) (lazy-number (* sgn n) radix exp))
  (parse-case
   s start end radix => c
   [(eof) (finish sgn (get-n) s state)]
   [(digit)
    (bad-misplaced "#" s state)]
   [(#\.)
    (if saw-.?
        (bad-misplaced "." s state)
        (read-approx sgn n exp #t s (fx+ 1 start) end radix state))]
   [(#\#)
    (read-approx sgn n (if saw-.? exp (fx+ 1 exp)) saw-.? s (fx+ 1 start) end radix state)]
   [(#\e #\E #\d #\D #\l #\L #\f #\F #\s #\S #\t #\T)
    (read-exponent sgn (* sgn n) exp s (fx+ 1 start) end radix (set-exactness-by-char state c))]
   [(#\/)
    (if saw-.?
        (bad-mixed-decimal-fraction s state)
        (read-rational sgn (get-n) #f s (fx+ 1 start) end radix state))]
   [(#\+ #\-)
    (read-imag c sgn (get-n) (if (eqv? c #\+) +1 -1) s (fx+ 1 start) end radix state)]
   [(#\@)
    (read-polar sgn (get-n) s (fx+ 1 start) end radix state)]
   [(#\i)
    (finish-imaginary sgn (get-n) s (fx+ 1 start) end state)]
   [else
    (bad-digit c s state)]))

;; consumed digits and "e" (or similar)
(define (read-exponent sgn sgn-n exp s start end radix state)
  (parse-case
   s start end radix => c
   [(eof #\@) (fail state "empty exponent `~.a`" s)]
   [(digit)
    (read-signed-exponent sgn sgn-n exp 1 c s (fx+ 1 start) end radix state)]
   [(#\+ #\-)
    (define sgn2 (if (eqv? c #\+) +1 -1))
    (read-signed-exponent sgn sgn-n exp sgn2 #f s (fx+ 1 start) end radix state)]
   [(#\. #\# #\/ #\e #\E #\d #\D #\l #\L #\f #\F #\s #\S #\t #\T)
    (bad-misplaced c s state)]
   [(#\i)
    (if (state-has-first-half? state)
        (fail state "empty exponent `~.a`" s)
        (bad-misplaced "i" s state))]
   [else
    (bad-digit c s state)]))

;; consumed digits and "e" (or similar) and "+" or "-" (if any) and maybe digits
(define (read-signed-exponent sgn sgn-n exp sgn2 exp2 s start end radix state)
  (define (get-n) (if exp2
                      (lazy-number sgn-n radix (+ exp (* sgn2 exp2)))
                      (fail state "empty exponent `~.a`" s)))
  (parse-case
   s start end radix => c
   [(eof) (or (and exp2
                   (number? sgn-n)
                   (fast-inexact state (if (eqv? sgn-n 0) sgn 1) sgn-n radix exp sgn2 exp2))
              (maybe (get-n)
                     (lambda (n)
                       (finish sgn n s state))))]
   [(digit)
    (define new-exp2 (+ (if exp2 (* exp2 radix) 0) c))
    (read-signed-exponent sgn sgn-n exp sgn2 new-exp2 s (fx+ 1 start) end radix state)]
   [(#\+ #\-)
    (maybe (get-n)
           (lambda (n)
             (read-imag c sgn n (if (eqv? c #\+) +1 -1) s (fx+ 1 start) end radix state)))]
   [(#\. #\# #\/ #\e #\E #\d #\D #\l #\L #\f #\F #\s #\S #\t #\T)
    (bad-misplaced c s state)]
   [(#\@)
    (maybe (get-n)
           (lambda (n)
             (read-polar sgn n s (fx+ 1 start) end radix state)))]
   [(#\i)
    (maybe (get-n)
           (lambda (n)
             (finish-imaginary sgn n s (fx+ 1 start) end state)))]
   [else
    (bad-digit c s state)]))

;; consumed "+in" or "-in"
(define (read-infinity sgn c s start end radix state)
  (parse-case*
   s start end
   [[(#\f #\F)
     (#\.)
     (#\0 #\f #\t)]
    (define n (if (negative? sgn) -inf.0 +inf.0))
    (define new-state (set-exactness-by-char state (string-ref s (fx+ start 2))
                                             #:override? #t))
    (parse-case
     s (fx+ 3 start) end radix => c2
     [(eof) (finish sgn n s new-state)]
     [(#\+ #\-)
      (read-imag c2 sgn n (if (eqv? c2 #\+) +1 -1) s (fx+ 4 start) end radix new-state)]
     [(#\@)
      (read-polar sgn n s (fx+ 4 start) end radix new-state)]
     [(#\i)
      (finish-imaginary sgn n s (fx+ 4 start) end new-state)]
     [else
      (bad-digit c s state)])]
   [else
    (bad-digit c s state)]))

;; consumed "+n"
(define (read-nan c s start end radix state)
  (parse-case*
   s start end
   [[(#\a #\A)
     (#\n #\N)
     (#\.)
     (#\0 #\f #\t)]
    (define n +nan.0)
    (define new-state (set-exactness-by-char state (string-ref s (fx+ start 3))
                                             #:override? #t))
    (parse-case
     s (fx+ 4 start) end radix => c2
     [(eof) (finish +1 n s new-state)]
     [(#\+ #\-)
      (read-imag c2 1 n (if (eqv? c2 #\+) +1 -1) s (fx+ 5 start) end radix new-state)]
     [(#\@)
      (read-polar 1 n s (fx+ 5 start) end radix new-state)]
     [(#\i)
      (finish-imaginary +1 n s (fx+ 5 start) end new-state)]
     [else
      (bad-digit c s state)])]
   [else
    (bad-digit c s state)]))

;; consumed digits and "/"
(define (read-rational sgn sgn-n d s start end radix state)
  (define (get-n) (if d
                      (lazy-divide sgn-n d 'exact)
                      (bad-no-digits "/" s state)))
  (parse-case
   s start end radix => c
   [(eof)
    (maybe (get-n)
           (lambda (n)
             (finish sgn n s state)))]
   [(digit)
    (read-rational sgn sgn-n (+ (if d (* d radix) 0) c) s (fx+ 1 start) end radix state)]
   [(#\.)
    (bad-mixed-decimal-fraction s state)]
   [(#\#)
    (if d
        (read-denom-approx sgn sgn-n d 1 s (fx+ 1 start) end radix (set-exactness state 'approx))
        (bad-misplaced "#" s state))]
   [(#\e #\E #\d #\D #\l #\L #\f #\F #\s #\S #\t #\T)
    (maybe (get-n)
           (lambda (sgn-n)
             (read-exponent sgn sgn-n 0 s (fx+ 1 start) end radix (set-exactness-by-char state c))))]
   [(#\/)
    (bad-misplaced "/" s state)]
   [(#\+ #\-)
    (maybe (get-n)
           (lambda (n)
             (read-imag c sgn n (if (eqv? c #\+) +1 -1) s (fx+ 1 start) end radix state)))]
   [(#\@)
    (maybe (get-n)
           (lambda (n)
             (read-polar sgn n s (fx+ 1 start) end radix state)))]
   [(#\i)
    (maybe (get-n)
           (lambda (n)
             (finish-imaginary sgn n s (fx+ 1 start) end state)))]
   [else
    (bad-digit c s state)]))

;; consumed digits and "/" and digits and "#"
(define (read-denom-approx sgn sgn-n d exp s start end radix state)
  (define (get-n) (lazy-divide sgn-n (lazy-number d radix exp) 'approx))
  (parse-case
   s start end radix => c
   [(eof) (finish sgn (get-n) s state)]
   [(#\#)
    (read-denom-approx sgn sgn-n d (fx+ 1 exp) s (fx+ 1 start) end radix state)]
   [(digit)
    (bad-misplaced "#" s state)]
   [(#\. #\/)
    (bad-misplaced c s state)]
   [(#\e #\E #\d #\D #\l #\L #\f #\F #\s #\S #\t #\T)
    (read-exponent sgn (get-n) 0 s (fx+ 1 start) end radix (set-exactness-by-char state c))]
   [(#\+ #\-)
    (read-imag c sgn (get-n) (if (eqv? c #\+) +1 -1) s (fx+ 1 start) end radix state)]
   [(#\@)
    (read-polar sgn (get-n) s (fx+ 1 start) end radix state)]
   [(#\i)
    (finish-imaginary sgn (get-n) s (fx+ 1 start) end state)]
   [else
    (bad-digit c s state)]))

;; consumed "+" or "-" after the number in `real`
(define (read-imag c real-sgn real sgn s start end radix state)
  (cond
    [(or (state-has-first-half? state)
         (eq? 'extended (parse-state-exactness state)))
     ;; already parsing a complex number
     (bad-misplaced c s state)]
    [else
     ;; take it from almost the top, pushing the number so far into `state`;
     ;; we don't have to start at the very top, because we saw a "+" or "-"
     (read-signed sgn s start end radix (state-set-first-half state (rect-prefix real-sgn real (fx- start 1))))]))

;; consumed "@" after the number in `real`
(define (read-polar real-sgn real s start end radix state)
  (cond
    [(or (state-has-first-half? state)
         (eq? 'extended (parse-state-exactness state)))
     ;; already parsing a complex number
     (bad-misplaced "@" s state)]
    [else
     ;; take it from the top, pushing the number so far into `state`
     (parse-case
      s start end radix => c
      [(eof)
       (bad-misplaced "@" s state)]
      [(#\+ #\-)
       (define new-state (state-set-first-half state (polar-prefix real-sgn real start)))
       (read-signed (if (eq? c '#\+) 1 -1) s (fx+ 1 start) end radix new-state)]
      [(digit)
       (define new-state (state-set-first-half state (polar-prefix real-sgn real start)))
       (read-integer 1 c s (fx+ 1 start) end radix new-state)]
      [else
       (bad-digit c s state)])]))

;; ----------------------------------------

(module+ test
  (require (only-in racket/base
                    [string->number racket:string->number]))
  (define (try s)
    (define expect (racket:string->number s 10 'read 'decimal-as-inexact))
    (define got (string->number s 10 'read 'decimal-as-inexact))
    (unless (equal? expect got)
      (error 'fail "~e\n  expect: ~e\n  got: ~e" s expect got)))

  (try "#i+inf.0")
  (try "-inf.0")
  (try "#i+inf.f")
  (try "-inf.f")
  (try "#e+inf.0")
  (try "-inf.t")
  (try "10")
  (try "10.1")
  (try "1+2i")
  (try "#e10.1")
  (try "1#.#")
  (try "#e1#.#")
  (try "1/2")
  (try "#x+e#s+e")
  (try "#e#x+e#s+e")
  (try "-e#l-e")
  (try "#e#x+e#s+e@-e#l-e")
  (try "3.1415926535897932385t0")
  (try "+nan.0+1i")
  (try "3.0t0")
  (try "+i")
  (try "-i")
  (try "#i3")
  (try "#i3+i")
  (try "1/2+i")
  (try "1.2+i")
  (try "1/2+3")
  (try "1.2+3")
  (try "#i1.2t0+3i")
  (try "#i-0")
  (try "#i0")
  (try "-0#")
  (try "#i1-0i")
  (try "1#e500")
  (try "1#e10000000000000000000000000000000")
  (try "1#e-10000000000000000000000000000000")
  (try "-0#e10")
  (try "-0#e10000000000000000000000000000000")
  (try "1/2@0")
  (try "#i+8#i")
  (try "1#/3")
  (try "+inf.0@1")
  (try "+inf.0@1/1")
  (try "1/0#")
  (try "1#/0")
  (try "-1/0#")
  (try "#e1/2#e10")
  (try "1/0")
  (try "1@+inf.0")
  (try "1/1@+inf.0")
  ;(try "#d1/0+3.0i")
  (try "3.0t0+1/0i")
  (try "1/0+3.0t0i")
  (try "+inf.t0+1/0i")
  (try "1/0+inf.t0i")
  (try "3.#t0")
  (try "-1-2i")
  (try "-4.242154731064108e-5-6.865001427422244e-5i")
  (try "1e300+1e300i")
  (try "#x8f0767e50d4d0c07563bd81f530d36")
  (try "t")
  (try "s2")
  (try "2e")
  (try ".e1")
  (try "+.e1")
  (try "#e1")
  (try "1e#")
  (try "1e+")
  (try "1e+-")
  (try ".#e1")
  (try "1/")
  (try "/2")
  (try "1//2")
  (try "2..")
  (try "2+1"))
