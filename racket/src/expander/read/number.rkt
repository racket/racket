#lang racket/base
(require racket/private/check
         racket/extflonum
         ;; Call the host `string->number` function only
         ;; on valid fixnum, bignum, {single-,double-,ext}flonum
         ;; representations that contain digits, possibly a
         ;; leading sign, possibly a `.`, and possibly an
         ;; exponent marker
         (prefix-in host: "../host/string-to-number.rkt")
         "parameter.rkt")

(provide string->number)

;; The `string->number` parser is responsible for handling Racket's
;; elaborate number syntax (mostly inherited from Scheme). It relies
;; on a host-system `string->number` that can handle well-formed
;; fixnum, bignum, and {double-,single-,extfl}flonum strings for a
;; given radix in the range [2,16]. Otherwise, the parser here
;; performs all checking that reader needs.

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

  (do-string->number s 0 (string-length s)
                     radix #:radix-set? #f
                     decimal-mode
                     convert-mode))

;; When parsing fails, either return an error string or #f. An error
;; string is reported only in 'read mode and when if we're somehow
;; onligated to parse as a number, such as after `#i`.
(define-syntax-rule (fail mode msg arg ...)
  (cond
    [(eq? mode 'must-read)
     (format msg arg ...)]
    [else #f]))

;; The `convert-mode` argument here can be 'number-or-false, 'read, or
;; 'must-read, where 'must-read reports an error on parsing failure
;; instead of returning #f. At this level, we mostly detect the
;; special numbers `+inf.0` in combinations, and otherwise dispatch
;; to parsing a complex number, fraction, or exponential.
(define (do-string->number s start end
                           radix #:radix-set? radix-set?
                           exactness ; 'inexact, 'exact, 'decimal-as-inexact, or 'decimal-as-exact
                           #:in-complex [in-complex #f] ; #f, 'i, or '@
                           convert-mode)
  (cond
    [(= start end)
     (fail convert-mode "no digits")]
    [else
     (define c (string-ref s start))
     (cond
       ;; `#e`, `#x`, etc.
       [(char=? #\# c)
        (define next (add1 start))
        (cond
          [(= next end)
           (fail convert-mode "no character after `#` indicator in `~.a`" s)]
          [else
           (define i (string-ref s next))
           (case i
             [(#\e #\E #\i #\I)
              (cond
                [(or (exactness-set? exactness) in-complex)
                 (fail convert-mode "misplaced exactness specification at `~.a`" (substring s start end))]
                [else
                 (do-string->number s (add1 next) end
                                    radix #:radix-set? radix-set?
                                    (if (or (char=? i #\e) (char=? i #\E)) 'exact 'inexact)
                                    (if (eq? convert-mode 'read) 'must-read convert-mode))])]
             [(#\b #\B #\o #\O #\d #\D #\x #\X)
              (cond
                [(or radix-set? in-complex)
                 (fail convert-mode "misplaced radix specification at `~.a`" (substring s start end))]
                [else
                 (define radix
                   (case i
                     [(#\b #\B) 2]
                     [(#\o #\O) 8]
                     [(#\d #\D) 10]
                     [else 16]))
                 (do-string->number s (add1 next) end
                                    radix #:radix-set? #t
                                    exactness
                                    (if (eq? convert-mode 'read) 'must-read convert-mode))])]
             [else
              ;; The reader always complains about a bad leading `#`
              (fail (read-complains convert-mode) "bad `#` indicator `~a` at `~.a`" i (substring s start end))])])]
       ;; +inf.0, etc.
       [(and (char-sign? c)
             (read-special-number s start end convert-mode))
        =>
        (lambda (v)
          (cond
            [(eq? exactness 'exact)
             (fail convert-mode "no exact representation for `~a`" v)]
            [else v]))]
       ;; +inf.0+...i, etc.
       [(and (char-sign? c)
             (not in-complex)
             ((- end start) . > . 7)
             (char=? #\i (string-ref s (sub1 end)))
             (char-sign? (string-ref s 6))
             (read-special-number s start (+ start 6) convert-mode))
        =>
        (lambda (v)
          (read-for-special-compound s (+ start 6) (sub1 end)
                                     radix
                                     exactness
                                     convert-mode
                                     #:in-complex 'i
                                     v (lambda (v v2)
                                         (make-rectangular v v2))))]
       ;; ...+inf.0i, etc.
       [(and (not in-complex)
             ((- end start) . >= . 7) ; allow `+inf.0i`
             (char=? #\i (string-ref s (sub1 end)))
             (char-sign? (string-ref s (- end 7)))
             (read-special-number s (- end 7) (sub1 end) convert-mode))
        =>
        (lambda (v2)
          (cond
            [(and (= start (- end 7))
                  (not (extflonum? v2)))
             (make-rectangular 0 v2)]
            [else
             (read-for-special-compound s start (- end 7)
                                        radix
                                        exactness
                                        convert-mode
                                        #:in-complex 'i
                                        #:reading-first? #t
                                        v2 (lambda (v2 v)
                                             (make-rectangular v v2)))]))]
       ;; +inf.0@..., etc.
       [(and (char-sign? c)
             (not in-complex)
             ((- end start) . > . 7)
             (char=? #\@ (string-ref s (+ start 6)))
             (read-special-number s start (+ start 6) convert-mode))
        =>
        (lambda (v)
          (read-for-special-compound s (+ start 7) end
                                     radix
                                     exactness
                                     convert-mode
                                     #:in-complex '@
                                     v (lambda (v v2)
                                         (make-polar v v2))))]
       ;; ...@+inf.0, etc.
       [(and (not in-complex)
             ((- end start) . > . 7)
             (char=? #\@ (string-ref s (- end 7)))
             (read-special-number s (- end 6) end convert-mode))
        =>
        (lambda (v2)
          (read-for-special-compound s start (- end 7)
                                     radix
                                     exactness
                                     convert-mode
                                     #:in-complex '@
                                     #:reading-first? #t
                                     v2 (lambda (v2 v)
                                          (make-polar v v2))))]
       [else
        (do-string->non-special-number s start end
                                       radix #:radix-set? radix-set?
                                       exactness
                                       #:in-complex in-complex
                                       convert-mode)])]))

(define (do-string->non-special-number s start end
                                       radix #:radix-set? radix-set?
                                       exactness
                                       #:in-complex [in-complex #f]
                                       convert-mode)
  ;; Look for `@`, `i`, `+`/`-`, and exponent markers like `e`.
  ;; Some of those can be used together, but we detect impossible
  ;; combinations here and complain. For example `+` that's not
  ;; after an exponential marker cannot appear twice, unless the
  ;; the two are separated by `@` or the second eventually supports
  ;; an ending `i`. Sometimes we can complain right away, and other
  ;; times we collect positions to complain at the end, which as
  ;; when an extra sign appears after a `.` or `/`.
  (let loop ([i start] [any-digits? #f] [any-hashes? #f] [i-pos #f] [@-pos #f]
             [sign-pos #f] [dot-pos #f] [slash-pos #f] [exp-pos #f]
             [must-i? #f])
    (cond
      [(= i end)
       ;; We've finished looking, so dispatch on the kind of number parsing
       ;;  based on found `@`, etc.
       ;; If we saw `@`, then we discarded other positions at that point.
       ;; If we saw `i` at the end, then we discarded other positions except `sign-pos`.
       ;; If we saw `.`, then we discarded earlier `slash-pos` and `exp-pos` or complained.
       ;; If we saw `/`, then we discarded earlier `dot-pos` and `exp-pos` or complained.
       ;; If we saw `+` or `-`, then we discarded earlier `exp-pos`.
       (cond
         [(and (not any-digits?)
               ;; A number like `+i` can work with no digits
               (not i-pos))
          (fail convert-mode "no digits in `~.a`" (substring s start end))]
         [(and must-i? (not i-pos))
          (fail convert-mode "too many signs in `~.a`" (substring s start end))]
         [(and sign-pos
               (or (and dot-pos (dot-pos . < . sign-pos))
                   (and slash-pos (slash-pos . < . sign-pos))))
          (fail convert-mode "misplaced sign in `~.a`" (substring s start end))]
         [i-pos
          (string->complex-number s start sign-pos sign-pos (sub1 end)
                                  i-pos sign-pos
                                  radix #:radix-set? radix-set?
                                  exactness
                                  #:in-complex 'i
                                  convert-mode)]
         [@-pos
          (string->complex-number s start @-pos (add1 @-pos) end
                                  i-pos sign-pos
                                  radix #:radix-set? radix-set?
                                  exactness
                                  #:in-complex '@
                                  convert-mode)]
         [else
          (string->real-number s start end
                               dot-pos slash-pos exp-pos
                               any-hashes?
                               radix
                               exactness
                               convert-mode)])]
      [else
       (define c (string-ref s i))
       (cond
         [(digit? c radix)
          (loop (add1 i) #t any-hashes? i-pos @-pos
                sign-pos dot-pos slash-pos exp-pos
                must-i?)]
         [(char=? c #\#) ; treat like a digit
          (loop (add1 i) #t #t i-pos @-pos
                sign-pos dot-pos slash-pos exp-pos
                must-i?)]
         [(char-sign? c)
          (cond
            [(and sign-pos must-i?)
             (fail convert-mode "too many signs in `~.a`" (substring s start end))]
            [else
             (loop (add1 i) any-digits? any-hashes? i-pos @-pos
                   i dot-pos slash-pos #f
                   ;; must be complex if sign isn't at start
                   (and (> i start) (or (not @-pos) (> i (add1 @-pos)))))])]
         [(char=? c #\.)
          (cond
            [(or (and exp-pos (or (not sign-pos) (exp-pos . > . sign-pos)))
                 (and dot-pos (or (not sign-pos) (dot-pos . > . sign-pos))))
             (fail convert-mode "misplaced `.` in `~.a`" (substring s start end))]
            [(and slash-pos (or (not sign-pos) (slash-pos . > . sign-pos)))
             (fail convert-mode "decimal points and fractions annot be mixed `~.a`" (substring s start end))]
            [else
             (loop (add1 i) any-digits? any-hashes? i-pos @-pos
                   sign-pos i #f #f
                   must-i?)])]
         [(char=? c #\/)
          (cond
            [(and dot-pos (or (not sign-pos) (dot-pos . > . sign-pos)))
             (fail convert-mode "decimal points and fractions annot be mixed `~.a`" (substring s start end))]
            [(or (and exp-pos (or (not sign-pos) (exp-pos . > . sign-pos)))
                 (and slash-pos (or (not sign-pos) (slash-pos . > . sign-pos))))
             (fail convert-mode "misplaced `/` in `~.a`" (substring s start end))]
            [else
             (loop (add1 i) any-digits? any-hashes? i-pos @-pos
                   sign-pos #f i #f
                   must-i?)])]
         [(or (char=? c #\e) (char=? c #\E)
              (char=? c #\f) (char=? c #\F)
              (char=? c #\d) (char=? c #\D)
              (char=? c #\s) (char=? c #\S)
              (char=? c #\l) (char=? c #\L)
              (char=? c #\t) (char=? c #\T))
          (cond
            [exp-pos
             (fail convert-mode "misplaced `~a` in `~.a`" c (substring s start end))]
            ;; Dont count a sign in something like 1e+2 as `sign-pos`
            [(and ((add1 i) . < . end)
                  (char-sign? (string-ref s (add1 i))))
             (loop (+ i 2) any-digits? any-hashes? i-pos @-pos
                   sign-pos dot-pos slash-pos (or exp-pos i)
                   must-i?)]
            [else
             (loop (+ i 1) any-digits? any-hashes? i-pos @-pos
                   sign-pos dot-pos slash-pos (or exp-pos i)
                   must-i?)])]
         [(char=? c #\@)
          (cond
            [(eq? in-complex 'i)
             (fail convert-mode "cannot mix `@` and `i` in `~.a`" (substring s start end))]
            [(or @-pos (eq? in-complex '@))
             (fail convert-mode "too many `@`s in `~.a`" (substring s start end))]
            [(= i start)
             (fail convert-mode "`@` cannot be at start in `~.a`" (substring s start end))]
            [must-i?
             (fail convert-mode "too many signs in `~.a`" (substring s start end))]
            [else
             (loop (add1 i) any-digits? any-hashes? i-pos i
                   #f #f #f #f
                   must-i?)])]
         [(and (or (char=? c #\i) (char=? c #\I))
               sign-pos)
          (cond
            [(or @-pos (eq? in-complex '@))
             (fail convert-mode "cannot mix `@` and `i` in `~.a`" (substring s start end))]
            [(or ((add1 i) . < . end) (eq? in-complex 'i))
             (fail convert-mode "`i' must be at the end in `~.a`" (substring s start end))]
            [else
             (loop (add1 i) any-digits? any-hashes? i @-pos
                   sign-pos #f #f #f
                   #f)])]
         [else
          (cond
            [(char=? c #\nul)
             (fail convert-mode "nul character in `~.a`" s)]
            [else
             (fail convert-mode "bad digit `~a`" c)])])])))

;; Parse and combine the halves of an impginary number, either
;; in `<real>[+-]<imag>i` form or `<mag>@<angle>` form as
;; indicated by `in-complex`
(define (string->complex-number s start1 end1 start2 end2
                                i-pos sign-pos
                                radix #:radix-set? radix-set?
                                exactness
                                #:in-complex in-complex ; 'i or '@
                                convert-mode)
  (define v1 (cond
               [(= start1 end1)
                ;; The input was "[+-]<num>i", so the real part
                ;; is implicitly "0"
                (if (eq? exactness 'inexact)
                    0.0
                    0)]
               [else
                (do-string->number s start1 end1
                                   radix #:radix-set? radix-set?
                                   exactness
                                   #:in-complex in-complex
                                   convert-mode)]))
  (define v2 (cond
               [(and (eq? in-complex 'i)
                     (= (- end2 start2) 1))
                ;; The input ends "[+-]i", so the number is implicitly
                ;; "1"
                (define neg? (char=? (string-ref s start2) #\-))
                (cond
                  [(eq? exactness 'inexact)
                   (if neg? -1.0 1.0)]
                  [else
                   (if neg? -1 1)])]
               [else
                (do-string->number s start2 end2
                                   radix #:radix-set? radix-set?
                                   exactness
                                   #:in-complex in-complex
                                   convert-mode)]))
  (cond
    [(or (not v1) (not v2))
     #f]
    [(and (or (extflonum? v1) (extflonum? v2))
          (not (eq? convert-mode 'must-read)))
     ;; If no 'must-read, then an extflonum-combination
     ;; failure hides even a divide-by-zero error
     (fail-extflonum convert-mode v1)]
    [(string? v1) v1]
    [(extflonum? v1)
     (fail-extflonum convert-mode v1)]
    [(string? v2) v2]
    [(extflonum? v2)
     (fail-extflonum convert-mode v2)]
    [(eq? in-complex 'i)
     (make-rectangular v1 v2)]
    [else
     (define p (make-polar v1 v2))
     (if (eq? exactness 'exact)
         (inexact->exact p)
         p)]))

;; Parse a real number that might be a faction, have `.`, or have `#`s
(define (string->real-number s start end
                             dot-pos slash-pos exp-pos
                             any-hashes? ; can be false-positive
                             radix
                             exactness
                             convert-mode)
  ;; Try shortcut of using primitive `string->number`, which should
  ;; work on real numbers and extflonums
  (define (extfl-mark?) (char=? (char-downcase (string-ref s exp-pos)) #\t))
  (define simple?
    (and (not slash-pos)
         (or (eq? exactness 'inexact)
             (eq? exactness 'decimal-as-inexact)
             (and (not dot-pos) (not exp-pos)))
         (or (not exp-pos)
             (not (eq? convert-mode 'number-or-false))
             (not (extfl-mark?)))
         (not (and any-hashes? (hashes? s start end)))))
  (define has-sign? (and (end . > . start) (char-sign? (string-ref s start))))
  (cond
    [(= (- end start) (+ (if dot-pos 1 0) (if exp-pos 1 0) (if has-sign? 1 0)))
     (if (= end start)
         (fail convert-mode "missing digits")
         (fail convert-mode "missing digits in `~.a`" (substring s start end)))]
    [simple?
     (cond
       [(and exp-pos (= (- exp-pos start)
                        (+ (if (and dot-pos (< dot-pos exp-pos)) 1 0)
                           (if has-sign? 1 0))))
        (fail convert-mode "missing digits before exponent marker in `~.a`" (substring s start end))]
       [(and exp-pos
             (or (= exp-pos (sub1 end))
                 (and (= exp-pos (- end 2))
                      (char-sign? (string-ref s (sub1 end))))))
        (fail convert-mode "missing digits after exponent marker in `~.a`" (substring s start end))]
       [else
        (define n (host:string->number (maybe-substring s start end) radix
                                       ;; Use 'read mode as needed to enable extflonum results
                                       (if (or (eq? convert-mode 'number-or-false)
                                               (not exp-pos)
                                               (not (extfl-mark?)))
                                           'number-or-false
                                           'read)))
        (cond
          [(or (not n) (string? n))
           (error 'string->number "host `string->number` failed on ~s" (substring s start end))]
          [(eq? exactness 'inexact)
           (cond
             [(extflonum? n)
              (fail convert-mode "cannot convert extflonum `~.a` to inexact" (substring s start end))]
             [(and (eqv? n 0)
                   (char=? (string-ref s start) #\-))
              -0.0]
             [else
              (exact->inexact n)])]
          [else n])])]
    [exp-pos
     (define m-v (string->real-number s start exp-pos
                                      dot-pos slash-pos #f
                                      any-hashes?
                                      radix
                                      'exact
                                      convert-mode))
     (define e-v (string->exact-integer-number s (+ exp-pos 1) end
                                               radix
                                               convert-mode))
     (define (real->precision-inexact r)
       (case (string-ref s exp-pos)
         [(#\s #\S #\f #\F) (real->single-flonum r)]
         [(#\t #\T)
          (if (extflonum-available?)
              (real->extfl r)
              ;; The host `string->number` can make a string-based
              ;; representation to preserve the content, if not compute
              ;; with it
              (host:string->number (replace-hashes s start end) radix 'read))]
         [else (real->double-flonum r)]))
     (define get-extfl? (extfl-mark?))
     (cond
       [(or (not m-v) (not e-v)) #f]
       [(string? m-v) m-v]
       [(string? e-v) e-v]
       [(and (eq? convert-mode 'number-or-false) get-extfl?)
        #f]
       [(and (or (eq? exactness 'inexact) (eq? exactness 'decimal-as-inexact))
             ((abs e-v) . > . (if get-extfl? 6000 400)))
        ;; Don't calculate a huge exponential to return a float:
        (real->precision-inexact
         (cond
           [(eqv? m-v 0) (if (char=? (string-ref s start) #\-)
                             -0.0
                             0.0)]
           [(positive? m-v) (if (positive? e-v)
                                +inf.0
                                +0.0)]
           [else (if (positive? e-v)
                     -inf.0
                     -0.0)]))]
       [(and (exactness-set? exactness) get-extfl?)
        (fail convert-mode "cannot convert extflonum `~.a` to ~a" (substring s start end) exactness)]
       [else
        ;; This calculation would lose precision for floating-point
        ;; numbers, but we don't get here for inexact `m-v`:
        (define n (* m-v (expt radix e-v)))
        (cond
          [(and (not get-extfl?)
                (or (eq? exactness 'exact) (eq? exactness 'decimal-as-exact)))
           n]
          [(and (eqv? n 0)
                (char=? (string-ref s start) #\-))
           (real->precision-inexact -0.0)]
          [else
           (real->precision-inexact n)])])]
    [slash-pos
     ;; the numerator or demoniator doesn't have a decimal
     ;; place or exponent marker, but it may have `#`s
     (define n-v (string->real-number s start slash-pos
                                      #f #f #f
                                      any-hashes?
                                      radix
                                      'exact
                                      convert-mode))
     (define d-v (string->real-number s (add1 slash-pos) end
                                      #f #f #f
                                      any-hashes?
                                      radix
                                      'exact
                                      convert-mode))
     (define (get-inexact? from-pos)
       (or (eq? exactness 'inexact)
           ;; For historical reasons, `#`s in a fraction trigger an
           ;; inexact result, even if `exactness` is 'decimal-as-exact
           (and (not (eq? exactness 'exact))
                (hashes? s from-pos end))))
     (cond
       [(or (not n-v) (not d-v)) #f]
       [(string? n-v) n-v]
       [(string? d-v) d-v]
       [(eqv? d-v 0)
        (cond
          [(get-inexact? (add1 slash-pos))
           (if (negative? n-v)
               -inf.0
               +inf.0)]
          [else
           ;; The reader always complains about divide-by-zero
           (fail (read-complains convert-mode) "division by zero in `~.a`" (substring s start end))])]
       [else
        (define n (/ n-v d-v))
        (if (get-inexact? start)
            (exact->inexact n)
            n)])]
    ;; We get this far only if the input has `#` or if the input has a
    ;; `.` and we want exact
    [else
     (string->decimal-number s start end
                             dot-pos
                             radix
                             exactness
                             convert-mode)]))

;; Parse a number that might have `.` and/or `#` in additon to digits
;; and possibiliy a leading `+` or `-`
(define (string->decimal-number s start end
                                dot-pos
                                radix
                                exactness
                                convert-mode)
  (define get-exact? (or (eq? exactness 'exact) (eq? exactness 'decimal-as-exact)))
  (define new-str (make-string (- end start (if (and dot-pos get-exact?) 1 0))))
  (let loop ([i (sub1 end)] [j (sub1 (string-length new-str))] [hashes-pos end])
    (cond
      [(i . < . start)
       ;; Convert `new-str` to an integer and finish up
       (cond
         [(= hashes-pos start)
          (fail convert-mode "misplaced `#` in `~.a`" (substring s start end))]
         [else
          (define n (host:string->number new-str radix))
          (cond
            [(not n)
             (fail-bad-number convert-mode s start end)]
            [(not get-exact?)
             (if (and (eqv? n 0)
                      (char=? (string-ref s start) #\-))
                 -0.0
                 (exact->inexact n))]
            [(and dot-pos get-exact?)
             (/ n (expt 10 (- end dot-pos 1)))]
            [else n])])]
      [else
       (define c (string-ref s i))
       (cond
         [(char=? c #\.)
          (cond
            [get-exact?
             (loop (sub1 i) j (if (= hashes-pos (add1 i)) i hashes-pos))]
            [else
             (string-set! new-str j c)
             (loop (sub1 i) (sub1 j) (if (= hashes-pos (add1 i)) i hashes-pos))])]
         [(or (char=? c #\-) (char=? c #\+))
          (string-set! new-str j c)
          (loop (sub1 i) (sub1 j) (if (= hashes-pos (add1 i)) i hashes-pos))]
         [(char=? c #\#)
          (cond
            [(= hashes-pos (add1 i))
             (string-set! new-str j #\0)
             (loop (sub1 i) (sub1 j) i)]
            [else
             (fail convert-mode "misplaced `#` in `~.a`" (substring s start end))])]
         [else
          (string-set! new-str j c)
          (loop (sub1 i) (sub1 j) hashes-pos)])])))

;; Parse an integer that might have `#` and a leading `+` or `-`, but
;; no other non-digit characters
(define (string->exact-integer-number s start end
                                      radix
                                      convert-mode)
  (cond
    [(hashes? s start end)
     (fail convert-mode "misplaced `#` in `~.a`" (substring s start end))]
    [else
     (define n (host:string->number (maybe-substring s start end) radix))
     (cond
       [(not n)
        (fail convert-mode "bad exponent `~.a`" (substring s start end))]
       [else n])]))

;; Try to read as `+inf.0`, etc.
(define (read-special-number s start end convert-mode)
  (and
   (= (- end start) 6)
   (or (char=? (string-ref s start) #\+)
       (char=? (string-ref s start) #\-))
   (or
    (and (char=? (char-downcase (string-ref s (+ start 1))) #\i)
         (char=? (char-downcase (string-ref s (+ start 2))) #\n)
         (char=? (char-downcase (string-ref s (+ start 3))) #\f)
         (char=? (char-downcase (string-ref s (+ start 4))) #\.)
         (or
          (and
           (char=? (char-downcase (string-ref s (+ start 5))) #\0)
           (if (char=? (string-ref s start) #\+)
               +inf.0
               -inf.0))
          (and
           (char=? (char-downcase (string-ref s (+ start 5))) #\f)
           (if (char=? (string-ref s start) #\+)
               +inf.f
               -inf.f))
          (and
           (char=? (char-downcase (string-ref s (+ start 5))) #\t)
           (not (eq? convert-mode 'number-or-false))
           (if (char=? (string-ref s start) #\+)
               +inf.t
               -inf.t))))
    (and (char=? (char-downcase (string-ref s (+ start 1))) #\n)
         (char=? (char-downcase (string-ref s (+ start 2))) #\a)
         (char=? (char-downcase (string-ref s (+ start 3))) #\n)
         (char=? (char-downcase (string-ref s (+ start 4))) #\.)
         (or (and (char=? (char-downcase (string-ref s (+ start 5))) #\0)
                  +nan.0)
             (and (char=? (char-downcase (string-ref s (+ start 5))) #\f)
                  +nan.f)
             (and (char=? (char-downcase (string-ref s (+ start 5))) #\t)
                  (not (eq? convert-mode 'number-or-false))
                  +nan.t))))))

(define (fail-extflonum convert-mode v)
  (fail convert-mode "cannot combine extflonum `~a` into complex number" v))

;; Read the other half of something like `+inf.0+...i` or `...@-inf.0`
(define (read-for-special-compound s start end
                                   radix
                                   exactness
                                   convert-mode
                                   #:in-complex in-complex
                                   #:reading-first? [reading-first? #f]
                                   v combine)
  (cond
    [(eq? exactness 'exact)
     (fail convert-mode "no exact representation for `~a`" v)]
    [(and (extflonum? v) (or (not reading-first?)
                             ;; If no 'must-read, then an extflonum-combination
                             ;; failure hides even a divide-by-zero error
                             (not (eq? convert-mode 'must-read))))
     (fail-extflonum convert-mode v)]
    [else
     (define v2
       (do-string->number s start end
                          radix #:radix-set? #t
                          exactness
                          #:in-complex in-complex
                          convert-mode))
     (cond
       [(string? v2) v2]
       [(not v2) v2]
       [(extflonum? v)
        (fail-extflonum convert-mode v)]
       [else (combine v v2)])]))

(define (hashes? s start end)
  (for/or ([c (in-string s start end)])
    (char=? c #\#)))

(define (replace-hashes s start end)
  (define new-s (make-string (- end start)))
  (for ([c (in-string s start end)]
        [i (in-naturals)])
    (if (char=? c #\#)
        (string-set! new-s i #\0)
        (string-set! new-s i c)))
  new-s)

(define (maybe-substring s start end)
  (if (and (= 0 start)
           (= end (string-length s)))
      s
      (substring s start end)))

(define (exactness-set? exactness)
  (or (eq? exactness 'exact) (eq? exactness 'inexact)))

(define (char-sign? c)
  (or (char=? c #\-) (char=? c #\+)))

(define (digit? c radix)
  (define v (char->integer c)) 
  (or (and (v . >= . (char->integer #\0))
           ((- v (char->integer #\0)) . < . radix))
      (and (radix . > . 10)
           (or (and
                (v . >= . (char->integer #\a))
                ((- v (- (char->integer #\a) 10)) . < . radix))
               (and
                (v . >= . (char->integer #\A))
                ((- v (- (char->integer #\A) 10)) . < . radix))))))

(define (fail-bad-number convert-mode s start end)
  (fail convert-mode "bad number `~.a`" (substring s start end)))

(define (read-complains convert-mode)
  (if (eq? convert-mode 'read) 'must-read convert-mode))

;; ----------------------------------------

(module+ test
  (define (try s)
    (define expect (host:string->number s 10 'read 'decimal-as-inexact))
    (define got (string->number s 10 'read 'decimal-as-inexact))
    (unless (equal? expect got)
      (error 'fail "~e\n  expect: ~e\n  got: ~e" s expect got)))

  (try "#i+inf.0")
  (try "-inf.0")
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
  (try "#e-e#l-e")
  (try "#e#x+e#s+e@-e#l-e")
  (try "#e+@1")
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
  (try "#d1/0+3.0i")
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
  (try "#ds2")
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
  (try "#/2")
  (try "1//2")
  (try "2..")
  (try "2+1"))
