#lang scheme/base

(require (only-in rnrs/base-6
                  div-and-mod div mod
                  div0-and-mod0 div0 mod0)
         rnrs/arithmetic/bitwise-6
         r6rs/private/num-inline
         (for-syntax r6rs/private/inline-rules))

(provide fixnum?
         fixnum-width
         least-fixnum
         greatest-fixnum
         fxbit-set?
         fxcopy-bit
         fxcopy-bit-field
         fxbit-field
         fxbit-count
         fxarithmetic-shift
         fxarithmetic-shift-left
         fxarithmetic-shift-right
         fxrotate-bit-field
         fxreverse-bit-field)
         ;; Many other provides from macros below

(define 64-bit? (fixnum? (expt 2 33)))

(define (fixnum-width) (if 64-bit? 63 31))
(define (least-fixnum) (if 64-bit? (- (expt 2 62)) -1073741824))
(define (greatest-fixnum) (if 64-bit? (- (expt 2 62) 1) +1073741823))

(define-syntax-rule (check v alt)
  (if (fixnum? v)
      v
      alt))

(define-inliner define-fx fixnum? "fixnum")

(define-fx = fx=? (a b c ...) nocheck)
(define-fx > fx>? (a b c ...) nocheck)
(define-fx < fx<? (a b c ...) nocheck)
(define-fx <= fx<=? (a b c ...) nocheck)
(define-fx >= fx>=? (a b c ...) nocheck)

(define-fx zero? fxzero? (a) nocheck)
(define-fx positive? fxpositive? (a) nocheck)
(define-fx negative? fxnegative? (a) nocheck)
(define-fx odd? fxodd? (a) nocheck)
(define-fx even? fxeven? (a) nocheck)

(define-fx max fxmax (a b ...) nocheck)
(define-fx min fxmin (a b ...) nocheck)

(define-fx + fx+ (a b) check)
(define-fx * fx* (a b) check)
(define-fx - fx- [(a) (a b)] check)

(provide fxdiv-and-mod
         fxdiv0-and-mod0)
(define (fxdiv-and-mod a b)
  (unless (fixnum? a)
    (raise-type-error 'fxdiv-and-mod "fixnum" a))
  (unless (fixnum? b)
    (raise-type-error 'fxdiv-and-mod "fixnum" b))
  (let-values ([(d m) (div-and-mod a b)])
    (check d (implementation-restriction 'div-and-mod d))
    (values d m)))
(define-fx div fxdiv (a b) check)
(define-fx mod fxmod (a b) nocheck)
(define (fxdiv0-and-mod0 a b)
  (unless (fixnum? a)
    (raise-type-error 'fxdiv0-and-mod0 "fixnum" a))
  (unless (fixnum? b)
    (raise-type-error 'fxdiv0-and-mod0 "fixnum" b))
  (let-values ([(d m) (div0-and-mod0 a b)])
    (check d (implementation-restriction 'div0-and-mod0 d))
    (values d m)))
(define-fx div0 fxdiv0 (a b) check)
(define-fx mod0 fxmod0 (a b) nocheck)

(define-syntax-rule (define-carry fx/carry (a b c) expr)
  (begin
    (provide fx/carry)
    (define (fx/carry a b c)
      (unless (fixnum? a)
        (raise-type-error 'fx/carry "fixnum" a))
      (unless (fixnum? a)
        (raise-type-error 'fx/carry "fixnum" b))
      (unless (fixnum? a)
        (raise-type-error 'fx/carry "fixnum" b))
      (let-values ([(d m) (div0-and-mod0 expr
                                         (arithmetic-shift 1 (fixnum-width)))])
        (values m d)))))

(define-carry fx+/carry (a b c) (+ a b c))
(define-carry fx-/carry (a b c) (- a b c))
(define-carry fx*/carry (a b c) (+ (* a b) c))

(define-fx bitwise-not fxnot (a) nocheck)
(define-fx bitwise-and fxand (a b ...) nocheck)
(define-fx bitwise-ior fxior (a b ...) nocheck)
(define-fx bitwise-xor fxxor (a b ...) nocheck)

(define-syntax-rule (fixnum-bitwise-if a b c)
  (bitwise-ior (bitwise-and a b)
               (bitwise-and (bitwise-not a) c)))
(define-fx fixnum-bitwise-if fxif (a b c) nocheck)

(define-fx bitwise-length fxlength (a) nocheck)
(define-fx bitwise-first-bit-set fxfirst-bit-set (a) nocheck)

(define positive-fixnum-width-bounds
  (string-append "exact integer in [0, " (number->string (- (fixnum-width) 1)) "]"))

(define fixnum-width-bounds
  (string-append "exact integer in [ " (number->string (- 1 (fixnum-width)))
                 ", " (number->string (- (fixnum-width) 1)) "]"))

(define (fxbit-set? n bit)
  (unless (fixnum? n)
    (raise-type-error 'fxbit-set? "fixnum" n))
  (bitwise-bit-set? n bit))

(define (fxbit-count n)
  (unless (fixnum? n)
    (raise-type-error 'fxbit-count "fixnum" n))
  (bitwise-bit-count n))

(define (fxcopy-bit n pos bit)
  (unless (fixnum? n)
    (raise-type-error 'fxcopy-bit "fixnum" n))
  (unless (and (exact-nonnegative-integer? pos)
               (< pos (fixnum-width)))
    (raise-type-error 'fxcopy-bit positive-fixnum-width-bounds pos))
  (bitwise-copy-bit n pos bit))

(define (fxcopy-bit-field n start end m)
  (unless (fixnum? n)
    (raise-type-error 'fxcopy-bit-field "fixnum" n))
  (unless (and (exact-nonnegative-integer? end)
               (< end (fixnum-width)))
    (raise-type-error 'fxcopy-bit-field positive-fixnum-width-bounds end))
  (unless (fixnum? m)
    (raise-type-error 'fxcopy-bit-field "fixnum" m))
  (bitwise-copy-bit-field n start end m))

(define (fxbit-field n start end)
  (unless (fixnum? n)
    (raise-type-error 'fxbit-field "fixnum" n))
  (unless (and (exact-nonnegative-integer? end)
               (< end (fixnum-width)))
    (raise-type-error 'fxbit-field positive-fixnum-width-bounds end))
  (bitwise-bit-field n start end))

(define-syntax-rule (define-shifter fxarithmetic-shift r6rs:fxarithmetic-shift
                      lower-bound bounds adjust)
  (begin
    (provide fxarithmetic-shift)
    (define-syntax fxarithmetic-shift
      (inline-rules
       r6rs:fxarithmetic-shift
       [(_ a b)
        (let ([t1 a]
              [t2 b])
          (if (and (fixnum? a)
                   (and (exact-integer? b) (<= lower-bound b (- (fixnum-width) 1))))
              (let ([v (arithmetic-shift a (adjust b))])
                (if (fixnum? v)
                    v
                    (r6rs:fxarithmetic-shift t1 t2)))
              (r6rs:fxarithmetic-shift t1 t2)))]))
    (define (r6rs:fxarithmetic-shift a b)
      (unless (fixnum? a)
        (raise-type-error 'fxarithmetic-shift "fixnum" a))
      (unless (and (exact-integer? b) (<= lower-bound b (- (fixnum-width) 1)))
        (raise-type-error 'fxarithmetic-shift bounds b))
      (let ([v (arithmetic-shift a (adjust b))])
        (if (fixnum? v)
            v
            (implementation-restriction 'fxarithmetic-shift v))))))

(define-shifter fxarithmetic-shift r6rs:fxarithmetic-shift
  (- 1 (fixnum-width)) fixnum-width-bounds values)
(define-shifter fxarithmetic-shift-left r6rs:fxarithmetic-shift-left
  0 positive-fixnum-width-bounds values)
(define-shifter fxarithmetic-shift-right r6rs:fxarithmetic-shift-right
  0 positive-fixnum-width-bounds -)


(define (fxrotate-bit-field n start end count)
  (unless (fixnum? n)
    (raise-type-error 'fxrotate-bit-field "fixnum" n))
  (unless (and (exact-nonnegative-integer? end)
               (< end (fixnum-width)))
    (raise-type-error 'fxrotate-bit-field positive-fixnum-width-bounds end))
  (unless (and (exact-nonnegative-integer? count)
               (< count (fixnum-width)))
    (raise-type-error 'fxrotate-bit-field positive-fixnum-width-bounds count))
  (bitwise-rotate-bit-field n start end count))

(define (fxreverse-bit-field n start end)
  (unless (fixnum? n)
    (raise-type-error 'fxrotate-bit-field "fixnum" n))
  (unless (and (exact-nonnegative-integer? end)
               (< end (fixnum-width)))
    (raise-type-error 'fxrotate-bit-field positive-fixnum-width-bounds end))
  (bitwise-reverse-bit-field n start end))
