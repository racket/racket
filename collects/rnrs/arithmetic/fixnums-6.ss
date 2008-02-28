#lang scheme/base

(require (only-in rnrs/base-6
                  div-and-mod div mod
                  div0-and-mod0 div0 mod0)
         (only-in rnrs/arithmetic/bitwise-6
                  bitwise-if
                  bitwise-first-bit-set
                  bitwise-copy-bit)
         (for-syntax r6rs/private/inline-rules))

(provide fixnum?
         fixnum-width
         least-fixnum
         greatest-fixnum)
;; Many other provides from macros below

(define (fixnum-width) 30)
(define (least-fixnum) -1073741824)
(define (greatest-fixnum) +1073741824)

(define (r6rs:fixnum? v)
  (and (exact-integer? v)
       (<= -1073741824 v 1073741823)))

(define-syntax fixnum?
  (inline-rules
   r6rs:fixnum?
   [(_ a) (let ([v a])
            (and (exact-integer? v)
                 (<= -1073741824 v 1073741823)))]))

(define (implementation-restriction who what)
  (error 'who "implementation restriction: ~e" what))

(define-syntax define-an-fx
  (syntax-rules ()
    [(_ orig fx check-result ([(arg ...) (tmp ...)] ...) . rest)
     (begin
       (provide fx)
       (define fx-proc
         (let ([fx (case-lambda 
                    [(arg ...)
                     (unless (fixnum? arg)
                       (raise-type-error 'fx "fixnum" arg))
                     ...
                     (let ([r (orig arg ...)])
                       (unless (fixnum? r)
                         (implementation-restriction 'fx r))
                       r)]
                    ...
                    . rest)])
           fx))
       (define-syntax fx
         (inline-rules
          fx-proc
          [(_ arg ...)
           (let ([tmp arg] ...)
             (if (and (fixnum? tmp) ...)
                 (let ([v (orig tmp ...)])
                   (check-result v (fx-proc tmp ...)))
                 (fx-proc tmp ...)))]
          ...)))]))

(define-syntax-rule (check v (fx-proc tmp ...))
  (if (fixnum? v)
      v
      (fx-proc tmp ...)))

(define-syntax-rule (nocheck v . _)
  v)

(define-syntax define-an-fx+rest
  (syntax-rules ()
    [(_ orig fx check clauses)
     (define-an-fx orig fx check clauses
       [args (for-each (lambda (arg)
                         (unless (fixnum? args)
                           (raise-type-error 'fx "fixnum" arg)))
                       args)
             (let ([r (apply orig args)])
               (unless (fixnum? r)
                 (implementation-restriction 'fx r))
               r)])]))


(define-syntax define-fx
  (syntax-rules (...)
    [(_ orig fx [(a) (b c)] check)
     (define-an-fx orig fx check
       ([(a) (t1)]
        [(b c) (t1 t2)]))]
    [(_ orig fx (a b c (... ...)) check)
     (define-an-fx+rest orig fx check
       ([(a b) (t1 t2)]))]
    [(_ orig fx (a b (... ...)) check)
     (define-an-fx+rest orig fx check
       ([(a) (t1)]
        [(a b) (t1 t2)]))]
    [(_ orig fx (a) check)
     (define-an-fx+rest orig fx check
       ([(a) (t1)]))]
    [(_ orig fx (a b) check)
     (define-an-fx orig fx check
       ([(a b) (t1 t2)]))]
    [(_ orig fx (a b c) check)
     (define-an-fx orig fx check
       ([(a b c) (t1 t2 t3)]))]))

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
(define-fx max fxmin (a b ...) nocheck)

(define-fx + fx+ (a b) check)
(define-fx * fx* (a b) check)
(define-fx - fx- [(a) (a b)] check)

(define-fx div-and-mod fxdiv-and-mod (a b) nocheck)
(define-fx div fxdiv (a b) nocheck)
(define-fx mod fxmod (a b) nocheck)
(define-fx div0-and-mod0 fxdiv0-and-mod0 (a b) nocheck)
(define-fx div0 fxdiv0 (a b) nocheck)
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
      (let-values ([(d m) (div0-and-mod0 (+ a b c) 
                                         (arithmetic-shift 1 (fixnum-width)))])
        (values m d)))))

(define-carry fx+/carry (a b c) (+ a b c))
(define-carry fx-/carry (a b c) (- a b c))
(define-carry fx*/carry (a b c) (* (+ a b) c))

(define-fx bitwise-not fxnot (a) nocheck)
(define-fx bitwise-and fxand (a b ...) nocheck)
(define-fx bitwise-ior fxior (a b ...) nocheck)
(define-fx bitwise-xor fxxor (a b ...) nocheck)
(define-fx bitwise-first-bit-set fxfirst-bit-set (a) nocheck)
(define-fx bitwise-copy-bit fxcopy-bit (a) nocheck)

(define-syntax-rule (fixnum-bitwise-if a b c)
  (bitwise-ior (bitwise-and a b)
               (bitwise-and (bitwise-not a) c)))
(define-fx fixnum-bitwise-if fxif (a b c) nocheck)



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
                   (and (exact-integer? b) (<= lower-bound b 30)))
              (let ([v (arithmetic-shift a (adjust b))])
                (if (fixnum? v)
                    v
                    (r6rs:fxarithmetic-shift t1 t2)))
              (r6rs:fxarithmetic-shift t1 t2)))]))
    (define (r6rs:fxarithmetic-shift a b)
      (unless (fixnum? a)
        (raise-type-error 'fxarithmetic-shift "fixnum" a))
      (unless (and (exact-integer? b) (<= lower-bound b 30))
        (raise-type-error 'fxarithmetic-shift bounds b))
      (let ([v (arithmetic-shift a (adjust b))])
        (if (fixnum? v)
            v
            (implementation-restriction 'fxarithmetic-shift v))))))

(define-shifter fxarithmetic-shift r6rs:fxarithmetic-shift
  -30 "exact integer in [-30, 30]" values)
(define-shifter fxarithmetic-shift-left r6rs:fxarithmetic-shift-left
  0 "exact integer in [0, 30]" values)
(define-shifter fxarithmetic-shift-right r6rs:fxarithmetic-shift-right
  0 "exact integer in [0, 30]" -)
