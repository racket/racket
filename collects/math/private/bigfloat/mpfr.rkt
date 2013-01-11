#lang racket/base

(require (for-syntax racket/base)
         ffi/unsafe
         ffi/unsafe/cvector
         ffi/unsafe/custodian
         ffi/unsafe/define
         racket/math
         racket/runtime-path
         racket/promise
         racket/serialize
         (only-in rnrs/arithmetic/bitwise-6
                  bitwise-first-bit-set)
         "gmp.rkt"
         "utils.rkt")

(provide
 ;; Parameters
 bf-rounding-mode
 bf-min-precision
 bf-max-precision
 bf-precision
 ;; Type predicate
 (rename-out [mpfr? bigfloat?])
 bfcanonicalize
 ;; Accessors
 bigfloat-precision
 bigfloat-signbit
 bigfloat-exponent
 bigfloat-significand
 ;; Conversion to and from Real
 sig+exp->bigfloat
 flonum->bigfloat
 integer->bigfloat
 rational->bigfloat
 real->bigfloat
 bigfloat->sig+exp
 bigfloat->flonum
 bigfloat->integer
 bigfloat->rational
 bigfloat->real
 ;; String conversion
 bigfloat->string
 string->bigfloat
 ;; Main constructor
 bf
 bigfloat-deserialize-info
 ;; Low-level stuff
 mpfr-lib
 get-mpfr-fun
 _rnd_t
 _prec_t
 _sign_t
 _exp_t
 _mpfr_size_limb_t
 _mpfr
 _mpfr-pointer
 (struct-out mpfr))

;; Arithmetic, comparison, and other functions are provided by the macros that create them

;; ===================================================================================================
;; Setup/takedown

(define-runtime-path libmpfr-so
  (case (system-type)
    [(macosx) '(so "libmpfr.4.dylib")]
    [(windows) '(so "libmpfr-4.dll")]
    [else '(so "libmpfr")]))

(define mpfr-lib (ffi-lib libmpfr-so '("4" "1" "") #:fail (λ () #f)))

(define-syntax get-mpfr-fun
  (syntax-rules ()
    [(_ name type) (get-mpfr-fun name type (make-not-available name))]
    [(_ name type fail-thunk) (get-ffi-obj name mpfr-lib type fail-thunk)]))

(define mpfr-free-cache (get-mpfr-fun 'mpfr_free_cache (_fun -> _void)))

(define mpfr-shutdown
  (register-custodian-shutdown 
   mpfr-free-cache ; acts as a "random" object for a shutdown handle
   (λ (free) 
     (when mpfr-lib
       ;; The direct reference here is important, since custodian holds only
       ;; a weak reference to shutdown handle:
       (mpfr-free-cache)))))

;; ===================================================================================================
;; MPFR types

;; Rounding modes (not all of them, just the useful/well-supported ones)
(define _rnd_t (_enum '(nearest zero up down)))

(define _prec_t _long)
(define _sign_t _int)
(define _exp_t _long)

;; In mpfr-impl.h, this is a union of mp_size_t and mp_limb_t
(define _mpfr_size_limb_t
  (if (sizeof-mp_limb_t . > . (ctype-sizeof _mp_size_t)) _mp_limb_t _mp_size_t))

(define sizeof-mpfr_size_limb_t (ctype-sizeof _mpfr_size_limb_t))

(define sizeof-exp_t (ctype-sizeof _exp_t))

;; ===================================================================================================
;; Parameters: rounding mode, precision

;; One of 'nearest 'zero 'up 'down
(define bf-rounding-mode (make-parameter 'nearest))

;; minimum precision (1 bit can't be rounded correctly)
(define bf-min-precision 2)
;; maximum precision (the number when longs are 64 bits is ridiculously large)
(define bf-max-precision _long-max)

(define bf-precision
  (make-parameter 128 (λ (p) (cond [(p . < . bf-min-precision)  bf-min-precision]
                                   [(p . > . bf-max-precision)  bf-max-precision]
                                   [else  p]))))

;; ===================================================================================================
;; _mpfr type (bigfloat)

(define (bigfloat-equal? x1 x2 _)
  (or (and (bfnan? x1) (bfnan? x2))
      (bf=? x1 x2)))

(define (canonicalize-sig+exp sig exp)
  (cond [(zero? sig)  (values 0 0)]
        [else
         (let-values ([(sgn sig)  (cond [(sig . < . 0)  (values -1 (- sig))]
                                        [else           (values 1 sig)])])
           (define shift (bitwise-first-bit-set sig))
           (cond [(shift . > . 0)  (values (* sgn (arithmetic-shift sig (- shift)))
                                           (+ exp shift))]
                 [else  (values (* sgn sig) exp)]))]))

(define (bfcanonicalize x)
  (cond [(bfzero? x)  (if (zero? (bigfloat-signbit x)) (force 0.bf) (force -0.bf))]
        [(bfnan? x)  (force +nan.bf)]
        [(bfinfinite? x)  (if (zero? (bigfloat-signbit x)) (force +inf.bf) (force -inf.bf))]
        [else
         (let*-values ([(sig exp)  (bigfloat->sig+exp x)]
                       [(sig exp)  (canonicalize-sig+exp sig exp)])
           (parameterize ([bf-precision (integer-length sig)])
             (sig+exp->bigfloat sig exp)))]))

(define (bigfloat-hash x recur-hash)
  (let*-values ([(x)  (bfcanonicalize x)]
                [(sig exp)  (bigfloat->sig+exp x)])
    (recur-hash (vector (bigfloat-signbit x) sig exp))))

(define bigfloat-deserialize
  (case-lambda
    [(p x)
     (unless (exact-integer? p)
       (raise-argument-error 'bigfloat-deserialize "Integer" 0 p x))
     (unless (or (string? x) (real? x))
       (raise-argument-error 'bigfloat-deserialize "(U String Real)" 1 p x))
     (parameterize ([bf-precision p])
       (bf x))]
    [(p sig exp)
     (unless (exact-integer? p)
       (raise-argument-error 'bigfloat-deserialize "Integer" 0 p sig exp))
     (unless (exact-integer? sig)
       (raise-argument-error 'bigfloat-deserialize "Integer" 1 p sig exp))
     (unless (exact-integer? exp)
       (raise-argument-error 'bigfloat-deserialize "Integer" 2 p sig exp))
     (parameterize ([bf-precision p])
       (sig+exp->bigfloat sig exp))]))

(define bigfloat-deserialize-info
  (make-deserialize-info
   bigfloat-deserialize
   #f))

(define bigfloat-serialize-info
  (make-serialize-info
   (λ (x)
     (cond [(bfzero? x)      (vector (bigfloat-precision x)
                                     (if (zero? (bigfloat-signbit x)) 0.0 -0.0))]
           [(bfnan? x)       (vector (bigfloat-precision x)
                                     +nan.0)]
           [(bfinfinite? x)  (vector (bigfloat-precision x)
                                     (if (zero? (bigfloat-signbit x)) +inf.0 -inf.0))]
           [else  (define-values (sig exp) (bigfloat->sig+exp (bfcanonicalize x)))
                  (vector (bigfloat-precision x) sig exp)]))
   #'bigfloat-deserialize-info
   #f
   (or (current-load-relative-directory) 
       (current-directory))))

;; _mpfr: a multi-precision float with rounding (the main data type)
(define-cstruct _mpfr ([prec _prec_t] [sign _sign_t] [exp _exp_t] [d _gcpointer])
  #:property prop:custom-print-quotable 'never
  #:property prop:custom-write (λ (b port mode) (bigfloat-custom-write b port mode))
  #:property prop:equal+hash (list bigfloat-equal? bigfloat-hash bigfloat-hash)
  #:property prop:serializable bigfloat-serialize-info)

;; ===================================================================================================
;; Foreign functions

(define mpfr-get-emin (get-mpfr-fun 'mpfr_get_emin (_fun -> _exp_t)))
(define mpfr-get-emax (get-mpfr-fun 'mpfr_get_emax (_fun -> _exp_t)))

;; Allocation/initialization
(define mpfr-set-nan (get-mpfr-fun 'mpfr_set_nan (_fun _mpfr-pointer -> _void)))
(define mpfr-init2 (get-mpfr-fun 'mpfr_init2 (_fun _mpfr-pointer _prec_t -> _void)))
(define mpfr-clear (get-mpfr-fun 'mpfr_clear (_fun _mpfr-pointer -> _void)))
;; A "special free" for strings allocated and returned by mpfr_get_str:
(define mpfr-free-str (get-mpfr-fun 'mpfr_free_str (_fun _pointer -> _void)))

;; Conversions from _mpfr to other types
(define mpfr-get-d (get-mpfr-fun 'mpfr_get_d (_fun _mpfr-pointer _rnd_t -> _double)))
(define mpfr-get-si (get-mpfr-fun 'mpfr_get_si (_fun _mpfr-pointer _rnd_t -> _long)))
(define mpfr-get-z (get-mpfr-fun 'mpfr_get_z (_fun _mpz-pointer _mpfr-pointer _rnd_t -> _int)))
(define mpfr-get-z-2exp
  (get-mpfr-fun 'mpfr_get_z_2exp (_fun _mpz-pointer _mpfr-pointer -> _exp_t)
                (λ () (get-mpfr-fun 'mpfr_get_z_exp
                                    (_fun _mpz-pointer _mpfr-pointer -> _exp_t)))))
(define mpfr-get-str
  (get-mpfr-fun 'mpfr_get_str (_fun _pointer (_cpointer _exp_t) _int _ulong _mpfr-pointer _rnd_t
                                    -> _bytes)))

;; Conversions from other types to _mpfr
(define mpfr-set (get-mpfr-fun 'mpfr_set (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
(define mpfr-set-d (get-mpfr-fun 'mpfr_set_d  (_fun _mpfr-pointer _double _rnd_t -> _void)))
(define mpfr-set-si (get-mpfr-fun 'mpfr_set_si (_fun _mpfr-pointer _long _rnd_t -> _void)))
(define mpfr-set-z (get-mpfr-fun 'mpfr_set_z  (_fun _mpfr-pointer _mpz-pointer _rnd_t -> _void)))
(define mpfr-set-str (get-mpfr-fun 'mpfr_set_str (_fun _mpfr-pointer _string _int _rnd_t -> _int)))

;; Functions without simple or uniformly typed wrappers
(define mpfr-nextabove (get-mpfr-fun 'mpfr_nextabove (_fun _mpfr-pointer -> _void)))
(define mpfr-nextbelow (get-mpfr-fun 'mpfr_nextbelow (_fun _mpfr-pointer -> _void)))
(define mpfr-jn (get-mpfr-fun 'mpfr_jn (_fun _mpfr-pointer _long _mpfr-pointer _rnd_t -> _int)))
(define mpfr-yn (get-mpfr-fun 'mpfr_yn (_fun _mpfr-pointer _long _mpfr-pointer _rnd_t -> _int)))
(define mpfr-root (get-mpfr-fun 'mpfr_root (_fun _mpfr-pointer _mpfr-pointer _ulong _rnd_t -> _int)))

;; mpfr-set-z-exp : _mpfr _mpz integer _rnd_t -> _int
;; Reimplementation of mpfr_set_z_2exp in Racket, used for older versions of MPFR that don't have it
(define (mpfr-set-z-exp x z e rnd)
  (define inex (mpfr-set-z x z rnd))
  (cond
    [(or (bfzero? x) (not (bfrational? x)))
     ;; Zeros and non-rational numbers have exponents outside the range [emin..emax], which MPFR
     ;; nevertheless recognizes as valid. Adding `e' to the exponent of one of those would result in
     ;; an invalid exponent, for which MPFR would raise an assertion failure that would crash Racket.
     inex]
    [else
     ;; Fix up the exponent
     (let ([e  (+ (mpfr-exp x) e)])
       ;; Check the exponent against its valid range; return sensible stuff if it's out of range
       (cond [(e . > . (mpfr-get-emax))
              (if ((mpfr-sign x) . >= . 0)
                  (mpfr-set-d x +inf.0 'zero)
                  (mpfr-set-d x -inf.0 'zero))
              0]
             [(e . < . (mpfr-get-emin))
              (if ((mpfr-sign x) . >= . 0)
                  (mpfr-set-d x +0.0 'zero)
                  (mpfr-set-d x -0.0 'zero))
              0]
             [else
              (set-mpfr-exp! x e)
              inex]))]))

(define mpfr-set-z-2exp
  (get-mpfr-fun 'mpfr_set_z_2exp (_fun _mpfr-pointer _mpz-pointer _exp_t _rnd_t -> _int)
                (λ () mpfr-set-z-exp)))

;; ===================================================================================================
;; Construction

#|
We always create _mpfr instances using new-mpfr, which uses Racket's memory management for the limbs.
In doing so, we assume that no mpfr_* function will ever try to reallocate limbs. This is a good
assumption because an _mpfr's precision is fixed from when it's allocated to when it's deallocated.
There's no reason to allocate new limbs for an _mpfr without changing its precision.
|#

;; raw-mpfr : integer -> bigfloat
;; Creates and initializes an _mpfr instance using MPFR's memory management. This function exists
;; mostly for documentation purposes. It's a simpler version of `new-mpfr'.
;; It's a bad idea to use `raw-mpfr' for long-lived _mpfr objects. Racket can't track the memory used
;; by the limbs of an _mpfr object constructed using `mpfr-init2', meaning that it would think it
;; uses less memory than it actually does.
(define (raw-mpfr prec)
  (define x (make-mpfr 0 0 0 #f))
  (mpfr-init2 x prec)
  (register-finalizer x (λ (x) (mpfr-clear x)))
  x)

;; Reimplementation of `mpfr-init2' starts here

;; mpfr-prec->limbs : integer -> integer
;; Reimplementation of MPFR_PREC2LIMBS
(define (mpfr-prec->limbs prec)
  (+ 1 (quotient (- prec 1) gmp-limb-bits)))

;; mpfr-malloc-size : integer -> integer
;; Reimplementation of MPFR_MALLOC_SIZE
(define (mpfr-malloc-size n)
  (+ sizeof-mpfr_size_limb_t (* sizeof-mp_limb_t n)))

;; Reimplementation of MPFR_EXP_INVALID
(define mpfr-exp-invalid
  (arithmetic-shift 1 (- (quotient (* gmp-limb-bits sizeof-exp_t) sizeof-mp_limb_t) 2)))

;; mpfr-set-alloc-size! : pointer integer -> void
;; Reimplementation of MPFR_SET_ALLOC_SIZE
(define (mpfr-set-alloc-size! d n)
  (ptr-set! d _mp_size_t -1 n))

;; new-mpfr : integer -> bigfloat
;; Creates a new _mpfr instance and initializes it, mimicking `mpfr-init2'.
(define (new-mpfr prec)
  (define n (mpfr-prec->limbs prec))
  ;; Allocate d so it won't be traced (atomic) or moved (interior)
  (define orig-d (malloc (mpfr-malloc-size n) 'atomic-interior))
  ;; An _mpfr object points at the second element of its limbs array, and uses the first element
  ;; to store its size, so shift the pointer
  (define d (ptr-add orig-d 1 _mpfr_size_limb_t))
  ;; Setting the size after shifting the pointer because that's what `mpfr-init2' does...
  (mpfr-set-alloc-size! d n)
  ;; Make an _mpfr object managed by Racket
  (define x (make-mpfr prec 0 mpfr-exp-invalid d))
  ;; Use a finalizer to keep a reference to orig-d as long as x is alive (equiv. to tracing x, if
  ;; the value of d pointed at a Racket object)
  (register-finalizer x (λ (x) orig-d))
  ;; Set +nan.bf because that's what `mpfr-init2' does...
  (mpfr-set-nan x)
  x)

;; ===================================================================================================
;; Accessors

;; bigfloat-precision : bigfloat -> integer
;; Returns the maximum number of nonzero bits in the significand.
(define bigfloat-precision mpfr-prec)

;; bigfloat-signbit : bigfloat -> fixnum
;; Returns the sign bit of a bigfloat.
(define (bigfloat-signbit x)
  (if ((mpfr-sign x) . < . 0) 1 0))

;; bigfloat-exponent : bigfloat -> integer
;; Returns the exponent part of a bigfloat.
(define (bigfloat-exponent x)
  (- (mpfr-exp x) (bigfloat-precision x)))

;; bigfloat->sig+exp : bigfloat -> integer integer
;; Returns the signed significand and exponent of a bigfloat.
(define (bigfloat->sig+exp x)
  (define z (new-mpz))
  (define exp (mpfr-get-z-2exp z x))
  (values (mpz->integer z) exp))

;; bigfloat-significand : bigfloat -> integer
;; Returns just the signed significand of a bigfloat.
(define (bigfloat-significand x)
  (define-values (sig exp) (bigfloat->sig+exp x))
  sig)

;; ===================================================================================================
;; Conversion from Racket data types to bigfloat

;; sig+exp->bigfloat : integer integer -> bigfloat
(define (sig+exp->bigfloat n e)
  (define y (new-mpfr (bf-precision)))
  (mpfr-set-z-2exp y (integer->mpz n) e (bf-rounding-mode))
  y)

;; flonum->bigfloat : float -> bigfloat
;; Converts a Racket inexact real to a bigfloat; rounds if bf-precision < 53.
(define (flonum->bigfloat value)
  (define x (new-mpfr (bf-precision)))
  (mpfr-set-d x value (bf-rounding-mode))
  x)

;; integer->bigfloat : integer -> bigfloat
;; Converts a Racket integer to a bigfloat; rounds if necessary.
(define (integer->bigfloat value)
  (define x (new-mpfr (bf-precision)))
  (if (_long? value)
      (mpfr-set-si x value (bf-rounding-mode))
      (mpfr-set-z x (integer->mpz value) (bf-rounding-mode)))
  x)

(define (round/mode q)
  (case (bf-rounding-mode)
    [(up)    (ceiling q)]
    [(down)  (floor q)]
    [(zero)  (truncate q)]
    [else    (round q)]))

(define (floor-log2 n) (max 0 (sub1 (integer-length n))))
(define (ceiling-log2 n) (max 0 (integer-length (sub1 n))))

(define (log2-lower-bound q)
  (- (floor-log2 (numerator q))
     (ceiling-log2 (denominator q))))

(define (log2-upper-bound q)
  (- (ceiling-log2 (numerator q))
     (floor-log2 (denominator q))))

;; rational->bigfloat : rational -> bigfloat
;; Converts a Racket rational to a bigfloat; rounds if necessary.
(define (rational->bigfloat q)
  (define prec (bf-precision))
  (define sgn-q (sgn q))
  (define abs-q (abs q))
  (define ipart (floor abs-q))
  (cond [(zero? sgn-q)  (sig+exp->bigfloat 0 0)]
        [(zero? ipart)
         (define e-ub (- (log2-upper-bound abs-q) prec))
         (define e-lb (- (log2-lower-bound abs-q) prec))
         (let loop ([e  e-ub])
           (define 2^-e (arithmetic-shift 1 (- e)))
           (define sig (round/mode (* sgn-q abs-q 2^-e)))
           (cond [(or (= e e-lb) (= prec (integer-length (abs sig))))
                  (sig+exp->bigfloat sig e)]
                 [else
                  (loop (- e 1))]))]
        [else
         (define e (- (integer-length ipart) prec))
         (define 2^-e (cond [(e . < . 0)  (arithmetic-shift 1 (- e))]
                            [else  (/ 1 (arithmetic-shift 1 e))]))
         (define sig (round/mode (* sgn-q abs-q 2^-e)))
         (sig+exp->bigfloat sig e)]))

;; real->bigfloat : real -> bigfloat
;; Converts any real Racket value to a bigfloat; rounds if necessary.
(define (real->bigfloat value)
  (cond [(inexact? value)  (flonum->bigfloat value)]
        [(integer? value)  (integer->bigfloat value)]
        [else  (rational->bigfloat value)]))

;; ===================================================================================================
;; Conversion from mpfr_t to Racket data types

;; bigfloat->flonum : bigfloat -> float
;; Converts a bigfloat to a Racket float; rounds if necessary.
(define (bigfloat->flonum x)
  (mpfr-get-d x (bf-rounding-mode)))

;; bigfloat->integer : bigfloat -> integer
;; Converts a bigfloat to a Racket integer; rounds if necessary.
(define (bigfloat->integer x)
  (unless (bfinteger? x) (raise-argument-error 'bigfloat->integer "bfinteger?" x))
  (define z (new-mpz))
  (mpfr-get-z z x (bf-rounding-mode))
  (define res (mpz->integer z))
  res)

;; bigfloat->rational : bigfloat -> rational
;; Converts a bigfloat to a Racket rational; does not round.
(define (bigfloat->rational x)
  (unless (bfrational? x) (raise-argument-error 'bigfloat->rational "bfrational?" x))
  (define-values (sig exp) (bigfloat->sig+exp x))
  (cond [(zero? sig)  0]  ; without this, (bigfloat->rational 0.bf) chews up half a gigabyte
        [else  (* sig (expt 2 exp))]))

; bigfloat->real : bigfloat -> (or exact-rational flonum)
(define (bigfloat->real x)
  (cond [(bfrational? x)  (bigfloat->rational x)]
        [else  (bigfloat->flonum x)]))

;; ===================================================================================================
;; String conversions

(define (mpfr-get-string x base rnd)
  (define exp-ptr (cast (malloc _exp_t 'atomic-interior) _pointer (_cpointer _exp_t)))
  (define bs (mpfr-get-str #f exp-ptr base 0 x rnd))
  (define exp (ptr-ref exp-ptr _exp_t))
  (define str (bytes->string/utf-8 bs))
  (mpfr-free-str bs)
  (values exp str))

(define (remove-trailing-zeros str)
  (let loop ([i  (string-length str)])
    (cond [(zero? i)  "0"]
          [(char=? #\0 (string-ref str (sub1 i)))  (loop (sub1 i))]
          [(char=? #\. (string-ref str (sub1 i)))  (substring str 0 (sub1 i))]
          [else  (substring str 0 i)])))

(define (scientific-string exp str)
  (define n (string-length str))
  (cond [(= n 0)  "0"]
        [else
         (define sig (remove-trailing-zeros (format "~a.~a" (substring str 0 1) (substring str 1))))
         (if (= exp 1) sig (format "~ae~a" sig (number->string (- exp 1))))]))

(define (decimal-string-length exp digs)
  (cond [(exp . > . (string-length digs))
         (+ (string-length digs) (- exp (string-length digs)))]
        [(exp . <= . 0)
         (let ([digs  (remove-trailing-zeros digs)])
           (cond [(equal? digs "0")  1]
                 [else  (+ 2 (- exp) (string-length digs))]))]
        [else
         (string-length
          (remove-trailing-zeros
           (string-append (substring digs 0 exp) "." (substring digs exp))))]))

(define (decimal-string exp digs)
  (cond [(exp . > . (string-length digs))
         (string-append digs (make-string (- exp (string-length digs)) #\0))]
        [(exp . <= . 0)
         (remove-trailing-zeros
          (string-append "0." (make-string (- exp) #\0) digs))]
        [else
         (remove-trailing-zeros
          (string-append (substring digs 0 exp) "." (substring digs exp)))]))

;; Converts a bigfloat to a Racket string of digits, with a decimal point.
;; Outputs enough digits to exactly recreate the bigfloat using string->bigfloat.
(define (bigfloat->string x)
  (cond
    [(bfzero? x)  (if (= 0 (bigfloat-signbit x)) "0.0" "-0.0")]
    [(bfinfinite? x)  (if (= 0 (bigfloat-signbit x)) "+inf.bf" "-inf.bf")]
    [(bfnan? x)   "+nan.bf"]
    [else
     (define-values (exp str) (mpfr-get-string x 10 'nearest))
     (cond
       [(not str)  (error 'bigfloat->string "string conversion failed for ~e"
                          (number->string (bigfloat->rational x)))]
       [else
        (define-values (sign digs)
          (if (char=? (string-ref str 0) #\-)
              (values "-" (substring str 1))
              (values "" str)))
        (define sstr (scientific-string exp digs))
        (define dlen (decimal-string-length exp digs))
        (cond [((string-length sstr) . < . dlen)  (string-append sign sstr)]
              [else  (string-append sign (decimal-string exp digs))])])]))

;; string->bigfloat : string [integer] -> bigfloat
;; Converts a Racket string to a bigfloat.
(define (string->bigfloat str)
  (case str
    [("-inf.bf" "-inf.0" "-inf.f")  (force -inf.bf)]
    [("-1.bf")  (force -1.bf)]
    [("-0.bf")  (force -0.bf)]
    [( "0.bf")  (force  0.bf)]
    [( "1.bf")  (force  1.bf)]
    [("+inf.bf" "+inf.0" "+inf.f")  (force +inf.bf)]
    [("+nan.bf" "+nan.0" "+nan.f")  (force +nan.bf)]
    [else
     (define y (new-mpfr (bf-precision)))
     (define bs (string->bytes/utf-8 str))
     (if (zero? (mpfr-set-str y bs 10 'nearest)) y #f)]))

(define (bigfloat-custom-write x port mode)
  (cond
    [(and mpfr-lib gmp-lib)
     ;; Only try to print if libmpfr was loaded; otherwise, we get an infinite loop when the
     ;; `make-not-available' handler tries to print any _mpfr arguments
     (write-string
      (cond [(bfzero? x)  (if (= 0 (bigfloat-signbit x)) "0.bf" "-0.bf")]
            [(bfrational? x)
             (define str (bigfloat->string x))
             (cond [(regexp-match #rx"\\.|e" str)
                    (define exp (bigfloat-exponent x))
                    (define prec (bigfloat-precision x))
                    (if ((abs exp) . > . (* prec 2))
                        (format "(bf \"~a\")" str)
                        (format "(bf #e~a)" str))]
                   [else  (format "(bf ~a)" str)])]
            [(bfinfinite? x)  (if (= 0 (bigfloat-signbit x)) "+inf.bf" "-inf.bf")]
            [else  "+nan.bf"])
      port)]
    [else
     (write-string "#<_mpfr>" port)]))

;; ===================================================================================================
;; Main bigfloat constructor

;; bf : (or real string) -> bigfloat
;;    : integer integer -> bigfloat
(define bf
  (case-lambda
    [(v)  (cond [(string? v)
                 (define x (string->bigfloat v))
                 (if x x (error 'bf "expected well-formed decimal number; given ~e" v))]
                [else
                 (real->bigfloat v)])]
    [(n e)  (sig+exp->bigfloat n e)]))

;; ===================================================================================================
;; Unary functions

(define-for-syntax 1ary-funs (list))
(provide (for-syntax 1ary-funs))

(define-syntax-rule (provide-1ary-fun name c-name)
  (begin
    (define cfun (get-mpfr-fun c-name (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define (name x)
      (define y (new-mpfr (bf-precision)))
      (cfun y x (bf-rounding-mode))
      y)
    (provide name)
    (begin-for-syntax (set! 1ary-funs (cons #'name 1ary-funs)))))

(define-syntax-rule (provide-1ary-funs [name c-name] ...)
  (begin (provide-1ary-fun name c-name) ...))

(provide-1ary-funs
 [bfsqr 'mpfr_sqr]
 [bfsqrt 'mpfr_sqrt]
 [bf1/sqrt 'mpfr_rec_sqrt]
 [bfcbrt 'mpfr_cbrt]
 [bfneg 'mpfr_neg]
 [bfabs 'mpfr_abs]
 [bflog 'mpfr_log]
 [bflog2 'mpfr_log2]
 [bflog10 'mpfr_log10]
 [bflog1p 'mpfr_log1p]
 [bfexp 'mpfr_exp]
 [bfexp2 'mpfr_exp2]
 [bfexp10 'mpfr_exp10]
 [bfexpm1 'mpfr_expm1]
 [bfcos 'mpfr_cos]
 [bfsin 'mpfr_sin]
 [bftan 'mpfr_tan]
 [bfsec 'mpfr_sec]
 [bfcsc 'mpfr_csc]
 [bfcot 'mpfr_cot]
 [bfacos 'mpfr_acos]
 [bfasin 'mpfr_asin]
 [bfatan 'mpfr_atan]
 [bfcosh 'mpfr_cosh]
 [bfsinh 'mpfr_sinh]
 [bftanh 'mpfr_tanh]
 [bfsech 'mpfr_sech]
 [bfcsch 'mpfr_csch]
 [bfcoth 'mpfr_coth]
 [bfacosh 'mpfr_acosh]
 [bfasinh 'mpfr_asinh]
 [bfatanh 'mpfr_atanh]
 [bfeint 'mpfr_eint]
 [bfli2 'mpfr_li2]
 [bfgamma 'mpfr_gamma]
 [bfpsi0 'mpfr_digamma]
 [bfzeta 'mpfr_zeta]
 [bferf 'mpfr_erf]
 [bferfc 'mpfr_erfc]
 [bfbesj0 'mpfr_j0]
 [bfbesj1 'mpfr_j1]
 [bfbesy0 'mpfr_y0]
 [bfbesy1 'mpfr_y1]
 [bfrint 'mpfr_rint]
 [bffrac 'mpfr_frac]
 [bfcopy 'mpfr_set])

(begin-for-syntax
  (set! 1ary-funs (remove* (list #'bfneg) 1ary-funs free-identifier=?)))

(define (bfsgn x)
  (cond [(bfzero? x)  x]
        [(= 0 (bigfloat-signbit x))  (force 1.bf)]
        [else  (force -1.bf)]))

(define (bfround x)
  (parameterize ([bf-rounding-mode  'nearest])
    (bfrint x)))

(provide bfsgn bfround)
(begin-for-syntax
  (set! 1ary-funs (list* #'bfsgn #'bfround 1ary-funs)))

(define mpfr-fac-ui (get-mpfr-fun 'mpfr_fac_ui (_fun _mpfr-pointer _ulong _rnd_t -> _int)))

(define (bffactorial n)
  (cond [(n . < . 0)  (raise-argument-error 'bffactorial "Natural" n)]
        [(n . > . 100000000)  (force +inf.bf)]
        [else  (define y (new-mpfr (bf-precision)))
               (mpfr-fac-ui y n (bf-rounding-mode))
               y]))

(provide bffactorial)

(define mpfr-sum (get-mpfr-fun 'mpfr_sum (_fun _mpfr-pointer (_list i _mpfr-pointer) _ulong
                                               _rnd_t -> _int)))

(define (bfsum xs)
  (define y (new-mpfr (bf-precision)))
  (mpfr-sum y xs (length xs) (bf-rounding-mode))
  y)

(provide bfsum)

(define-syntax-rule (provide-1ary-fun/noround name c-name)
  (begin
    (define cfun (get-mpfr-fun c-name (_fun _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define (name x)
      (define y (new-mpfr (bf-precision)))
      (cfun y x (bf-rounding-mode))
      y)
    (provide name)
    (begin-for-syntax (set! 1ary-funs (cons #'name 1ary-funs)))))

(define-syntax-rule (provide-1ary-funs/noround [name c-name] ...)
  (begin (provide-1ary-fun/noround name c-name) ...))

(provide-1ary-funs/noround
 [bfceiling 'mpfr_ceil]
 [bffloor 'mpfr_floor]
 [bftruncate 'mpfr_trunc])

(define-for-syntax 1ary2-funs (list))
(provide (for-syntax 1ary2-funs))

(define-syntax-rule (provide-1ary2-fun name c-name)
  (begin
    (define cfun
      (get-mpfr-fun c-name (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define (name x)
      (define y (new-mpfr (bf-precision)))
      (define z (new-mpfr (bf-precision)))
      (cfun y z x (bf-rounding-mode))
      (values y z))
    (provide name)
    (begin-for-syntax (set! 1ary2-funs (cons #'name 1ary2-funs)))))

(define-syntax-rule (provide-1ary2-funs [name c-name] ...)
  (begin (provide-1ary2-fun name c-name) ...))

(provide-1ary2-funs
 [bfsin+cos 'mpfr_sin_cos]
 [bfsinh+cosh 'mpfr_sinh_cosh]
 [bfmodf 'mpfr_modf])

(define mpfr-lgamma
  (get-mpfr-fun 'mpfr_lgamma (_fun _mpfr-pointer _pointer _mpfr-pointer _rnd_t -> _int)))

(define (bflog-gamma/sign x)
  (define y (new-mpfr (bf-precision)))
  (define s (malloc _int 'atomic-interior))
  (mpfr-lgamma y s x (bf-rounding-mode))
  (values y (ptr-ref s _int)))

(define (bflog-gamma x)
  (define-values (y _) (bflog-gamma/sign x))
  y)

(provide bflog-gamma/sign bflog-gamma)
(begin-for-syntax
  (set! 1ary-funs (list* #'bflog-gamma 1ary-funs)))

;; ===================================================================================================
;; Unary predicates

(define-for-syntax 1ary-preds (list))
(provide (for-syntax 1ary-preds))

(define-syntax-rule (provide-1ary-pred name c-name)
  (begin
    (define cfun (get-mpfr-fun c-name (_fun _mpfr-pointer -> _int)))
    (define (name x) (not (zero? (cfun x))))
    (provide name)
    (begin-for-syntax (set! 1ary-preds (cons #'name 1ary-preds)))))

(define-syntax-rule (provide-1ary-preds [name c-name] ...)
  (begin (provide-1ary-pred name c-name) ...))

(provide-1ary-preds
 [bfnan?  'mpfr_nan_p]
 [bfinfinite?  'mpfr_inf_p]
 [bfrational? 'mpfr_number_p]
 [bfinteger? 'mpfr_integer_p]
 [bfzero? 'mpfr_zero_p])

(define (bfpositive? x)
  (bfgt? x (force 0.bf)))

(define (bfnegative? x)
  (bflt? x (force 0.bf)))

(define (bfeven? x)
  (and (bfinteger? x) (even? (bigfloat->integer x))))

(define (bfodd? x)
  (and (bfinteger? x) (odd? (bigfloat->integer x))))

(provide bfpositive? bfnegative? bfeven? bfodd?)
(begin-for-syntax
  (set! 1ary-preds (append (list #'bfpositive? #'bfnegative? #'bfeven? #'bfodd?)
                           1ary-preds)))

;; ===================================================================================================
;; Binary functions

(define-for-syntax 2ary-funs (list))
(provide (for-syntax 2ary-funs))

(define-syntax-rule (provide-2ary-fun name c-name)
  (begin
    (define cfun
      (get-mpfr-fun c-name (_fun _mpfr-pointer _mpfr-pointer _mpfr-pointer _rnd_t -> _int)))
    (define (name x1 x2)
      (define y (new-mpfr (bf-precision)))
      (cfun y x1 x2 (bf-rounding-mode))
      y)
    (provide name)
    (begin-for-syntax (set! 2ary-funs (cons #'name 2ary-funs)))))

(define-syntax-rule (provide-2ary-funs [name c-name] ...)
  (begin (provide-2ary-fun name c-name) ...))

(provide-2ary-funs
 [bfadd 'mpfr_add]
 [bfsub 'mpfr_sub]
 [bfmul 'mpfr_mul]
 [bfdiv 'mpfr_div]
 [bfremainder 'mpfr_fmod]  ; this may not be right
 [bfexpt 'mpfr_pow]
 [bfmax2 'mpfr_max]
 [bfmin2 'mpfr_min]
 [bfatan2 'mpfr_atan2]
 [bfhypot 'mpfr_hypot]
 [bfagm 'mpfr_agm])

(begin-for-syntax
  (set! 2ary-funs (remove* (list #'bfadd #'bfsub #'bfmul #'bfdiv #'bfmax2 #'bfmin2)
                           2ary-funs
                           free-identifier=?)))

(define (bfbesj n x)
  (unless (_long? n) (raise-argument-error 'bfbesj "_long?" 0 n x))
  (define y (new-mpfr (bf-precision)))
  (mpfr-jn y n x (bf-rounding-mode))
  y)

(define (bfbesy n x)
  (unless (_long? n) (raise-argument-error 'bfbesy "_long?" 0 n x))
  (define y (new-mpfr (bf-precision)))
  (mpfr-yn y n x (bf-rounding-mode))
  y)

(define (bfroot x n)
  (unless (and (_ulong? n) (n . >= . 0)) (raise-argument-error 'bfroot "_ulong?" 1 x n))
  (define y (new-mpfr (bf-precision)))
  (mpfr-root y x n (bf-rounding-mode))
  y)

(define (bfnext x)
  (define y (bfcopy x))
  (mpfr-nextabove y)
  y)

(define (bfprev x)
  (define y (bfcopy x))
  (mpfr-nextbelow y)
  y)

(define (bfshift x n)
  (unless (fixnum? n) (raise-argument-error 'bfshift "Fixnum" 1 x n))
  (cond [(bfzero? x)  x]
        [(not (bfrational? x))  x]
        [else  (define-values (sig exp) (bigfloat->sig+exp x))
               (bf sig (+ n exp))]))

(define (infinite-ordinal p)
  (+ 1 (arithmetic-shift #b1111111111111111111111111111111 (- p 1))))

(define (bigfloat->ordinal x)
  (cond [(bfzero? x)  0]
        [else
         (define p (bf-precision))
         (let loop ([x  (bfcopy x)])
           (cond [(bfnegative? x)  (- (loop (bfneg x)))]
                 [(bfinfinite? x)  (infinite-ordinal p)]
                 [(bfnan? x)  (+ 1 (infinite-ordinal p))]
                 [else
                  (define-values (sig exp) (bigfloat->sig+exp x))
                  (+ 1 (bitwise-ior (arithmetic-shift (+ exp (- p (mpfr-get-emin))) (- p 1))
                                    (bitwise-xor (arithmetic-shift 1 (- p 1)) sig)))]))]))

(define (ordinal->bigfloat n)
  (define p (bf-precision))
  (cond [(zero? n)  (bf 0)]
        [(negative? n)  (bfneg (ordinal->bigfloat (- n)))]
        [(n . > . (infinite-ordinal p))  (force +nan.bf)]
        [else
         (let ([n  (- n 1)])
           (define exp (- (arithmetic-shift n (- 1 p)) (- p (mpfr-get-emin))))
           (define high-bit (arithmetic-shift 1 (- p 1)))
           (define sig (bitwise-ior high-bit (bitwise-and n (- high-bit 1))))
           (bf sig exp))]))

(define (bigfloats-between x y)
  (- (bigfloat->ordinal y) (bigfloat->ordinal x)))
  
(define (bfstep x n)
  (cond [(or (bfnan? x) (= n 0))  x]
        [else
         (let loop ([x  (bfcopy x)] [n n])
           (cond [(negative? n)  (bfneg (loop (bfneg x) (- n)))]
                 [(bfinfinite? x)
                  (cond [(bfpositive? x)  x]
                        [else  (loop (bfnext x) (- n 1))])]
                 [else
                  (define p (bf-precision))
                  (define i (+ n (bigfloat->ordinal x)))
                  (cond [(and (i . > . 0) (i . > . (infinite-ordinal p)))  (force +inf.bf)]
                        [else  (ordinal->bigfloat i)])]))]))

(provide bfbesj bfbesy bfroot
         bigfloat->ordinal ordinal->bigfloat bigfloats-between bfshift bfstep bfprev bfnext)

;; ===================================================================================================
;; Binary predicates

(define-syntax-rule (provide-2ary-pred name c-name)
  (begin (define cfun (get-mpfr-fun c-name (_fun _mpfr-pointer _mpfr-pointer -> _int)))
         (define (name x1 x2)
           (not (zero? (cfun x1 x2))))
         (provide name)))

(define-syntax-rule (provide-2ary-preds [name c-name] ...)
  (begin (provide-2ary-pred name c-name) ...))

(provide-2ary-preds
 [bf=? 'mpfr_equal_p]
 [bflt?  'mpfr_less_p]
 [bflte? 'mpfr_lessequal_p]
 [bfgt?  'mpfr_greater_p]
 [bfgte? 'mpfr_greaterequal_p])

;; ===================================================================================================
;; Constants and variable-precision constants (i.e. 0-ary functions)

(define-for-syntax consts (list))
(provide (for-syntax consts))

(define-syntax-rule (define-bf-constant name prec expr)
  (begin
    (define name (lazy (parameterize ([bf-precision  prec]) expr)))
    (provide name)
    (begin-for-syntax
      (set! consts (cons #'name consts)))))

(define-bf-constant -inf.bf 2 (flonum->bigfloat -inf.0))
(define-bf-constant -0.bf   2 (flonum->bigfloat -0.0))
(define-bf-constant  0.bf   2 (flonum->bigfloat  0.0))
(define-bf-constant +inf.bf 2 (flonum->bigfloat +inf.0))
(define-bf-constant +nan.bf 2 (flonum->bigfloat +nan.0))

(define-bf-constant 1.bf 4 (flonum->bigfloat 1.0))
(define-bf-constant 2.bf 4 (flonum->bigfloat 2.0))
(define-bf-constant 3.bf 4 (flonum->bigfloat 3.0))
(define-bf-constant 4.bf 4 (flonum->bigfloat 4.0))
(define-bf-constant 5.bf 4 (flonum->bigfloat 5.0))
(define-bf-constant 6.bf 4 (flonum->bigfloat 6.0))
(define-bf-constant 7.bf 4 (flonum->bigfloat 7.0))
(define-bf-constant 8.bf 4 (flonum->bigfloat 8.0))
(define-bf-constant 9.bf 4 (flonum->bigfloat 9.0))
(define-bf-constant 10.bf 4 (flonum->bigfloat 10.0))

(define-bf-constant -1.bf 4 (flonum->bigfloat -1.0))
(define-bf-constant -2.bf 4 (flonum->bigfloat -2.0))
(define-bf-constant -3.bf 4 (flonum->bigfloat -3.0))
(define-bf-constant -4.bf 4 (flonum->bigfloat -4.0))
(define-bf-constant -5.bf 4 (flonum->bigfloat -5.0))
(define-bf-constant -6.bf 4 (flonum->bigfloat -6.0))
(define-bf-constant -7.bf 4 (flonum->bigfloat -7.0))
(define-bf-constant -8.bf 4 (flonum->bigfloat -8.0))
(define-bf-constant -9.bf 4 (flonum->bigfloat -9.0))
(define-bf-constant -10.bf 4 (flonum->bigfloat -10.0))

(define-for-syntax 0ary-funs (list))
(provide (for-syntax 0ary-funs))

(define-syntax-rule (provide-0ary-fun name c-name)
  (begin
    (define cfun (get-mpfr-fun c-name (_fun _mpfr-pointer _rnd_t -> _int)))
    (define (name)
      (let ([y  (new-mpfr (bf-precision))])
        (cfun y (bf-rounding-mode))
        y))
    (provide name)
    (begin-for-syntax (set! 0ary-funs (cons #'name 0ary-funs)))))

(define-syntax-rule (provide-0ary-funs [name c-name] ...)
  (begin (provide-0ary-fun name c-name) ...))

(provide-0ary-funs
 [log2.bf 'mpfr_const_log2]
 [pi.bf 'mpfr_const_pi]
 [gamma.bf 'mpfr_const_euler]
 [catalan.bf 'mpfr_const_catalan])

(define constant-hash (make-hash))

(define (phi.bf)
  (define p (bf-precision))
  (hash-ref!
   constant-hash (cons 'phi.bf p)
   (λ () (bfcopy
          (parameterize ([bf-precision (+ p 10)])
            (bfdiv (bfadd (force 1.bf) (bfsqrt (force 5.bf))) (force 2.bf)))))))

(define (epsilon.bf)
  (define p (bf-precision))
  (hash-ref! constant-hash (cons 'epsilon.bf p) (λ () (bfexpt (force 2.bf) (bf (- 1 p))))))

(define (-max.bf) (bfnext (bf -inf.0)))
(define (-min.bf) (bfprev (bf -0.0)))
(define (+min.bf) (bfnext (bf 0.0)))
(define (+max.bf) (bfprev (bf +inf.0)))

(provide phi.bf epsilon.bf -max.bf -min.bf +min.bf +max.bf)
(begin-for-syntax
  (set! 0ary-funs (list* #'phi.bf #'epsilon.bf #'-max.bf #'-min.bf #'+min.bf #'+max.bf
                         0ary-funs)))
