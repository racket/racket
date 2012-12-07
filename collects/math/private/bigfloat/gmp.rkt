#lang racket/base

(require (for-syntax racket/base)
         ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path)

(provide
 _mp_size_t
 _mp_limb_t
 _mp_bitcnt_t
 sizeof-mp_limb_t
 gmp-limb-bits
 _mpz
 _mpz-pointer
 (struct-out mpz)
 new-mpz
 integer->mpz
 mpz->integer
 ;; Low-level
 gmp-lib)

;; ===================================================================================================
;; Setup

(define-runtime-path libgmp-so 
  (case (system-type)
    [(macosx) '(so "libgmp.10.dylib")]
    [(windows) '(so "libgmp-10.dll")]
    [else '(so "libgmp")]))

(define gmp-lib (ffi-lib libgmp-so '("10" "3" "") #:fail (λ () #f)))

(define (get-gmp-fun name type)
  (get-ffi-obj name gmp-lib type (make-not-available name)))

;; ===================================================================================================
;; Types

(define 64-bit? (= 8 (ctype-sizeof _intptr)))
(define win64? (and 64-bit? (eq? (system-type) 'windows)))

;; The Win64 build of libgmp is compiled using mingw64, for which GMP's configure script defines
;; LONG_LONG_LIMB
(define _mp_limb_t (if win64? _ullong _ulong))
(define _mp_size_t _long)  ; used for limb counts
(define _mp_bitcnt_t _ulong)

(define sizeof-mp_limb_t (ctype-sizeof _mp_limb_t))

;; Number of bits in a limb
(define gmp-limb-bits (* 8 sizeof-mp_limb_t))
;; We don't support "nail" builds, which haven't worked since before GMP 4.3 anyway (a "nail" takes
;; two bits from each limb, and is supposed to speed up carries between limbs on certain systems)

;; GMP's big integers
(define-cstruct _mpz ([alloc _int] [size _int] [limbs _pointer]))

;; ALWAYS let GMP manage the memory pointed at by the limbs field in an _mpz. It likes to reallocate
;; limbs, particularly when the _mpz is an output argument.

;; This means that _mpz instances take up memory that Racket can't account for, so they should only
;; be used as temporary values. Possibly permanent values should be either instances of _mpfr or
;; Racket's own exact integers.

(define mpz-init (get-gmp-fun '__gmpz_init (_fun _mpz-pointer -> _void)))
(define mpz-init2 (get-gmp-fun '__gmpz_init2 (_fun _mpz-pointer _mp_bitcnt_t -> _void)))
(define mpz-clear (get-gmp-fun '__gmpz_clear (_fun _mpz-pointer -> _void)))

;; new-mpz : [_mp_bitcnt_t] -> _mpz-pointer
;; Creates an _mpz that is managed by the garbage collector, but whose limbs are not, and which is
;; not relocatable. These are always safe to pass to mpz_* functions.
(define (new-mpz [bits 0])
  (define z (make-mpz 0 0 #f))
  (if (= bits 0) (mpz-init z) (mpz-init2 z bits))
  (register-finalizer z (λ (z) (mpz-clear z)))
  z)

;; ===================================================================================================
;; Conversions

(define mpz-get-si (get-gmp-fun '__gmpz_get_si (_fun _mpz-pointer -> _long)))
(define mpz-fits-long? (get-gmp-fun '__gmpz_fits_slong_p (_fun _mpz-pointer -> _int)))

;; integer->mpz : integer -> _mpz
;; Converts a Racket integer to an _mpz.
(define (integer->mpz n)
  (define abs-n (abs n))
  (define z (new-mpz (integer-length abs-n)))
  (define len (mpz-alloc z))
  (define limbs (mpz-limbs z))
  (let loop ([i 0])
    (when (i . < . len)
      (define bit (* i gmp-limb-bits))
      (ptr-set! limbs _mp_limb_t i (bitwise-bit-field abs-n bit (+ bit gmp-limb-bits)))
      (loop (+ i 1))))
  (define size (ceiling (/ (integer-length abs-n) gmp-limb-bits)))
  (set-mpz-size! z (if (< n 0) (- size) size))
  z)

;; size+limbs->integer : integer (listof integer) -> integer
;; Converts a size (which may be negative) and a limb list into an integer.
(define (size+limbs->integer size limbs)
  (define len (abs size))
  (define num
    (let loop ([i 0] [res  0])
      (cond [(i . < . len)
             (define v (ptr-ref limbs _mp_limb_t i))
             (loop (+ i 1) (bitwise-ior res (arithmetic-shift v (* i gmp-limb-bits))))]
            [else  res])))
  (if (negative? size) (- num) num))

;; mpz->integer : _mpz -> integer
;; Converts an mpz_t to a Racket integer.
(define (mpz->integer z)
  (if (zero? (mpz-fits-long? z))
      (size+limbs->integer (mpz-size z) (mpz-limbs z))
      (mpz-get-si z)))

;; ===================================================================================================
;; Number Theoretic Functions
;; http://gmplib.org/manual/Number-Theoretic-Functions.html#Number-Theoretic-Functions

(define mpz-probab-prime-p 
  (get-gmp-fun '__gmpz_probab_prime_p  (_fun _mpz-pointer _int -> _int)))

(define (probably-prime n [repetitions 10])
  (case (mpz-probab-prime-p (integer->mpz n) repetitions)
    [(0) 'composite]
    [(1) 'probably-prime]
    [(2) 'prime]))

(define (prime? n)
  (case (probably-prime n)
    [(probably-prime prime) #t]
    [else #f]))

(define mpz-nextprime
  (get-gmp-fun '__gmpz_nextprime  (_fun _mpz-pointer _mpz-pointer -> _void)))

(define (next-prime op)
  (define result (new-mpz))
  (mpz-nextprime result (integer->mpz op))
  (mpz->integer result))

(define mpz-invert
  (get-gmp-fun '__gmpz_invert  (_fun _mpz-pointer _mpz-pointer _mpz-pointer -> _void)))

(define (mod-inverse n modulus)
  (define result (new-mpz))
  (mpz-invert result (integer->mpz n) (integer->mpz modulus))
  (mpz->integer result))

(define mpz-jacobi
  (get-gmp-fun '__gmpz_jacobi  (_fun _mpz-pointer _mpz-pointer -> _int)))

(define (jacobi a b)
  (mpz-jacobi (integer->mpz a) (integer->mpz b)))

(define mpz-legendre
  (get-gmp-fun '__gmpz_legendre  (_fun _mpz-pointer _mpz-pointer -> _int)))

(define (legendre a b)
  (mpz-legendre (integer->mpz a) (integer->mpz b)))

(define mpz-remove
  (get-gmp-fun '__gmpz_remove  (_fun _mpz-pointer _mpz-pointer _mpz-pointer -> _int)))

(define (remove-factor z f)
  (define result (new-mpz))
  (define n (mpz-remove result (integer->mpz z) (integer->mpz f)))
  (define z/f (mpz->integer result))
  (values z/f n))
