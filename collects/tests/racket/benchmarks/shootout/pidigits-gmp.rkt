#lang racket/base

;; The Computer Language Shootout
;; http://shootout.alioth.debian.org/
;; Based on the Perl version of the benchmark
;; adapted with a GMP interface by Eli Barzilay
;;
;; Note that this was later used by several other shootout submissions
;; without attribution.

(require racket/cmdline
         (for-syntax racket/base)
         ffi/unsafe)

;; quick libgmp interface, limited to what we need below
(define libgmp (ffi-lib "libgmp"))
(define-syntax-rule (defgmp op type ...)
  (define op (get-ffi-obj (format "__gmpz_~a" 'op) libgmp (_fun type ...))))
(define-cstruct _mpz ([alloc _int] [size _int] [limbs _pointer]))
(defgmp init_set_ui _mpz-pointer _ulong -> _void)
(defgmp set_ui _mpz-pointer _ulong -> _void)
(defgmp get_ui    _mpz-pointer -> _ulong)
(defgmp add       _mpz-pointer _mpz-pointer _mpz-pointer -> _void)
(defgmp mul       _mpz-pointer _mpz-pointer _mpz-pointer -> _void)
(defgmp mul_ui    _mpz-pointer _mpz-pointer _long        -> _void)
(defgmp addmul    _mpz-pointer _mpz-pointer _mpz-pointer -> _void)
(defgmp addmul_ui _mpz-pointer _mpz-pointer _ulong       -> _void)
(defgmp submul_ui _mpz-pointer _mpz-pointer _ulong       -> _void)
(defgmp tdiv_q    _mpz-pointer _mpz-pointer _mpz-pointer -> _void)
(defgmp cmp       _mpz-pointer _mpz-pointer              -> _int)
(define (make-ui n) (let ([i (make-mpz 0 0 #f)]) (init_set_ui i n) i))
;; "fancy" parser, for fun (only for the limited subset we use)
(define-syntax (gmp stx)
  (define (sym=? x y)
    (eq? (if (syntax? x) (syntax-e x) x) (if (syntax? y) (syntax-e y) y)))
  (define (_? stx)
    (and (identifier? stx)
         (regexp-match? #rx"^_" (symbol->string (syntax-e stx)))))
  (define (split xs)
    (let loop ([xs xs] [cur '()] [r '()])
      (define (add) (cons (reverse cur) r))
      (cond [(null? xs) (reverse (add))]
            [(syntax-case (car xs) (unquote) [,x #'x] [else #f])
             => (lambda (x) (loop (cdr xs) (list x) (add)))]
            [else (loop (cdr xs) (cons (car xs) cur) r)])))
  (define (translate expr)
    (syntax-case* expr (= += -= + * / < >) sym=?
      [(x = y + z)  #'(add x y z)]
      [(x = y * z)  #`(#,(if (_? #'z) #'mul #'mul_ui) x y z)]
      [(x += y * z) #`(#,(if (_? #'z) #'addmul #'addmul_ui) x y z)]
      [(x -= y * z) #`(#,(if (_? #'z) #'submul #'submul_ui) x y z)]
      [(x = y / z)  #'(tdiv_q x y z)]
      [(x < y)      #'(< (cmp x y) 0)]
      [(x > y)      #'(> (cmp x y) 0)]
      [(get x)      #'(get_ui x)]))
  (syntax-case stx ()
    [(_ x ...) #`(begin #,@(map translate (split (syntax->list #'(x ...)))))]))

;; the actual code

(define (digits n)
  (define i 0)
  (define _x0 (make-ui 1))
  (define _x1 (make-ui 0))
  (define _x2 (make-ui 1))
  (define _r  (make-ui 0))

  (define (extract-digit n)
    (gmp _r = _x0 * n, _r = _r + _x1, _r = _r / _x2, get _r))

  (let loop ([k 0])

    (define-syntax-rule (compose1!+loop)
      (let* ([k (add1 k)] [y2 (add1 (* k 2))])
        (gmp _x1 = _x1 * y2, _x1 += _x0 * (* y2 2), _x0 = _x0 * k,_x2 = _x2 * y2)
        (loop k)))
    (define-syntax-rule (compose2! d)
      (begin (gmp _x1 -= _x2 * d, _x1 = _x1 * 10, _x0 = _x0 * 10)
             (loop k)))

    (if (gmp _x0 > _x1)
      (compose1!+loop)
      (let ([d (extract-digit 3)])
        (if (not (= d (extract-digit 4)))
          (compose1!+loop)
          (begin (display d)
                 (set! i (add1 i))
                 (let ([m (modulo i 10)])
                   (when (zero? m) (printf "\t:~a\n" i))
                   (if (< i n)
                     (compose2! d)
                     (unless (zero? m)
                       (printf "~a\t:~a\n"
                               (make-string (- 10 m) #\space)
                               n))))))))))

(digits (command-line #:args (n) (string->number n)))
