#lang racket/base
(require ffi2
         racket/fixnum
         racket/flonum
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define N 10000000)

(define-syntax-rule (time* lbl e)
  (begin
    (displayln 'lbl)
    (collect-garbage)
    (println (time e))
    (newline)))

;; ----------------------------------------

(let ()
  (local-require ffi/unsafe)

  (define double_sum
    (get-ffi-obj 'double_sum test-lib (_fun _double _double -> _double)))

  (time*
   1-double-sum
   (let loop ([i N] [v 0.0])
     (if (eqv? i 0)
         (fl+ v)
         (loop (fx- i 1) (double_sum 0.0 0.0))))))

(define-ffi2-procedure double_sum (double_t double_t . -> . double_t)
  #:lib test-lib)

(time*
 2-double-sum
 (let loop ([i N] [v 0.0])
   (if (eqv? i 0)
       (fl+ v)
       (loop (fx- i 1) (double_sum 0.0 0.0)))))

(define-ffi2-procedure atomic_double_sum (double_t double_t . -> . double_t
                                                   #:atomic)
  #:c-id double_sum
  #:lib test-lib)

(time*
 2a-double-sum
 (let loop ([i N] [v 0.0])
   (if (eqv? i 0)
       (fl+ v)
       (loop (fx- i 1) (atomic_double_sum 0.0 0.0)))))

(time*
 r-double-sum
 (let loop ([i N] [v 0.0])
   (if (eqv? i 0)
       (fl+ v)
       (loop (fx- i 1) ((black-box fl+) 0.0 0.0)))))

(time*
 ri-double-sum
 (let loop ([i N] [v 0.0])
   (if (eqv? i 0)
       (fl+ v)
       (loop (fx- i 1) (fl+ 0.0 0.0)))))

;; ----------------------------------------

(let ()
  (local-require ffi/unsafe)
  (define f (malloc 10))
  (memset f 3 10)

  (time*
   1-read-write
   (let loop ([i 10000000])
     (cond
       [(eqv? i 0)
        (ptr-ref f _int8)]
       [else
        (ptr-set! f _int8 1 (ptr-ref f _int8 0))
        (ptr-set! f _int8 0 (ptr-ref f _int8 1))
        (loop (fx- i 1))]))))

(define f (ffi2-malloc byte_t 10))
(ffi2-memset f 3 10)

(time*
 2-read-write
 (let loop ([i N])
   (cond
     [(eqv? i 0)
      (ffi2-ref f int8_t)]
     [else
      (ffi2-set! f int8_t 1 (ffi2-ref f int8_t 0))
      (ffi2-set! f int8_t 0 (ffi2-ref f int8_t 1))
      (loop (fx- i 1))])))

;; ----------------------------------------

(clean-ffi2-lib)
