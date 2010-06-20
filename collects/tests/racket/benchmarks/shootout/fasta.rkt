#lang racket/base

;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; fasta - benchmark
;;
;; Very loosely based on the Chicken variant by Anthony Borla, some
;; optimizations taken from the GCC version by Petr Prokhorenkov, and
;; additional optimizations by Eli Barzilay (not really related to the
;; above two now).

(define +alu+
  (bytes-append #"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
                #"GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
                #"CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
                #"ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
                #"GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
                #"AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
                #"AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))

(define IUB
  '([#\a 0.27] [#\c 0.12] [#\g 0.12] [#\t 0.27] [#\B 0.02]
    [#\D 0.02] [#\H 0.02] [#\K 0.02] [#\M 0.02] [#\N 0.02]
    [#\R 0.02] [#\S 0.02] [#\V 0.02] [#\W 0.02] [#\Y 0.02]))

(define HOMOSAPIEN
  '([#\a 0.3029549426680] [#\c 0.1979883004921]
    [#\g 0.1975473066391] [#\t 0.3015094502008]))

(define line-length 60)

;; ----------------------------------------

(require racket/cmdline racket/require (for-syntax racket/base)
         (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                       racket/unsafe/ops))

;; ----------------------------------------

(define (repeat-fasta header N sequence)
  (define out (current-output-port))
  (define len (bytes-length sequence))
  (define buf (make-bytes (+ len line-length)))
  (bytes-copy! buf 0 sequence)
  (bytes-copy! buf len sequence 0 line-length)
  (display header out)
  (let loop ([n N] [start 0])
    (when (fx> n 0)
      (let ([end (fx+ start (fxmin n line-length))])
        (write-bytes buf out start end)
        (newline)
        (loop (fx- n line-length) (if (fx> end len) (fx- end len) end))))))

;; ----------------------------------------

(define IA 3877)
(define IC 29573)
(define IM 139968)
(define IM.0 (fx->fl IM))

(define (random-next cur) (fxmodulo (fx+ IC (fx* cur IA)) IM))

(define (make-lookup-table frequency-table)
  (define v (make-bytes IM))
  (let loop ([t frequency-table] [c 0] [c. 0.0])
    (unless (null? t)
      (let* ([c1. (fl+ c. (fl* IM.0 (cadar t)))]
             [c1 (inexact->exact (flceiling c1.))]
             [b (char->integer (caar t))])
        (for ([i (in-range c c1)]) (bytes-set! v i b))
        (loop (cdr t) c1 c1.))))
  v)

(define (random-fasta header N table R)
  (define out (current-output-port))
  (define lookup-byte (make-lookup-table table))
  (define (n-randoms to R)
    (let loop ([n 0] [R R])
      (if (fx< n to)
        (let ([R (random-next R)])
          (bytes-set! buf n (bytes-ref lookup-byte R))
          (loop (fx+ n 1) R))
        (begin (write-bytes buf out 0 (fx+ to 1)) R))))
  (define buf (make-bytes (add1 line-length)))
  (define LF (char->integer #\newline))
  (bytes-set! buf line-length LF)
  (display header out)
  (let-values ([(full-lines last) (quotient/remainder N line-length)])
    (let loop ([i full-lines] [R R])
      (cond [(fx> i 0) (loop (fx- i 1) (n-randoms line-length R))]
            [(fx> last 0) (bytes-set! buf last LF) (n-randoms last R)]
            [else R]))))

;; ----------------------------------------

(let ([n (command-line #:args (n) (string->number n))])
  (repeat-fasta ">ONE Homo sapiens alu\n" (* n 2) +alu+)
  (random-fasta ">THREE Homo sapiens frequency\n" (* n 5) HOMOSAPIEN
                (random-fasta ">TWO IUB ambiguity codes\n" (* n 3) IUB 42))
  (void))
