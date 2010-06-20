#lang racket/base

;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; fasta - benchmark
;;
;; Very loosely based on the Chicken variant by Anthony Borla,
;; some optimizations taken from the GCC version by Petr Prokhorenkov,
;; and some more optimizations added by Eli Barzilay.

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

(require racket/require racket/require-syntax (for-syntax racket/base))
(define-require-syntax overriding-in
  (syntax-rules () [(_ R1 R2) (combine-in R2 (subtract-in R1 R2))]))
(require (overriding-in
          racket/flonum
          (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                       racket/unsafe/ops))
         racket/cmdline)

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

(define (fl->fx f) (inexact->exact (flfloor f)))

(define IA 3877)
(define IC 29573)
(define IM 139968)
(define IM.0 (fx->fl IM))
(define random-state 42)

(define (random-next)
  (set! random-state (fxmodulo (fx+ IC (fx* random-state IA)) IM))
  random-state)

(define (make-lookup-vectors frequency-table)
  (define byte-vec (make-bytes IM))
  (define cumu-vec (make-flvector IM))
  (define (set-range from to b)
    (for ([i (in-range (fl->fx from) (fl->fx (flround to)))])
      (bytes-set! byte-vec i b)
      (flvector-set! cumu-vec i from)))
  (let loop ([t frequency-table] [c 0.0])
    (unless (null? t)
      (let ([c1 (fl+ c (fl* IM.0 (cadar t)))])
        (set-range c c1 (char->integer (caar t)))
        (loop (cdr t) c1))))
  (values byte-vec cumu-vec))

(define (random-fasta header N table)
  (define out (current-output-port))
  (define-values (lookup-byte lookup-cumu) (make-lookup-vectors table))
  (define (n-randoms to)
    (let loop ([n 0])
      (when (fx< n to)
        (let* ([i (random-next)]
               [i (if (fl< (fx->fl i) (flvector-ref lookup-cumu i))
                    (fx- i 1) i)]
               [b (bytes-ref lookup-byte i)])
          (bytes-set! buf n b)
          (loop (fx+ n 1)))))
    (write-bytes buf out 0 (fx+ to 1)))
  (define buf (make-bytes (add1 line-length)))
  (define LF (char->integer #\newline))
  (bytes-set! buf line-length LF)
  (display header out)
  (for ([n (in-range (quotient N line-length))]) (n-randoms line-length))
  (let ([n (remainder N line-length)])
    (unless (zero? n) (bytes-set! buf n LF) (n-randoms n)))
  (void))

;; ----------------------------------------

(let ([n (command-line #:args (n) (string->number n))])
  (repeat-fasta ">ONE Homo sapiens alu\n" (* n 2) +alu+)
  (random-fasta ">TWO IUB ambiguity codes\n" (* n 3) IUB)
  (random-fasta ">THREE Homo sapiens frequency\n" (* n 5) HOMOSAPIEN))
