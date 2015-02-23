;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/

(require racket/require racket/require-syntax (for-syntax racket/base))
(define-require-syntax overriding-in
  (syntax-rules () [(_ R1 R2) (combine-in R2 (subtract-in R1 R2))]))
(require (overriding-in
          racket/flonum
          (filtered-in (lambda (name) (regexp-replace #rx"unsafe-" name ""))
                       racket/unsafe/ops))
         racket/cmdline)

(define O (current-output-port))

(define LIMIT-SQR 4.0)
(define ITERATIONS 50)
(define N (command-line #:args (n) (assert (string->number (assert n string?)) exact-integer?)))
(define N.0 (fx->fl N))
(define 2/N (fl/ 2.0 N.0))
(define Crs
  (let ([v (make-flvector N)])
    (for ([x (in-range N)])
      (flvector-set! v x (fl- (fl/ (fx->fl (fx* 2 x)) N.0) 1.5)))
    v))

(define-syntax (let-n stx)
  (syntax-case stx ()
    [(_ N bindings E)
     (let loop ([N (syntax-e #'N)] [E #'E])
       (if (zero? N) E (loop (sub1 N) #`(let bindings #,E))))]))

(define-syntax-rule (mandelbrot Cr Ci)
  (let: loop : Integer ([i : Integer 0] [Zr : Float 0.0] [Zi : Float 0.0])
    (cond [(fl> (fl+ (fl* Zr Zr) (fl* Zi Zi)) LIMIT-SQR) 0]
          [(fx= i ITERATIONS) 1]
          [else (let-n 5 ([Zr (fl+ (fl- (fl* Zr Zr) (fl* Zi Zi)) Cr)]
                          [Zi (fl+ (fl* 2.0 (fl* Zr Zi)) Ci)])
                  (loop (fx+ i 5) Zr Zi))])))

(fprintf O "P4\n~a ~a\n" N N)
(let: loop-y : Void ([y : Integer N])
  (let ([Ci (fl- (fl* 2/N (fx->fl y)) 1.0)])
    (let: loop-x : Void ([x : Integer 0] [bitnum : Integer 0] [byteacc : Integer 0])
      (if (fx< x N)
        (let* ([Cr (flvector-ref Crs x)]
               [bitnum (fx+ bitnum 1)]
               [byteacc (fx+ (fxlshift byteacc 1) (mandelbrot Cr Ci))])
          (cond [(fx= bitnum 8)
                 (write-byte byteacc O)
                 (loop-x (fx+ x 1) 0 0)]
                [else (loop-x (fx+ x 1) bitnum byteacc)]))
        (begin (when (fx> bitnum 0)
                 (write-byte (fxlshift byteacc (fx- 8 (fxand N #x7))) O))
               (when (fx> y 1) (loop-y (fx- y 1))))))))
