;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla
;;
;; This version uses unsafe operations

(require racket/cmdline
	 racket/require (for-syntax racket/base)
	 (filtered-in
	  (lambda (name) (regexp-replace #rx"unsafe-" name ""))
	  racket/unsafe/ops))

(define +limit-sqr+ 4.0)

(define +iterations+ 50)

;; -------------------------------

(: mandelbrot (Integer Integer Integer Float -> (U 0 1)))
(define (mandelbrot x y n ci)
  (let ((cr (fl- (fl/ (fl* 2.0 (fx->fl x)) (fx->fl n)) 1.5)))
    (let loop ((i 0) (zr 0.0) (zi 0.0))
      (if (fx> i +iterations+)
          1
          (cond
           ((fl> (fl+ (fl* zr zr) (fl* zi zi)) +limit-sqr+) 0)
           (else (loop (fx+ 1 i) 
                       (fl+ (fl- (fl* zr zr) (fl* zi zi)) cr) 
                       (fl+ (fl* 2.0 (fl* zr zi)) ci))))))))

;; -------------------------------

(: main (Integer -> Void))
(define (main n)
  (let ((out (current-output-port)))

    (fprintf out "P4\n~a ~a\n" n n)

    (let loop-y ((y 0))

      (when (fx< y n)
        
        (let ([ci (fl- (fl/ (fl* 2.0 (fx->fl y)) (fx->fl n)) 1.0)])
          
          (let: loop-x : Void ((x : Integer 0) (bitnum : Integer 0) (byteacc : Integer 0))

            (if (fx< x n)
                (let ([bitnum (fx+ 1 bitnum)]
                      [byteacc (fx+ (fxlshift byteacc 1) 
                                           (mandelbrot x y n ci))])

                  (cond
                   ((fx= bitnum 8)
                    (write-byte byteacc out)
                    (loop-x (fx+ 1 x) 0 0))
                   
                   [else (loop-x (fx+ 1 x) bitnum byteacc)]))

                (begin
                  (when (positive? bitnum)
                    (write-byte (arithmetic-shift byteacc 
                                                  (- 8 (bitwise-and n #x7))) 
                                out))

                  (loop-y (add1 y))))))))))

;; -------------------------------

(command-line #:args (n)
              (main (assert (string->number (assert n string?)) exact-integer?)))
