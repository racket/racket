;; ---------------------------------------------------------------------
;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla
 
#lang scheme/base
(require scheme/cmdline
         scheme/unsafe/ops)

(define +limit-sqr+ 4.0)

(define +iterations+ 50)

;; -------------------------------

(define (mandelbrot x y n ci)
  (let ((cr (unsafe-fl- (unsafe-fl/ (unsafe-fl* 2.0 (unsafe-fx->fl x)) (unsafe-fx->fl n)) 1.5)))
    (let loop ((i 0) (zr 0.0) (zi 0.0))
      (if (unsafe-fx> i +iterations+)
          1
          (cond
           ((unsafe-fl> (unsafe-fl+ (unsafe-fl* zr zr) (unsafe-fl* zi zi)) +limit-sqr+) 0)
           (else (loop (unsafe-fx+ 1 i) 
                       (unsafe-fl+ (unsafe-fl- (unsafe-fl* zr zr) (unsafe-fl* zi zi)) cr) 
                       (unsafe-fl+ (unsafe-fl* 2.0 (unsafe-fl* zr zi)) ci))))))))

;; -------------------------------

(define (main n)
  (let ((out (current-output-port)))

    (fprintf out "P4\n~a ~a\n" n n)

    (let loop-y ((y 0))

      (when (unsafe-fx< y n)
        
        (let ([ci (unsafe-fl- (unsafe-fl/ (unsafe-fl* 2.0 (unsafe-fx->fl y)) (unsafe-fx->fl n)) 1.0)])
          
          (let loop-x ((x 0) (bitnum 0) (byteacc 0))

            (if (unsafe-fx< x n)
                (let ([bitnum (unsafe-fx+ 1 bitnum)]
                      [byteacc (unsafe-fx+ (unsafe-fxlshift byteacc 1) 
                                           (mandelbrot x y n ci))])

                  (cond
                   ((unsafe-fx= bitnum 8)
                    (write-byte byteacc out)
                    (loop-x (unsafe-fx+ 1 x) 0 0))
                   
                   [else (loop-x (unsafe-fx+ 1 x) bitnum byteacc)]))

                (begin
                  (when (positive? bitnum)
                    (write-byte (arithmetic-shift byteacc 
                                                  (- 8 (bitwise-and n #x7))) 
                                out))

                  (loop-y (add1 y))))))))))

;; -------------------------------

(command-line #:args (n)
              (main (string->number n)))
