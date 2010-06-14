;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla
 
(require racket/cmdline
         racket/flonum)

(define +limit-sqr+ 4.0)

(define +iterations+ 50)

;; -------------------------------

(: mandelbrot (Integer Integer Integer Float -> (U 1 0)))
(define (mandelbrot x y n ci)
  (let ((cr (fl- (fl/ (fl* 2.0 (->fl x)) (->fl n)) 1.5)))
    (let loop ((i 0) (zr 0.0) (zi 0.0))
      (if (> i +iterations+)
          1
          (cond
           ((fl> (fl+ (fl* zr zr) (fl* zi zi)) +limit-sqr+) 0)
           (else (loop (+ 1 i) 
                       (fl+ (fl- (fl* zr zr) (fl* zi zi)) cr) 
                       (fl+ (fl* 2.0 (fl* zr zi)) ci))))))))

;; -------------------------------

(: main (Integer -> Void))
(define (main n)
  (let ((out (current-output-port)))

    (fprintf out "P4\n~a ~a\n" n n)

    (let loop-y ((y 0))

      (when (< y n)
        
        (let ([ci (fl- (fl/ (fl* 2.0 (->fl y)) (->fl n)) 1.0)])
          
          (let: loop-x : Void
                ((x : Natural 0) (bitnum : Natural 0) (byteacc : Natural 0))

            (if (< x n)
                (let: ([bitnum : Natural (+ 1 bitnum)]
                       [byteacc : Natural (+ (arithmetic-shift byteacc 1) 
                                             (mandelbrot x y n ci))])

                  (cond
                   ((= bitnum 8)
                    (write-byte byteacc out)
                    (loop-x (+ 1 x) 0 0))
                   
                   [else (loop-x (+ 1 x) bitnum byteacc)]))

                (begin
                  (when (positive? bitnum)
                    (write-byte (arithmetic-shift byteacc 
                                                  (- 8 (bitwise-and n #x7)))
                                out))

                  (loop-y (add1 y))))))))))

;; -------------------------------

(command-line #:args (n)
              (main (assert (string->number (assert n string?)) exact-integer?)))
