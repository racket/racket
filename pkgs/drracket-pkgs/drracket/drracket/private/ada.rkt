#lang racket/base
(require pict
         racket/gui/base 
         racket/class
         racket/list
         racket/runtime-path
         (for-syntax racket/base))
(provide ada-size draw-splash-ada)

(module+ test (require rackunit))

(define-runtime-path ada.png (if (getenv "PLTDRBREAKIMAGES")
                                 "ada-broken.png"
                                 "ada.png"))

#|
code for Bernoulli numbers taken from Wikipedia: 
  http://en.wikipedia.org/wiki/Bernoulli_number#Algorithmic_description
|#

(define-syntax-rule
  (for-from-by-to-do var start step end body ...)
  (for ([var (in-range start (+ end step) step)])
    body ...))

(define-syntax-rule 
  (in-range/including-end start end step)
  (in-range start (+ end step) step))

(define (bernoulli-number n)
  (define A (make-vector (+ n 1)))
  (for ([m (in-range/including-end 0 n 1)])
   (vector-set! A m (/ 1 (+ m 1)))
    (for ([j (in-range/including-end m 1 -1)])
      (vector-set! A
                   (- j 1) 
                   (* j (- (vector-ref A (- j 1))
                           (vector-ref A j))))))
  (vector-ref A 0))

(module+ test
  (check-equal? 
   (build-list 9 bernoulli-number)
   '(1 1/2 1/6 0 -1/30 0 1/42 0 -1/30)))

#|

Code for making a pict out of the
bernoulli sequence

|#

(define (make-bernoulli-sequence-pict n)
  (apply hc-append
         (add-between
          (for/list ([x (in-range n)])
            (number->pict (bernoulli-number x)))
          (tt ",  "))))

(define (tt str)
  (text str 'roman 24))

(define (number->pict num)
  (cond
    [(integer? num) (tt (integer->string num))]
    [else
     (define n (tt (integer->string (abs (numerator num)))))
     (define d (tt (integer->string (denominator num))))
     (define line (frame (blank (max (pict-width d) (pict-width n)) 0)))
     (define abs-frac (vc-append 2 n line d))
     (cond
       [(negative? num)
        (hc-append 4 (tt "-") abs-frac)]
       [else
        abs-frac])]))
       
(define (integer->string n)
  (define s (format "~a" n))
  (cond
    [(<= (string-length s) 3) s]
    [else
     (remove-leading-comma
      (reverse-string
       (regexp-replace* #rx"[0-9][0-9][0-9]" 
                        (reverse-string s)
                        "\\0,")))]))

(define (reverse-string s) (list->string (reverse (string->list s))))
(define (remove-leading-comma s) (regexp-replace #rx"^," s ""))

(module+ test
  (check-equal? (integer->string 0) "0")
  (check-equal? (integer->string 1) "1")
  (check-equal? (integer->string 100) "100")
  (check-equal? (integer->string 1000) "1,000")
  (check-equal? (integer->string 1234) "1,234")
  (check-equal? (integer->string 1000000) "1,000,000")
  (check-equal? (integer->string 100000) "100,000"))

#|

Code for sliding a wide pict by

|#

;; 0 <= n <= 1
(define (sliding-sequence n width pict)
  (define bw (pict-width pict))
  (define starting-x (* (- bw width) n))
  (inset/clip pict
              (- starting-x)
              0 
              (+ (- bw) starting-x width)
              0))

#|

Put things together into a function
that actually does drawing in a window.

|#

(define ada-bmp (and (file-exists? ada.png)
                     (read-bitmap ada.png)))
(define ada-w (if ada-bmp (send ada-bmp get-width) 300))
(define ada-h (if ada-bmp (send ada-bmp get-height) 300))
(define ada-size (+ 32 (max ada-w ada-h)))

(define (draw-ada dc point-in-time cw ch [sequence-on-top? #f])
  (define p (sliding-sequence 
             point-in-time
             cw
             bernoulli-sequence-pict))
  (draw-pict p dc 0 0)
  (when ada-bmp
    (send dc draw-bitmap 
          ada-bmp
          (- (/ cw 2) (/ ada-w 2))
          (- (/ ch 2) (/ ada-h 2)))))

(define bernoulli-sequence-pict (make-bernoulli-sequence-pict 50))
(define (draw-splash-ada dc current max width height)
  (send dc clear)
  (draw-ada dc (/ current max) width height) #t)

(module+ main
  
  (define bmp (read-bitmap ada.png))
  (define point-in-time 0)
  (define δ #e0.0002)
  
  (define (draw c dc)
    (define-values (cw ch) (send c get-client-size))
    (draw-ada dc point-in-time cw ch))
  
  (void (new timer%
             [notify-callback
              (λ () 
                (set! point-in-time (+ point-in-time δ))
                (send c refresh))]
             [interval 50]))
  
  
  
  (define f (new frame% [label ""]))
  (define c (new canvas% 
                 [parent f] 
                 [paint-callback draw]
                 [min-width ada-size]
                 [min-height ada-size]))
  (send f show #t))
