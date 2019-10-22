;; Racket-style outtput for timing informatiton:
(define-syntax time
  (syntax-rules ()
    [(_ e)
     (let ([pre (statistics)])
       (let ([r e])
         (let ([post (statistics)])
           (print-delta pre post)
           r)))]))

(define (print-delta pre post)
  (define (msecs a b)
    (quotient (- (+ (* 1000000000 (time-second a)) (time-nanosecond a))
                 (+ (* 1000000000 (time-second b)) (time-nanosecond b)))
              1000000))
  (printf "cpu time: ~a real time: ~a gc time: ~a\n"
          (msecs (sstats-cpu post) (sstats-cpu pre))
          (msecs (sstats-real post) (sstats-real pre))
          (msecs (sstats-gc-cpu post) (sstats-gc-cpu pre))))

(print-extended-identifiers #t)

(define (show v)
  (when (symbol? v) (display "'"))
  (write v)
  (newline))
