(: main ((Vectorof String) -> Void))
(define (main args)
  (let: ((n : Integer
            (if (= (vector-length args) 0)
                1
                (assert (string->number (vector-ref args 0)) exact-integer?)))
         (count : Integer 0)
         (flags : (Vectorof Boolean) (make-vector 8192 #t)))
    (let loop ((iter n))
      (if (> iter 0)
          (begin
            (do ((i 0 (+ i 1))) ((>= i 8192)) (vector-set! flags i #t))
            (set! count 0)
            (do ((i 2 (+ 1 i)))
                ((>= i 8192))
              (if (vector-ref flags i)
                  (begin
                    (do ((k (+ i i) (+ k i)))
                        ((>= k 8192))
                      (vector-set! flags k #f))
                    (set! count (+ 1 count)))
                  #t))
            (loop (- iter 1)))
          #t))
    (display "Count: ") (display count) (newline)))

(main (current-command-line-arguments))
