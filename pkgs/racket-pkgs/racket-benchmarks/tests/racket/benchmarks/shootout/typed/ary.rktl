(: main ((Vectorof String) -> Void))
(define (main args)
  (let*: ((n : Exact-Positive-Integer
             (if (= (vector-length args) 0)
                 1
                 (assert (string->number (vector-ref args 0)) exact-positive-integer?)))
          (x : (Vectorof Integer) (make-vector n 0))
          (y : (Vectorof Integer) (make-vector n 0))
          (last : Natural (- n 1)))
    (do ((i 0 (+ i 1)))
        ((= i n))
      (vector-set! x i (+ i 1)))
    (do ((k 0 (+ k 1)))
        ((= k 1000))
      (do: : Void ((i : Integer last (- i 1)))
          ((< i 0))
        (vector-set! y i (+ (vector-ref x i) (vector-ref y i)))))
    (print-list (vector-ref y 0) " " (vector-ref y last))))

(: print-list (Any * -> Void))
(define (print-list . items) (for-each display items) (newline))

(main (current-command-line-arguments))
