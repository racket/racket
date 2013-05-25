(require mzlib/defmacro)

(define-macro (nest n expr)
  (if (> n 0)
      `(let loop ([i 1]) (unless (> i n)
                           (nest ,(- n 1) ,expr)
                           (loop (add1 i))))
      expr))


(: main ((Vectorof String) -> Void))
(define (main argv)
  (let*: ([n : Integer (assert (string->number (vector-ref argv 0)) exact-integer?)]
          [x : Integer 0])
    (nest 6 (set! x (+ x 1)))
    (printf "~s\n" x)))

(main (current-command-line-arguments))
