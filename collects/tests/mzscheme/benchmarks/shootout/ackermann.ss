(module ackermann mzscheme
  (define (ack m n)
    (cond ((zero? m) (+ n 1))
	  ((zero? n) (ack (- m 1) 1))
	  (else      (ack (- m 1) (ack m (- n 1))))))

  (define (main args)
    (let ((n (if (= (vector-length args) 0)
		 1
		 (string->number (vector-ref args 0)))))
      (printf "Ack(3,~a): ~a~n" n (ack 3 n))))

  (main (current-command-line-arguments)))
