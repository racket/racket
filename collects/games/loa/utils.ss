(unit/sig loa:utils^
  (import)

  (define (vector-for-each v f)
    (let loop ([n (vector-length v)])
      (unless (zero? n)
	(f (vector-ref v (- n 1)))
	(loop (- n 1))))))