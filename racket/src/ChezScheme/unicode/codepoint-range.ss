(module ((extract-range))

  (define (extract-range str)
    (define (find-char c s)
      (let f ([i 0] [n (string-length s)])
	(cond
	  [(= i n) #f]
	  [(char=? (string-ref s i) c) i]
	  [else (f (+ i 1) n)])))
    (cond
      [(find-char #\. str) =>
       (lambda (i)
	 (cons
	   (hex->num (substring str 0 i))
	   (hex->num (substring str (+ i 2) (string-length str)))))]
      [else (let ([n (hex->num str)]) (cons n n))])))
