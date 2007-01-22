
(module reversecomplement mzscheme 

  (define translation (make-vector 128))

  (for-each (lambda (from-to)
	      (let ([char (lambda (sym)
			    (string-ref (symbol->string sym) 0))])
		(let ([from (char (car from-to))]
		      [to (char->integer (char-upcase (char (cadr from-to))))])
		  (vector-set! translation (char->integer from) to)
		  (vector-set! translation (char->integer (char-upcase from)) to))))
	    '([a t]
	      [c g]
	      [g c]
	      [t a]
	      [u a]
	      [m k]
	      [r y]
	      [w w]
	      [s s]
	      [y R]
	      [k M]
	      [v b]
	      [h d]
	      [d h]
	      [b v]
	      [n n]))

  (define (output lines)
    (let* ([str (apply bytes-append lines)]
	   [o (current-output-port)]
	   [len (bytes-length str)])
      (let loop ([offset 0])
	(when (< offset len)
	  (write-bytes str o offset (min len (+ offset 60)))
	  (newline o)
	  (loop (+ offset 60))))))
  
  (let ([in (current-input-port)])
    (let loop ([accum null])
      (let ([l (read-bytes-line in)])
	(if (eof-object? l)
	    (output accum)
	    (cond
	     [(regexp-match? #rx#"^>" l)
	      (output accum)
	      (printf "~a\n" l)
	      (loop null)]
	     [else
	      (let* ([len (bytes-length l)]
		     [dest (make-bytes len)])
		(let loop ([i 0][j (- len 1)])
		  (unless (= i len)
		    (bytes-set! dest
				j
				(vector-ref translation (bytes-ref l i)))
		    (loop (add1 i) (sub1 j))))
		(loop (cons dest accum)))]))))))

