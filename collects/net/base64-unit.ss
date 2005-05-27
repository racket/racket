

(module base64-unit mzscheme
  (require (lib "unitsig.ss"))

  (require "base64-sig.ss")

  (provide net:base64@)
  (define net:base64@
    (unit/sig net:base64^
      (import)

      (define base64-digit (make-vector 256))
      (let loop ([n 0])
	(unless (= n 256)
	  (cond
	   [(<= (char->integer #\A) n (char->integer #\Z)) 
	    (vector-set! base64-digit n (- n (char->integer #\A)))]
	   [(<= (char->integer #\a) n (char->integer #\z)) 
	    (vector-set! base64-digit n (+ 26 (- n (char->integer #\a))))]
	   [(<= (char->integer #\0) n (char->integer #\9)) 
	    (vector-set! base64-digit n (+ 52 (- n (char->integer #\0))))]
	   [(= (char->integer #\+) n)
	    (vector-set! base64-digit n 62)]
	   [(= (char->integer #\/) n)
	    (vector-set! base64-digit n 63)]
	   [else
	    (vector-set! base64-digit n #f)])
	  (loop (add1 n))))

      (define digit-base64 (make-vector 64))
      (let ([each-char (lambda (s e pos)
			 (let loop ([i (char->integer s)][pos pos])
			   (unless (> i (char->integer e))
			     (vector-set! digit-base64 pos i)
			     (loop (add1 i) (add1 pos)))))])
	(each-char #\A #\Z 0)
	(each-char #\a #\z 26)
	(each-char #\0 #\9 52)
	(each-char #\+ #\+ 62)
	(each-char #\/ #\/ 63))

      (define (base64-decode-stream in out)
	(let loop ([waiting 0][waiting-bits 0])
	  (if (>= waiting-bits 8)
	      (begin
		(write-byte (arithmetic-shift waiting (- 8 waiting-bits))
			    out)
		(let ([waiting-bits (- waiting-bits 8)])
		  (loop (bitwise-and waiting (sub1 (arithmetic-shift 1 waiting-bits)))
			waiting-bits)))
	      (let* ([c0 (read-byte in)]
		     [c (if (eof-object? c0) (char->integer #\=) c0)]
		     [v (vector-ref base64-digit c)])
		(cond
		 [v (loop (+ (arithmetic-shift waiting 6) v)
			  (+ waiting-bits 6))]
		 [(eq? c (char->integer #\=)) (void)] ; done
		 [else (loop waiting waiting-bits)])))))


      (define base64-encode-stream
	(case-lambda
	 [(in out) (base64-encode-stream in out #"\n")]
	 [(in out linesep)
	  ;; Process input 3 characters at a time, because 18 bits
	  ;;  is divisible by both 6 and 8, and 72 (the line length)
	  ;;  is divisible by 3.
	  (let ([three (make-bytes 3)]
		[outc (lambda (n)
			(write-byte (vector-ref digit-base64 n) out))]
		[done (lambda (fill)
			(let loop ([fill fill])
			  (unless (zero? fill)
			    (write-byte (char->integer #\=) out)
			    (loop (sub1 fill))))
			(display linesep out))])
	    (let loop ([pos 0])
	      (if (= pos 72)
		  ; Insert newline
		  (begin
		    (display linesep out)
		    (loop 0))
		  ;; Next group of 3
		  (let ([n (read-bytes-avail! three in)])
		    (cond
		     [(eof-object? n)
		      (unless (= pos 0)
			(done 0))]
		     [(= n 3)
		      ;; Easy case:
		      (let ([a (bytes-ref three 0)]
			    [b (bytes-ref three 1)]
			    [c (bytes-ref three 2)])
			(outc (arithmetic-shift a -2))
			(outc (+ (bitwise-and #x3f (arithmetic-shift a 4))
				 (arithmetic-shift b -4)))
			(outc (+ (bitwise-and #x3f (arithmetic-shift b 2))
				 (arithmetic-shift c -6)))
			(outc (bitwise-and #x3f c))
			(loop (+ pos 4)))]
		     [else
		      ;; Hard case: n is 1 or 2
		      (let ([a (bytes-ref three 0)])
			(outc (arithmetic-shift a -2))
			(let* ([next (if (= n 2)
					 (bytes-ref three 1)
					 (read-byte in))]
			       [b (if (eof-object? next)
				      0
				      next)])
			  (outc (+ (bitwise-and #x3f (arithmetic-shift a 4))
				   (arithmetic-shift b -4)))
			  (if (eof-object? next)
			      (done 2)
			      ;; More to go
			      (let* ([next (read-byte in)]
				     [c (if (eof-object? next)
					    0
					    next)])
				(outc (+ (bitwise-and #x3f (arithmetic-shift b 2))
					 (arithmetic-shift c -6)))
				(if (eof-object? next)
				    (done 1)
				    ;; Finish c, loop
				    (begin
				      (outc (bitwise-and #x3f c))
				      (loop (+ pos 4))))))))])))))]))

      (define (base64-decode src)
	(let ([s (open-output-bytes)])
	  (base64-decode-stream (open-input-bytes src) s)
	  (get-output-bytes s)))

      (define (base64-encode src)
	(let ([s (open-output-bytes)])
	  (base64-encode-stream (open-input-bytes src) s
				(bytes 13 10))
	  (get-output-bytes s))))))

