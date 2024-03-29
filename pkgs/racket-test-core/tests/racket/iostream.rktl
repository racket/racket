
(printf "Stream Tests (current dir must be startup dir)\n")

(require racket/system)

(define (log . args)
  '(begin
     (apply printf args)
     (newline)))

(define cs-prog
  '(define (copy-stream in out)
     (lambda ()
       (let ([s (make-bytes 4096)])
	 (let loop ()
	   (let ([l (read-bytes-avail! s in)])
	     (log "in: ~a" l)
	     (unless (eof-object? l)
	       (let loop ([p 0][l l])
		 (let ([r (write-bytes-avail s out p (+ p l))])
		   (log "out: ~a" r)
		   (when (< r l)
		     (loop (+ p r) (- l r)))))
	       (loop))))))))

(eval cs-prog)

(define test-file (find-executable-path (find-system-path 'exec-file) #f))
(define tmp-file (build-path (find-system-path 'temp-dir) "ZstreamZ"))

(define (feed-file out)
  (let ([p (open-input-file test-file)])
    (let loop ()
      (let ([c (read-byte p)])
	(unless (eof-object? c)
	  (write-byte c out)
	  (loop))))))
	
(define (feed-file/fast out)
  (let ([p (open-input-file test-file)])
    ((copy-stream p out))
    (close-input-port p)))
	
(define (check-file in)
  (let ([p (open-input-file test-file)])
    (let loop ([badc 0])
      (let ([c (read-byte p)]
	    [c2 (read-byte in)])
	(unless (eq? c c2)
	  (if (= badc 30)
	      (error "check-failed" (file-position p) c c2)
	      (begin
		(fprintf (current-error-port)
			 "fail: ~a ~s=~s ~s=~s\n"
			 (file-position p) c (integer->char c) c2 (integer->char c2))
		(loop (add1 badc)))))
	(unless (eof-object? c)
	  (loop badc))))
    (close-input-port p)))

(define (check-file/fast in)
  (let ([p (open-input-file test-file)])
    (let loop ()
      (let* ([s (read-bytes 5000 p)]
	     [s2 (read-bytes (if (bytes? s) (bytes-length s) 100) in)])
	(unless (equal? s s2)
	  (error 'check-file/fast "failed: ~s vs. ~s" s s2))
	(unless (eof-object? s)
	  (loop))))
    (close-input-port p)))

(define (check-file/fastest in)
  (let ([p (open-input-file test-file)]
	[s1 (make-bytes 5000)]
	[s2 (make-bytes 5000)])
    (let loop ([leftover 0][startpos 0][pos 0])
      (let* ([n1 (if (zero? leftover)
		     (read-bytes-avail! s1 p)
		     leftover)]
	     [n2 (read-bytes-avail! s2 in 0 (if (eof-object? n1)
                                                1
						 n1))])
	(unless (if (or (eof-object? n1)
			(eof-object? n2))
		    (and (eof-object? n1)
			 (eof-object? n2))
		    (if (= n2 n1 5000)
			(bytes=? s1 s2)
			(bytes=? (subbytes s1 startpos (+ startpos n2))
				  (subbytes s2 0 n2))))
	  (error 'check "failed at ~a (~a@~a ~a)" pos n1 startpos n2))
	(unless (eof-object? n1)
	  (loop (- n1 n2) 
		(if (= n1 n2)
		    0
		    (+ startpos n2))
		(+ pos n2)))))
    (close-input-port p)))

(define portno 40010)

(define (setup-mzscheme-echo tcp?)
  (define p (process* test-file "-q" "-b"))
  (define s (make-bytes 256))
  (define r #f)
  (define w #f)
  (define r2 #f)
  (define w2 #f)
  (thread (copy-stream (cadddr p) (current-error-port)))
  (fprintf (cadr p) "(define log void)\n")
  (fprintf (cadr p) "~s\n" cs-prog)
  (if tcp?
      (let ([t 
	     (thread (lambda ()
		       (define-values (rr ww) (tcp-accept l1))
		       (define-values (rr2 ww2) (tcp-accept l2))
		       (set! r rr)
		       (set! w ww)
		       (set! r2 rr2)
		       (set! w2 ww2)))])
	(fprintf (cadr p) "(define-values (r w) (tcp-connect \"localhost\" ~a))\n" portno)
	(fprintf (cadr p) "(define-values (r2 w2) (tcp-connect \"localhost\" ~a))\n" (add1 portno))
        (flush-output (cadr p))
	(thread-wait t)
	(fprintf (cadr p) "(begin ((copy-stream r w2)) (exit))\n"))
      (fprintf (cadr p) "(begin (display \"!READY!\") (flush-output) ((copy-stream (current-input-port) (current-output-port))) (exit))"))
  (flush-output (cadr p))

  (unless tcp?
    ;; Flush initial output from other process:
    (regexp-match #rx"!READY!" (car p)))

  (if tcp?
      (values r w r2 w2)
      p))

(define start-ms 0)
(define start-ps-ms 0)
(define start-gc-ms 0)
(define (start s)
  (printf s)
  (set! start-ms (current-milliseconds))
  (set! start-gc-ms (current-gc-milliseconds))
  (set! start-ps-ms (current-process-milliseconds)))
(define (end)
  (let ([ps-ms (current-process-milliseconds)]
	[gc-ms (current-gc-milliseconds)]
	[ms (current-milliseconds)])
    (printf "cpu: ~a real: ~a gc ~a\n" 
	    (- ps-ms start-ps-ms)
	    (- ms start-ms)
	    (- gc-ms start-gc-ms))))

'(thread (lambda ()
	  (let loop ()
	    (printf "alive\n")
	    (sleep 1)
	    (loop))))

(start "Quick check:\n")
(define p (open-input-file test-file))
(check-file/fast p)
(close-input-port p)
(end)

(start "Quicker check:\n")
(define p (open-input-file test-file))
(check-file/fastest p)
(close-input-port p)
(end)

(start "Plain pipe...\n")
(define-values (r w) (make-pipe))
(feed-file w)
(close-output-port w)
(check-file r)
(end)

(start "Plain pipe, faster...\n")
(define-values (r w) (make-pipe))
(feed-file/fast w)
(close-output-port w)
(check-file/fast r)
(end)

(start "Plain pipe, fastest...\n")
(define-values (r w) (make-pipe))
(feed-file/fast w)
(close-output-port w)
(check-file/fastest r)
(end)

(start "Limited pipe...\n")
(define-values (r w) (make-pipe 253))
(thread (lambda ()
	  (feed-file w)
	  (close-output-port w)))
(check-file r)
(end)

(start "Limited pipe, faster...\n")
(define-values (r w) (make-pipe 253))
(thread (lambda ()
	  (feed-file/fast w)
	  (close-output-port w)))
(check-file/fast r)
(end)

(start "Limited pipe, fastest...\n")
(define-values (r w) (make-pipe 253))
(thread (lambda ()
	  (feed-file/fast w)
	  (close-output-port w)))
(check-file/fastest r)
(end)

(start "To file and back:\n")
(start " to...\n")
(define-values (r w) (make-pipe))
(define p (open-output-file tmp-file #:exists 'truncate))
(define t (thread (copy-stream r p)))
(feed-file w)
(close-output-port w)
(thread-wait t)
(close-output-port p)
(end)

(start " back...\n")
(define-values (r w) (make-pipe))
(define p (open-input-file tmp-file))
(define t (thread (copy-stream p w)))
(thread-wait t)
(close-output-port w)
(close-input-port p)
(check-file r)
(end)

(start "To file and back, faster:\n")
(start " to...\n")
(define-values (r w) (make-pipe))
(define p (open-output-file tmp-file #:exists 'truncate))
(define t (thread (copy-stream r p)))
(feed-file/fast w)
(close-output-port w)
(thread-wait t)
(close-output-port p)
(end)

(start " back...\n")
(define-values (r w) (make-pipe))
(define p (open-input-file tmp-file))
(define t (thread (copy-stream p w)))
(thread-wait t)
(close-output-port w)
(close-input-port p)
(check-file/fast r)
(end)

(start "File back, fastest:\n")
(define-values (r w) (make-pipe))
(define p (open-input-file tmp-file))
(define t (thread (copy-stream p w)))
(thread-wait t)
(close-output-port w)
(close-input-port p)
(check-file/fastest r)
(end)

(start "Echo...\n")
(define p (setup-mzscheme-echo #f))
(thread (lambda () 
	  (feed-file (cadr p))
	  (close-output-port (cadr p))))
(check-file (car p))
(end)

(start "Echo, faster...\n")
(define p (setup-mzscheme-echo #f))
(thread (lambda () 
	  (feed-file/fast (cadr p))
	  (close-output-port (cadr p))))
(check-file/fast (car p))
(end)

(start "Echo, indirect...\n")
(define p (setup-mzscheme-echo #f))
(define-values (rp1 wp1) (make-pipe))
(define-values (rp2 wp2) (make-pipe))
(thread (lambda () ((copy-stream rp1 (cadr p))) (close-output-port (cadr p))))
(thread (lambda () ((copy-stream (car p) wp2)) (close-output-port wp2)))
(thread (lambda () 
	  (feed-file/fast wp1)
	  (close-output-port wp1)))
(check-file/fast rp2)
(end)

(define l1 (tcp-listen portno 5 #t))
(define l2 (tcp-listen (add1 portno) 5 #t))

(start "TCP Echo...\n")
(define-values (r w r2 w2) (setup-mzscheme-echo #t))
(close-input-port r)
(thread (lambda () 
	  (feed-file w)
	  (close-output-port w)))
(check-file r2)
(close-input-port r2)
(end)

;; ----------------------------------------
;; TCP listeners as events

(chaperone-evt (tcp-listen 0)
               (lambda (e) (values e values)))


(start "TCP Echo, faster...\n")
(define-values (r w r2 w2) (setup-mzscheme-echo #t))
(close-input-port r)
(thread (lambda () 
	  (feed-file/fast w)
	  (close-output-port w)))
(check-file/fast r2)
(close-input-port r2)
(end)

(start "TCP Echo, indirect...\n")
(define-values (rp1 wp1) (make-pipe))
(define-values (rp2 wp2) (make-pipe))
(define-values (r w r2 w2) (setup-mzscheme-echo #t))
(close-input-port r)
(thread (lambda () ((copy-stream rp1 w)) (close-output-port w)))
(thread (lambda () ((copy-stream r2 wp2)) (close-output-port wp2)))
(thread (lambda () 
	  (feed-file/fast wp1)
	  (close-output-port wp1)))
(check-file/fast rp2)
(end)

(tcp-close l1)
(tcp-close l2)
