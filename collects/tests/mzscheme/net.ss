
(load-relative "loadtest.ss")

(require (lib "base64.ss" "net")
	 (lib "qp.ss" "net")
	 (lib "thread.ss"))

(define tricky-strings
  (list (make-bytes 200 32)
	(make-bytes 200 9)
	(make-bytes 200 (char->integer #\x))
	(list->bytes
	 (let loop ([i 0])
	   (if (= i 256)
	       null
	       (cons i (loop (add1 i))))))
	;; Something that doesn't end with a LF:
	(bytes-append 
	 (with-input-from-file "net.ss" (lambda () (read-bytes 500)))
	 #"xxx")
	;; CRLF:
	(regexp-replace
	 #rx#"\n"
	 (with-input-from-file "net.ss" (lambda () (read-bytes 500)))
	 #"\r\n")))

(define (check-same encode decode port line-rx max-w)
  (let ([p (open-output-bytes)])
    (copy-port port p)
    (let ([bytes (get-output-bytes p)]
	  [r (open-output-bytes)])
      (encode (open-input-bytes bytes) r)
      (let ([p (open-input-bytes (get-output-bytes r))])
	(let loop ()
	  (let ([l (read-bytes-line p 'any)])
	    (unless (eof-object? l)
	      (unless (<= (bytes-length l) max-w)
		(test encode "line too long" l))
	      (let ([m (regexp-match-positions line-rx l)])
		(unless (and m
			     (= (bytes-length l) (cdar m)))
		  (test encode 'bad-line l)))
	      (loop))))
	(let ([q (open-output-bytes)])
	  (decode (open-input-bytes (get-output-bytes r)) q)
	  (unless (equal? (get-output-bytes q) bytes)
	    (with-output-to-file "/tmp/x0" (lambda () (display (get-output-bytes r))) 'truncate)
	    (with-output-to-file "/tmp/x1" (lambda () (display (get-output-bytes q))) 'truncate)
	    (with-output-to-file "/tmp/x2" (lambda () (display bytes)) 'truncate)
	    (error 'decode "failed")))))))

(define (check-same-file encode decode file line-rx max-w)
  ;; This "test" is really just a progress report:
  (test #t list? (list file encode))
  (call-with-input-file file
    (lambda (p)
      (check-same encode decode p line-rx max-w))))

(define (check-same-all encode decode line-rx max-w)
  (for-each (lambda (tricky-string)
	      (check-same encode decode 
			  (open-input-bytes tricky-string)
			  line-rx max-w))
	    tricky-strings)
  (let ([dir (collection-path "tests" "mzscheme")])
    (for-each (lambda (p)
		(when (regexp-match #rx"[.]ss$" (path->string p))
		  (unless (equal? (path->string p) "flat.ss")
		    (check-same-file encode decode (build-path dir p) line-rx max-w))))
	      (directory-list dir))))

(check-same-all (lambda (i o) (qp-encode-stream i o))
		qp-decode-stream
		#rx#"^(|[\t \41-\176]*[\41-\176]+)$"
		76)

(check-same-all base64-encode-stream 
		base64-decode-stream
		#rx#"^[0-9a-zA-Z+=/]*$"
		72)

(report-errs)
