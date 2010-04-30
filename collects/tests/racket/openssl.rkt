
(load-relative "loadtest.rkt")

(Section 'openssl)

(require openssl/mzssl)

(define pem (build-path (collection-path "openssl")
			"test.pem"))

(define (test-ssl limit buffer? close?)
  ;; Test SSL communication using a limited pipe.
  ;; (Using a pipe limited to a small buffer helps make sure
  ;; that race conditions and deadlocks are eliminated in the
  ;; implementation.)
  (let-values ([(r1 w2) (make-pipe limit)]
	       [(r2 w1) (make-pipe limit)])
    (define t1
      (thread (lambda ()
		(let-values ([(r w) (ports->ssl-ports
				     r1 w1
				     #:mode 'connect 
				     #:close-original? close? 
				     #:shutdown-on-close? #t)])
		  (unless buffer?
		    (file-stream-buffer-mode w 'none))
		  (test 5 write-bytes #"abcde" w)
		  (when buffer?
		    (flush-output w))
		  (test "hello" read-string 5 r)
		  (test eof read-string 5 r)
		  (close-input-port r)
		  (close-output-port w)))))
    (define t2
      (thread (lambda ()
		(define ctx (ssl-make-server-context 'sslv2-or-v3))
		(ssl-load-certificate-chain! ctx pem)
		(ssl-load-private-key! ctx pem)
		(let-values ([(r w) (ports->ssl-ports
				     r2 w2
				     #:context ctx
				     #:mode 'accept 
				     #:close-original? close? 
				     #:shutdown-on-close? #t)])
		  (test #"abcde" read-bytes 5 r)
		  (test 5 write-string "hello" w)
		  (close-output-port w)))))
    (thread-wait t1)
    (thread-wait t2)
    ;; Check that ports were closed not:
    (if close?
	(begin
	  (err/rt-test (read-byte r1) exn:fail?)
	  (err/rt-test (write-byte 0 w1) exn:fail?))
	(let ([v (random 256)]
	      [v2 (random 256)])
	  (test (void) write-byte v w2)
	  (test v read-byte r1)
	  (test (void) write-byte v2 w1)
	  (test v2 read-byte r2)))
    (void)))

(test-ssl 1 #t #t)
(test-ssl 10 #t #t)
(test-ssl 100 #t #t)
(test-ssl 1 #f #t)
(test-ssl 10 #f #t)
(test-ssl 100 #f #t)

(test-ssl 100 #t #f)
(test-ssl 100 #f #f)


(report-errs)
