
(load-relative "loadtest.ss")

(when (file-exists? "tmp-pipe")
  (delete-file "tmp-pipe"))

(require (lib "process.ss"))
(system "mknod tmp-pipe p")

(define i1 (open-input-file "tmp-pipe"))
(define i2 (open-input-file "tmp-pipe"))
(define o (open-output-file "tmp-pipe" 'append))

(let ([t (thread
	  (lambda ()
	    (with-handlers ([exn:break? void])
	      (parameterize-break #f
		(read-bytes-avail!/enable-break (make-bytes 2) i1)))))])
  (sleep 0.1)
  (break-thread t)
  (sleep 0.1)
  (test #f thread-running? t))

;; Reading from i1 will pull in all ready chars
(define (test-buffered i1 i2)
  (test #"he" read-bytes 2 i1)
  (test #f char-ready? i2)
  (test #"ll" read-bytes 2 i1)
  (test #"o\n" read-bytes 2 i1)
  (test #f char-ready? i1)
  (test #f char-ready? i2))

(display "hello\n" o)
(test-buffered i1 i2)
(display "hello\n" o)
(test-buffered i2 i1)

(close-output-port o)
(test eof read-byte i1)
(test eof read-byte i2)
(close-input-port i1)
(close-input-port i2)

(define i1 (open-input-file "tmp-pipe"))
(define i2 (open-input-file "tmp-pipe"))
(define o (open-output-file "tmp-pipe" 'append))

(test 'block file-stream-buffer-mode i1)
(test 'block file-stream-buffer-mode i2)
(test (void) file-stream-buffer-mode i1 'none)
(test (void) file-stream-buffer-mode i2 'none)

(define (test-unbuffered i1 i2)
  (let ([s (make-bytes 10)])
    (test 1 peek-bytes-avail! s 0 #f i1)
    (test 1 peek-bytes-avail! s 0 #f i1))
  (test #"he" read-bytes 2 i1)
  (test #t char-ready? i2)
  (test #"ll" read-bytes 2 i2)
  (test #"o\n" read-bytes 2 i1)
  (test #f char-ready? i1)
  (test #f char-ready? i2))

(display "hello\n" o)
(test-unbuffered i1 i2)
(display "hello\n" o)
(test-unbuffered i2 i1)


(report-errs)
