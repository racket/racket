
(load-relative "loadtest.rktl")

(Section 'portlib)

(define SLEEP-TIME 0.1)

(require racket/port)

;; ----------------------------------------

(let* ([p (lambda () (open-input-string "hello\r\nthere"))])
  (test '(hello there) port->list read (p))
  (test '(#\h #\e #\l #\l #\o #\return #\newline #\t #\h #\e #\r #\e)
	port->list read-char (p))
  (test "hello\r\nthere" port->string (p))
  (test #"hello\r\nthere" port->bytes (p))
  (test '("hello" "there") port->lines (p))
  (test '(#"hello" #"there") port->bytes-lines (p))
  (test '("hello\r" "there") port->lines (p) #:line-mode 'linefeed)
  (test '(#"hello\r" #"there") port->bytes-lines (p) #:line-mode 'linefeed)
  (test '("hello" "" "there") port->lines (p) #:line-mode 'any-one)
  (test '(#"hello" #"" #"there") port->bytes-lines (p) #:line-mode 'any-one))

(let* ([x (make-string 50000 #\x)]
       [p (lambda () (open-input-string x))])
  (test (string-length x) 'long-string (string-length (port->string (p))))
  (test (string-length x) 'long-string (bytes-length (port->bytes (p)))))

(let ([p (open-output-bytes)])
  (display-lines '(1 2 3) p)
  (test "1\n2\n3\n" get-output-string p))
(let ([p (open-output-bytes)])
  (display-lines '(1 2 3) p #:separator #"!!")
  (test "1!!2!!3!!" get-output-string p))

;; ----------------------------------------

(let ()
  (define (test-with cw-in cw-out s wrap-in wrap-out)
    (test 'cat cw-in s (wrap-in (lambda (p) (read p))))
    (test s cw-out (wrap-out (lambda (p) (write 'cat p)))))
  (test-with call-with-input-bytes call-with-output-bytes #"cat" values values)
  (test-with call-with-input-string call-with-output-string "cat" values values)
  (let ([wrap-in (lambda (f) (lambda () (f (current-input-port))))]
        [wrap-out (lambda (f) (lambda () (f (current-output-port))))])
    (test-with with-input-from-bytes with-output-to-bytes #"cat" wrap-in wrap-out)
    (test-with with-input-from-string with-output-to-string "cat" wrap-in wrap-out)))

(err/rt-test (call-with-input-bytes "x" values))
(err/rt-test (call-with-input-string #"x" values))
(err/rt-test (with-input-from-bytes "x" values))
(err/rt-test (with-input-from-string #"x" values))
(err/rt-test (call-with-input-bytes #"x" (lambda () 'x)))
(err/rt-test (call-with-input-string "x" (lambda () 'x)))
(err/rt-test (with-input-from-bytes #"x" add1))
(err/rt-test (with-input-from-string "x" add1))
(err/rt-test (call-with-output-bytes (lambda () 'x)))
(err/rt-test (call-with-output-string (lambda () 'x)))
(err/rt-test (with-output-to-bytes add1))
(err/rt-test (with-output-to-string add1))

;; ----------------------------------------

;; pipe and pipe-with-specials commmit tests
(define (test-pipe-commit make-pipe)
  (let-values ([(in out) (make-pipe)])
    (display "apple" out)
    (test #"app" peek-bytes 3 0 in)
    (let ([sema (make-semaphore 1)])
      (test #t port-commit-peeked 3 (port-progress-evt in) sema in)
      (test #f semaphore-try-wait? sema))
    (test #"le" read-bytes 2 in)
    (display "banana" out)
    (test #"ban" peek-bytes 3 0 in)
    ;; Set up a commit that fails, because the done-evt never becomes ready:
    (let* ([r '?]
	   [unless-evt (port-progress-evt in)]
	   [th (thread
		(lambda ()
		  (set! r (port-commit-peeked 3 unless-evt never-evt in))))])
      (sync (system-idle-evt))
      (test #t thread-running? th)
      (test #\b peek-char in)
      (sync (system-idle-evt))
      (test #t thread-running? th)
      (test #f sync/timeout 0 unless-evt)
      (test #\b read-char in)
      (sync (system-idle-evt))
      (test th sync th)
      (test #f values r))
    (test "anana" read-string 5 in)
    ;; Set up two commits, pick one to succeed:
    (let ([go (lambda (which peek? suspend/kill)
		(printf "~a ~a ~a\n" which peek? suspend/kill)
		(display "donut" out)
		(test #"don" peek-bytes 3 0 in)
		(let* ([r1 '?]
		       [r2 '?]
		       [s1 (make-semaphore)]
		       [s2 (make-semaphore)]
		       [unless-evt (port-progress-evt in)]
		       [th1 (thread
			     (lambda ()
			       (set! r1 (port-commit-peeked 1 unless-evt s1 in))))]
		       [_ (sync (system-idle-evt))]
		       [th2 (thread
			     (lambda ()
			       (set! r2 (port-commit-peeked 2 unless-evt (semaphore-peek-evt s2) in))))])
                  (sync (system-idle-evt))
		  (when suspend/kill
		    (case suspend/kill
		      [(suspend) (thread-suspend th1)]
		      [(kill) (kill-thread th1)])
                    (sync (system-idle-evt)))
		  (test (eq? suspend/kill 'kill) thread-dead? th1)
		  (test #f thread-dead? th2)
		  (when peek?
		    (test #"do" peek-bytes 2 0 in)
                    (sync (system-idle-evt)))
		  (unless (= which 3)
		    (semaphore-post (if (= which 1) s1 s2)))
		  (when (= which 3)
		    (test #"do" read-bytes 2 in))
                  (sync (system-idle-evt))
		  (test unless-evt sync/timeout 0 unless-evt)
		  (test (not (eq? suspend/kill 'suspend)) thread-dead? th1)
                  (sync (system-idle-evt))
		  (test #t thread-dead? th2)
		  (test (if (= which 1) #t (if suspend/kill '? #f)) values r1)
		  (test (= which 2) values r2)
		  (test (if (= which 1) #\o #\n) read-char in)
		  (test (if (= which 1) #"nut" #"ut") read-bytes (if (= which 1) 3 2) in)))])
      (go 1 #f #f)
      (go 2 #f #f)
      (go 1 #t #f)
      (go 2 #t #f)
      (go 3 #f #f)
      (go 2 #f 'suspend)
      (go 2 #t 'suspend)
      (go 3 #f 'suspend)
      (go 2 #f 'kill)
      (go 2 #t 'kill)
      (go 3 #f 'kill))))
(test-pipe-commit make-pipe)
(test-pipe-commit (lambda () (make-pipe-with-specials 10000 'special-pipe 'spec-pipe)))

;; pipe-with-specials and limit; also used to test peeked-input-port
(define (test-special-pipe make-pipe-with-specials)
  (let-values ([(in out) (make-pipe-with-specials 10)])
    ;; Check that write events work
    (test 5 sync (write-bytes-avail-evt #"12345" out))
    (test #"12345" read-bytes 5 in)
    (test #f char-ready? in)
    (test #t sync (write-special-evt 'okay out))
    (test 11 write-bytes-avail (make-bytes 11 65) out)
    (test 'okay read-char-or-special in)
    (test (make-bytes 11 65) read-bytes 11 in)

    (let ()
      (define (bg thunk runs? spec? exn?)
	;; Fill the pipe, again:
	(test 10 write-bytes (make-bytes 10 66) out)
        (sync (system-idle-evt))
	(let* ([ex #f]
	       [th (thread 
		    (lambda ()
		      (with-handlers ([exn:fail? (lambda (x) 
						   (set! ex #t)
						   (raise x))])
                        (let ([evt (write-bytes-avail-evt #"x" out)])
                          (sync evt)))))])
          (sync (system-idle-evt))
	  (test #t thread-running? th)
	  ;; This thunk (and sometimes read) should go through the manager:
	  (thunk)
          (sync (system-idle-evt))
	  (test (not runs?) thread-running? th)
	  (test (make-bytes 10 66) read-bytes 10 in)
	  (thread-wait th)
	  (test ex values exn?))
	(when spec?
	  (test 'c read-char-or-special in))
	(test (if exn? eof #"x") read-bytes 1 in))
      
      (bg (lambda () (test 0 write-bytes-avail* #"c" out)) #f #f #f)
      (bg (lambda () (test #t write-special 'c out)) #t #t #f)
      (bg (lambda () (test (void) close-output-port out)) #t #f #t))))
(test-special-pipe make-pipe-with-specials)
(test-special-pipe (lambda (limit)
		     (let-values ([(in out) (make-pipe-with-specials limit)])
		       (values (peeking-input-port in) out))))

;; copy-port and make-pipe-with-specials tests
(let ([s (let loop ([n 10000][l null])
	   (if (zero? n)
	       (apply bytes l)
	       (loop (sub1 n) (cons (random 256) l))))])
  (let-values ([(in out) (make-pipe-with-specials)])
    (display s out)
    (test #t 'pipe-same? (bytes=? s (read-bytes (bytes-length s) in)))
    (test out sync/timeout 0 out)
    (test #f sync/timeout 0 in)
    (write-special 'hello? out)
    (test 'hello? read-char-or-special in)
    (display "123" out)
    (write-special 'again! out)
    (display "45" out)
    (let ([s (make-bytes 5)])
      (test 3 read-bytes-avail! s in)
      (test #"123\0\0" values s)
      (let ([p (read-bytes-avail! s in)])
	(test #t procedure? p)
	(test 'again! p 'ok 1 2 3))
      (test 2 read-bytes-avail! s in)
      (test #"453\0\0" values s)))
  (let ([in (open-input-bytes s)]
	[out (open-output-bytes)])
    (copy-port in out)
    (test #t 'copy-same? (bytes=? s (get-output-bytes out))))
  (let* ([a (subbytes s 0 (max 1 (random (bytes-length s))))]
	 [b (subbytes s (bytes-length a) (+ (bytes-length a)
					    (max 1 (random (- (bytes-length s) (bytes-length a))))))]
	 [c (subbytes s (+ (bytes-length a) (bytes-length b)))])
    (define (go-stream close? copy? threads? peek?)
      (printf "Go stream: ~a ~a ~a ~a\n" close? copy? threads? peek?)
      (let*-values ([(in1 out) (make-pipe-with-specials)]
		    [(in out1) (if copy?
				   (make-pipe-with-specials)
				   (values in1 out))])
	(let ([w-th
	       (lambda ()
		 (display a out)
		 (write-special '(first one) out)
		 (display b out)
		 (write-special '(second one) out)
		 (display c out)
		 (when close?
		   (close-output-port out)))]
	      [c-th (lambda ()
		      (when copy?
			(copy-port in1 out1)
			(close-output-port out1)))]
	      [r-th (lambda ()
		      (let ([get-one-str
			     (lambda (a)
			       (let ([dest (make-bytes (bytes-length s))]
				     [target (bytes-length a)])
				 (let loop ([n 0])
				   (let ([v (read-bytes-avail! dest in n)])
				     (if (= target (+ v n))
					 (test #t `(same? ,target) (equal? (subbytes dest 0 target) a))
					 (loop (+ n v)))))))]
			    [get-one-special
			     (lambda (spec)
			       (let ([v (read-bytes-avail! (make-bytes 10) in)])
				 (test #t procedure? v)
				 (test spec v 'ok 5 5 5)))])
			(when peek?
			  (test '(second one) peek-byte-or-special in (+ (bytes-length a) 1 (bytes-length b))))
			(get-one-str a)
			(get-one-special '(first one))
			(get-one-str b)
			(get-one-special '(second one))
			(get-one-str c)
			(if close?
			    (test eof read-byte in)
			    (test #f sync/timeout 0 in))))])
	  (let ([th (if threads?
			thread
			(lambda (f) (f)))])
	    (for-each (lambda (t)
			(and (thread? t) (thread-wait t)))
		      (list
		       (th w-th)
		       (th c-th)
		       (th r-th)))))))
    (go-stream #f #f #f #f)
    (go-stream #t #f #f #f)
    (go-stream #t #t #f #f)
    (go-stream #t #f #t #f)
    (go-stream #t #t #t #f)
    (go-stream #t #f #f #t)
    (go-stream #t #t #f #t)
    (go-stream #t #f #t #t)
    (go-stream #t #t #t #t)))

;; Check port shortcuts for `make-input-port' and `make-output-port' with
;; pipes and specials
(let-values ([(i o) (make-pipe-with-specials 5)])
  (define i2 (make-input-port
              (object-name i)
              i
              i
              void))
  (define o2 (make-output-port
              (object-name o)
              o
              o
              void
              o))
  (test #f sync/timeout 0 i2)
  (test o2 sync/timeout 0 o2)
  (write-bytes #"01234" o2)
  (test #f sync/timeout 0 o2)
  (test i2 sync/timeout 0 i2)
  (test #"01234" read-bytes 5 i2)
  (test 0 read-bytes-avail!* (make-bytes 3) i2)
  (thread (lambda () 
            (sync (system-idle-evt))
            (write-bytes #"5" o2)))
  (test #\5 read-char i2)
  (let ([s (make-bytes 6)])
    (thread (lambda () 
              (sync (system-idle-evt))
              (test 5 write-bytes-avail #"6789ab" o2)))
    (test 5 read-bytes-avail! s i2)
    (test #"6789a\0" values s))

  (test #t port-writes-special? o2)
  (write-special 'ok o2)
  (test 'ok read-byte-or-special i2)

  (test #t write-special-avail* 'ok-again o2)
  (test i2 sync i2)
  (test 'ok-again read-byte-or-special i2)

  (void))

;; make-input-port/read-to-peek
(define (make-list-port #:eof-as-special? [eof-as-special? #f] . l)
  (make-input-port/read-to-peek 
   'list-port
   (lambda (bytes)
     (cond
      [(null? l) eof]
      [(byte? (car l))
       (bytes-set! bytes 0 (car l))
       (set! l (cdr l))
       1]
      [(and (char? (car l))
	    (byte? (char->integer (car l))))
       (bytes-set! bytes 0 (char->integer (car l)))
       (set! l (cdr l))
       1]
      [(and (not eof-as-special?)
            (eof-object? (car l)))
       (set! l (cdr l))
       eof]
      [else
       (let ([v (car l)])
	 (set! l (cdr l))
	 (lambda (a b c d) v))]))
   #f
   void))

(let ([p (make-input-port/read-to-peek 
          'example
          (lambda (bytes) (bytes-set! bytes 0 0) 1)
          #f
          void)])
  (close-input-port p)
  (test #t evt? (sync/timeout 0 (port-progress-evt p))))

(let ([p (make-list-port #\h #\e #\l #\l #\o)])
  (test (char->integer #\h) peek-byte p)
  (test (char->integer #\e) peek-byte p 1)
  (test (char->integer #\l) peek-byte p 2)
  (test #"hel" read-bytes 3 p)
  (test (char->integer #\l) peek-byte p)
  (test (char->integer #\o) peek-byte p 1)
  (test #"lo" read-bytes 3 p)
  (test eof peek-byte p)
  (test eof peek-byte p)
  (test eof read-byte p)
  (test eof read-byte p))

(let ([p (make-list-port #\h #\e #\l 'ack #\l #\o)])
  (test (char->integer #\h) read-byte p)
  (test (char->integer #\e) read-byte p)
  (test (char->integer #\l) read-byte p)
  (test 'ack read-byte-or-special p)
  (test (char->integer #\l) read-byte p)
  (test (char->integer #\o) read-byte p))

(let ([p (make-list-port #\h #\e #\l 'ack #\l #\o)])
  (test (char->integer #\h) peek-byte p)
  (test (char->integer #\l) peek-byte p 2)
  (test 'ack peek-byte-or-special p 3)
  (test (char->integer #\l) peek-byte p 4)
  (test #"hel" read-bytes 3 p)
  (test 'ack read-byte-or-special p)
  (test #"lo" read-bytes 4 p))

(test 'hello read (make-list-port #\h #\e #\l #\l #\o))
(let ([p (make-list-port #\h #\e #\l eof #\l #\o)])
  (test 'hel read p)
  (test eof read p)
  (test 'lo read p)
  (test eof read p)
  (test eof read p))
(let ([p (make-list-port #:eof-as-special? #t #\h #\e #\l eof #\l #\o)])
  (test 'hel read p)
  (test eof read p)
  (test 'lo read p)
  (test eof read p)
  (test eof read p))
(let ([p (make-list-port #\h #\e #\l #\u7238 #\l #\o)])
  (test 'hel read p)
  (test #\u7238 read p)
  (test 'lo read p))

(let ([p (make-list-port 65 eof 66 67)])
  (test 65 peek-byte p 0)
  (test eof peek-byte p 1)
  (test #t port-commit-peeked 2 (port-progress-evt p) always-evt p)
  (test 66 peek-byte p 0))

;; Check that make-input-port/read-to-peek isn't trying
;; to use chars when it should use bytes:
(let-values ([(pipe-r pipe-w) (make-pipe)])
  (write-byte 200 pipe-w)
  (let ([p (make-input-port/read-to-peek 'name
                                         (lambda (s)
                                           (read-bytes-avail!* s pipe-r))
                                         #f
                                         void)])
    (test 200 peek-byte p)
    (test 200 read-byte p)))

;; Tests for `peeking-input-port`
(let ()
  (define (try-peeking buffer-mode)
    (define an-original-port (open-input-string "123456789abc"))
    (define a-peeking-port (peeking-input-port an-original-port))
    (file-stream-buffer-mode a-peeking-port buffer-mode)
    (test "123" read-string 3 a-peeking-port)
    (test "456" read-string 3 a-peeking-port)

    (test "123" read-string 3 an-original-port)

    ;; It's not well defined what will happen now when buffering is
    ;; enabled, but we can predict given the current implementation:
    (test (if (eq? 'block buffer-mode) "789" "abc") read-string 3 a-peeking-port)

    (test #f file-stream-buffer-mode an-original-port)
    (test buffer-mode file-stream-buffer-mode a-peeking-port)
    (test (void) file-stream-buffer-mode a-peeking-port 'none)
    (test 'none file-stream-buffer-mode a-peeking-port))

  (try-peeking 'none)
  (try-peeking 'block))

;; read synchronization events
(define (go mk-hello sync atest btest)
  (test #t list? (list mk-hello sync atest btest))
  (test #"" sync (peek-bytes-evt 0 0 #f (mk-hello)))
  (test #"" sync (read-bytes-evt 0 (mk-hello)))
  (let ([p (mk-hello)])
    (atest #"hello" sync (peek-bytes-evt 5 0 #f p))
    (atest #"llo" sync (peek-bytes-evt 5 2 #f p))
    (atest #"hello" sync (read-bytes-evt 5 p))
    (atest eof sync (peek-bytes-evt 5 0 #f p))
    (atest eof sync (read-bytes-evt 5 p)))
  (test 0 sync (peek-bytes!-evt (make-bytes 0) 0 #f (mk-hello)))
  (test 0 sync (read-bytes!-evt (make-bytes 0) (mk-hello)))
  (let ([s (make-bytes 5)]
	[p (mk-hello)])
    (atest 5 sync (peek-bytes!-evt s 0 #f p))
    (btest #"hello" values s)
    (atest 3 sync (peek-bytes!-evt s 2 #f p))
    (btest #"llolo" values s)
    (bytes-copy! s 0 #"\0\0\0\0\0")
    (atest 5 sync (read-bytes!-evt s p))
    (btest #"hello" values s)
    (atest eof sync (read-bytes!-evt s p)))
  (test 0 sync (read-bytes-avail!-evt (make-bytes 0) (mk-hello)))
  (let ([s (make-bytes 5)]
	[p (mk-hello)])
    (atest 5 sync (peek-bytes-avail!-evt s 0 #f p))
    (btest #"hello" values s)
    (atest 2 sync (peek-bytes-avail!-evt s 3 #f p))
    (btest #"lollo" values s)
    (bytes-copy! s 0 #"\0\0\0\0\0")
    (atest 5 sync (read-bytes-avail!-evt s p))
    (btest #"hello" values s)
    (atest eof sync (read-bytes-avail!-evt s p)))
  (test "" sync (read-string-evt 0 (mk-hello)))
  (let ([p (mk-hello)])
    (atest "hello" sync (peek-string-evt 5 0 #f p))
    (atest "lo" sync (peek-string-evt 5 3 #f p))
    (atest "hello" sync (read-string-evt 5 p))
    (atest eof sync (peek-string-evt 5 0 #f p))
    (atest eof sync (peek-string-evt 5 100 #f p))
    (atest eof sync (read-string-evt 5 p)))
  (test 0 sync (read-string!-evt (make-string 0) (mk-hello)))
  (let ([s (make-string 5)]
	[p (mk-hello)])
    (let ([s2 (make-string 5)])
      (atest 5 sync (peek-string!-evt s2 0 #f p))
      (btest "hello" values s2))
    (atest 5 sync (read-string!-evt s p))
    (btest "hello" values s)
    (atest eof sync (read-string!-evt s p)))
  (let ([p (mk-hello)])
    (atest '(#"hello") sync (regexp-match-evt #rx"....." p)))
  (let ([p (mk-hello)])
    (atest '(#"hello") sync (regexp-match-evt #rx".*" p)))
  (let ([p (mk-hello)])
    (atest '(#"hel") sync (regexp-match-evt #rx"..." p))
    (atest '(#"lo") sync (regexp-match-evt #rx".." p)))
  (let ([p (mk-hello)])
    (atest #"hello" sync (read-bytes-line-evt p))
    (atest eof sync (read-bytes-line-evt p))
    (atest eof sync (eof-evt p)))
  (let ([p (mk-hello)])
    (atest "hello" sync (read-line-evt p))
    (atest eof sync (read-line-evt p))))
(go (lambda () (open-input-bytes #"hello")) sync test test)

(define (sync/poll . args) (apply sync/timeout 0 args))
(go (lambda () (open-input-bytes #"hello")) sync/poll test test)

(define (delay-hello)
  (let-values ([(r w) (make-pipe)])
    (thread (lambda ()
	      (sync (system-idle-evt))
	      (write-string "hello" w)
	      (close-output-port w)))
    r))
(go delay-hello sync test test)

(go (lambda ()
      (let-values ([(r w) (make-pipe)])
	r))
    sync/poll 
    (lambda args
      (apply test #f (cdr args)))
    (lambda args
      (apply test (if (string? (car args))
		      (make-string (string-length (car args)))
		      (make-bytes (bytes-length (car args))))
	     (cdr args))))


;; extra checks for read-line-evt:
(let ([p (open-input-string "ab\nc")])
  (test "ab" sync (read-line-evt p))
  (test "c" sync (read-line-evt p))
  (test eof sync (read-line-evt p)))
(let ([p (open-input-string "ab\nc")])
  (test "ab\nc" sync (read-line-evt p 'return))
  (test eof sync (read-line-evt p 'return)))
(let ([p (open-input-string "ab\r\nc\r")])
  (test "ab" sync (read-line-evt p 'return))
  (test "\nc" sync (read-line-evt p 'return))
  (test eof sync (read-line-evt p 'return)))
(let ([p (open-input-string "ab\r\nc\r")])
  (test "ab" sync (read-line-evt p 'return-linefeed))
  (test "c\r" sync (read-line-evt p 'return-linefeed))
  (test eof sync (read-line-evt p 'return-linefeed)))
(let ([p (open-input-string "ab\r\nc\r")])
  (test "ab" sync (read-line-evt p 'any))
  (test "c" sync (read-line-evt p 'any))
  (test eof sync (read-line-evt p 'any)))
(let ([p (open-input-string "ab\r\nc\r")])
  (test "ab" sync (read-line-evt p 'any-one))
  (test "" sync (read-line-evt p 'any-one))
  (test "c" sync (read-line-evt p 'any-one))
  (test eof sync (read-line-evt p 'any-one)))

;; input-port-append tests
(let* ([do-test
	;; ls is a list of strings for ports
	;;  n, m, q are positive
	;;  n and n+m < total length
	;;  n+m+q can be greater than total length
	(lambda (ls n m q)
	  (let* ([p (apply input-port-append #f (map open-input-string ls))]
		 [s (apply string-append ls)]
		 [l (string-length s)])
	    (test (substring s 0 n) peek-string n 0 p)
	    (test (substring s n (min l (+ n m q))) peek-string (+ m q) n p)
	    (test (substring s (+ n m) (min l (+ n m q))) peek-string q (+ n m) p)

	    (test (substring s 0 n) read-string n p)
	    
	    (test (substring s n (+ n m)) peek-string m 0 p)
	    (test (substring s (+ n m) (min l (+ n m q))) peek-string q m p)

	    (test (substring s n (+ n m)) read-string m p)

	    (test (substring s (+ n m) (min l (+ n m q))) peek-string q 0 p)))]
       [do-tests
	(lambda (ls)
	  (let ([l (apply + (map string-length ls))])
	    (let loop ([n 1])
	      (unless (= n (- l 2))
		(let loop ([m 1])
		  (unless (= (+ m n) (- l 1))
		    (do-test ls n m 1)
		    (do-test ls n m (- l n m))
		    (do-test ls n m (+ (- l n m) 2))
		    (loop (add1 m))))
		(loop (add1 n))))))])
  (do-tests '("apple" "banana"))
  (do-tests '("ax" "b" "cz")))
;; input-port-append and not-ready inputs
(let ([p0 (open-input-bytes #"123")])
  (let-values ([(p1 out) (make-pipe)])
    (let ([p (input-port-append #f p0 p1)])
      (display "4" out)
      (test #"1234" peek-bytes 4 0 p)
      (test #"34" peek-bytes 2 2 p)
      (test #"4" peek-bytes 1 3 p)
      (let* ([v #f]
	     [t (thread (lambda ()
			  (set! v (read-bytes 6 p))))])
	(test (void) sync (system-idle-evt) t)
	(display "56" out)
	(test (void) sync (system-idle-evt))
	(test t sync/timeout SLEEP-TIME t)
	(test #"123456" values v)))))

;; make-limited-input-port tests
(let* ([s (open-input-string "123456789")]
       [s2 (make-limited-input-port s 5)])
  (test #"123" peek-bytes 3 0 s2)
  (test #"12345" peek-bytes 6 0 s2)
  (test #"12" read-bytes 2 s2)
  (test #"345" read-bytes 6 s2)
  (test eof read-bytes 6 s2))
(let* ([s (open-input-string "123456789")]
       [s2 (make-limited-input-port s 5)])
  (test #t port-provides-progress-evts? s2)
  (test #"123" peek-bytes 3 0 s2)
  (test #t port-commit-peeked 3 (port-progress-evt s2) always-evt s2)
  (test #"45" read-bytes 2 s2))
(let-values ([(i o) (make-pipe)])
  (let ([s (make-limited-input-port i 5)])
    (test #f char-ready? s)
    (display "123" o)
    (test #t char-ready? s)
    (let ([b (make-bytes 10)])
      (test 3 peek-bytes-avail!* b 0 #f s)
      (test 3 read-bytes-avail!* b s)
      (test 0 peek-bytes-avail!* b 0 #f s)
      (display "456" o)
      (test 2 peek-bytes-avail!* b 0 #f s)
      (test 1 peek-bytes-avail!* b 1 #f s)
      (test 2 read-bytes-avail!* b s))))

;; Make sure that blocking on a limited input port doesn't
;; block in the case of a peek after available bytes:
(let ()
  (define-values (i o) (make-pipe))
  (write-char #\a o)
  (write-char #\a o)
  
  (thread (lambda ()
            (sync (system-idle-evt))
            (write-char #\b o)))
  (test 1 peek-bytes-avail! (make-bytes 1) 2 #f (make-limited-input-port i 10)))
	     
;; ----------------------------------------
;; Conversion wrappers

(define (try-eip-seq encoding only-if-avail? bytes try-map)
  (when (or (not only-if-avail?)
            (let ([c (bytes-open-converter "UTF-8" encoding)])
              (and c
                   (bytes-close-converter c)
                   #t)))
    (let* ([p (open-input-bytes bytes)]
           [p2 (reencode-input-port p encoding #".!")])
      (for-each (lambda (one-try)
                  (let ([p (if (car one-try)
                               p2
                               p)]
                        [len (cadr one-try)]
                        [expect (caddr one-try)])
                    (test expect read-bytes len p)))
                try-map))))

(try-eip-seq "UTF-8" #f #"apple" `((#t 3 #"app") (#f 2 #"le") (#t 4 ,eof)))
(try-eip-seq "UTF-8" #f #"ap\303\251ple" `((#t 3 #"ap\303") (#f 2 #"pl") (#t 4 #"\251e") (#t 5 ,eof)))
(try-eip-seq "ISO-8859-1" #t #"ap\303\251ple" `((#t 3 #"ap\303") (#f 2 #"\251p") (#t 4 #"\203le") (#t 5 ,eof)))
(try-eip-seq "UTF-8" #f #"ap\251ple" `((#t 2 #"ap") (#f 2 #"\251p") (#t 4 #"le") (#t 5 ,eof)))
(try-eip-seq "UTF-8" #f #"ap\251ple" `((#t 3 #"ap.") (#f 1 #"p") (#t 4 #"!le") (#t 5 ,eof)))
(try-eip-seq "UTF-8" #f #"ap\251ple" `((#t 4 #"ap.!") (#f 1 #"l") (#t 4 #"pe") (#t 5 ,eof)))

(let ([try (lambda (s s2)
             (let ([mk (lambda ()
                         (reencode-input-port (open-input-string s) "UTF-8" #f #f 'test #t))])
               (let ([p (mk)])
                 (for ([c (in-string s2)])
                   (test c read-char p))
                 (test eof read-char p))
               (let ([p (mk)])
                 (test s2 read-string (add1 (string-length s2)) p))
               (when ((string-length s2) . > . 2)
                 (test (substring s2 0 2) read-string 2 (mk)))
               (let-values ([(r w) (make-pipe-with-specials)])
                 (display s w)
                 (write-special 'x w)
                 (display s w)
                 (close-output-port w)
                 (let ([p (reencode-input-port r "UTF-8" #f #f 'test #t)])
                   (test s2 read-string (string-length s2) p)
                   (test 'x read-char-or-special p)
                   (test s2 read-string (string-length s2) p)
                   (test eof read-char-or-special p)))))])
  (for-each (lambda (cr)
              (try cr "\n")
              (try (format "a~a" cr) "a\n")
              (try (format "a~a12" cr) "a\n12")
              (try (format "~a12" cr) "\n12")
              (try (format "a\n~a12" cr) "a\n\n12")
              (try (format "a~a\r12" cr) "a\n\n12"))
            '("\n" "\r" "\r\n" "\x85" "\r\x85" "\u2028"))
  (try "a\u2028\r\n12" "a\n\n12"))

(let-values ([(in out) (make-pipe-with-specials)])
  (display "ok" out)
  (write-special 'special! out)
  (display "yz" out)
  (let ([p (reencode-input-port in "UTF-8")])
    (test #"ok" read-bytes 2 p)
    (test 'special! read-byte-or-special p)
    (test #"yz" read-bytes 2 p)
    (close-output-port out)
    (test eof read-bytes 3 p)))

(let*-values ([(r w) (make-pipe 10)]
	      [(w2) (reencode-output-port w "UTF-8" #"!?")])
  (test 4 write-bytes #"abcd" w2)
  (flush-output w2)
  (test #"abcd" read-bytes 4 r)
  
  (test 3 write-bytes #"abc" w2)
  (test 0 read-bytes-avail!* (make-bytes 10) r)
  (test 1 write-bytes-avail #"wx" w2) ; implementation converts minimal prefix
  (test #"abcw" read-bytes 4 r)

  ;; Check encoding error
  (test 4 write-bytes #"ab\303x" w2)
  (flush-output w2)
  (test #"ab!?x" read-bytes 5 r)

  ;; Check flushing in middle of encoding:
  (test 3 write-bytes #"ab\303" w2)
  (test 0 read-bytes-avail!* (make-bytes 10) r)
  (test 1 write-bytes-avail #"\251x" w2)
  (test #"ab\303\251" read-bytes 4 r)
  (test 1 write-bytes-avail #"abc" w2)
  (test #"a" read-bytes 1 r)

  ;; Check blocking on full pipe:
  (test 10 write-bytes #"1234567890" w2)
  (flush-output w2)
  (test #f sync/timeout 0.0 w)
  (test #f sync/timeout 0.0 w2)
  (test 0 write-bytes-avail* #"123" w2)
  (test 0 write-bytes-avail* #"123" w2)
  (test 0 write-bytes-avail* #"123" w2)
  (test #"1234567890" read-bytes 10 r)
  (test w2 sync/timeout 0.0 w2)
  (test 1 write-bytes-avail #"123" w2)

  ;; Check specials:
  (let*-values ([(in out) (make-pipe-with-specials)]
		[(out2) (reencode-output-port out "UTF-8" #"!")])
    (test 3 write-bytes #"123" out2)
    (test #t write-special 'spec out2)
    (test 3 write-bytes #"456" out2)
    (flush-output out2)
    (test #"123" read-bytes 3 in)
    (test 'spec read-char-or-special in)
    (test #"456" read-bytes 3 in))

  (void))

;; Check buffer modes:
(let ()
  (define (check-buffering flush-output)
    (let ([i (open-input-string "abc")]
          [o (open-output-string)])
      (test #f file-stream-buffer-mode i)
      (test #f file-stream-buffer-mode o)
      (let ([ei (reencode-input-port i "UTF-8")]
            [eo (reencode-output-port o "UTF-8")])
        (test 'none file-stream-buffer-mode ei)
        (test 'block file-stream-buffer-mode eo)

        (test (void) display 10 eo)
        (test (void) display 12 eo)
        (test (void) newline eo)
        (test #"" get-output-bytes o)
        (test (void) flush-output eo)
        (test #"1012\n" get-output-bytes o)
        
        (test (void) file-stream-buffer-mode eo 'line)
        (test 'line file-stream-buffer-mode eo)
        (test (void) display 13 eo)
        (test #"1012\n" get-output-bytes o)
        (test (void) newline eo)
        (test #"1012\n13\n" get-output-bytes o)
        (test (void) flush-output eo)
        (test #"1012\n13\n" get-output-bytes o)

        (test (void) display 14 eo)
        (test #"1012\n13\n" get-output-bytes o)
        (test (void) file-stream-buffer-mode eo 'none)
        (test #"1012\n13\n14" get-output-bytes o)
        (test 'none file-stream-buffer-mode eo)
        (test (void) display 15 eo)
        (test #"1012\n13\n1415" get-output-bytes o)

        (test #\a read-char ei)
        (test #\b peek-char i)
        (test (void) file-stream-buffer-mode ei 'block)
        (test 'block file-stream-buffer-mode ei)
        (test #\b read-char ei)
        (test eof peek-char i)
        (test #\c read-char ei)
        (test eof read-char ei))))
  ;; (check-buffering flush-output)
  (let ([p (make-plumber)])
    (parameterize ([current-plumber p])
      (check-buffering (lambda (o) (plumber-flush-all p))))))

(err/rt-test
 (port->bytes (reencode-input-port (open-input-bytes #"\xFF\xFF") "utf-8"))
 (lambda (exn)
   (regexp-match? #rx"^reencode-input-port:" (exn-message exn))))
(err/rt-test
 (let ([o (reencode-output-port (open-output-bytes) "utf-8")])
   (display #"\xFF\xFF" o) (flush-output o))
 (lambda (exn)
   (regexp-match? #rx"^reencode-output-port:" (exn-message exn))))

;; Check that slow input stream is handled correctly:
(let ()
  (define poem (bytes-append #"\307\256\314\306\272\376\264\272\320\320\n\n\271\302\311\275\313\302\261\261"
                             #"\274\326\315\244\316\367\n\313\256\303\346\263\365\306\275\324\306\275\305\265"
                             #"\315\n\274\270\264\246\324\347\335\272\325\371\305\257\312\367\n\313\255\274"
                             #"\322\320\302\321\340\327\304\264\272\304\340\n\302\322\273\250\275\245\323\373"
                             #"\303\324\310\313\321\333\n\307\263\262\335\262\305\304\334\303\273\302\355\314"
                             #"\343\n\327\356\260\256\272\376\266\253\320\320\262\273\327\343\n\302\314\321\356"
                             #"\322\365\300\357\260\327\311\263\265\314\n"))
  (define (slower p)
    (make-input-port
     (object-name p)
     (lambda (bstr)
       (if (zero? (random 2))
           (wrap-evt (alarm-evt (+ (current-inexact-milliseconds) 5))
                     (lambda (v) 0))
           (read-bytes-avail! bstr p)))
     #f
     (lambda () (void))))
  (define i (reencode-input-port (slower (open-input-bytes poem)) "gbk" #"?"))
  (test #f regexp-match? #px#"temple" i)
  (close-input-port i))

;; --------------------------------------------------

(let ([o (open-output-bytes)])
  (port-count-lines! o)
  (define o2 (transplant-output-port o
                                     (lambda ()
                                       (define-values (l c p) (port-next-location o))
                                       (values (* 2 l) (* 2 c) (* 2 p)))
                                     57))
  (test 0 file-position o)
  (test 56 file-position o2)
  (port-count-lines! o2)
  (test-values '(1 0 1) (lambda () (port-next-location o)))
  (test-values '(2 0 2) (lambda () (port-next-location o2)))
  (write-byte 45 o2)
  (test-values '(1 1 2) (lambda () (port-next-location o)))
  (test-values '(2 2 4) (lambda () (port-next-location o2)))
  (test (file-stream-buffer-mode o) file-stream-buffer-mode o2))

(let ([i (open-input-bytes #"x")])
  (port-count-lines! i)
  (define i2 (transplant-input-port i
                                    (lambda ()
                                      (define-values (l c p) (port-next-location i))
                                      (values (* 2 l) (* 2 c) (* 2 p)))
                                    57))
  (test 0 file-position i)
  (test 56 file-position i2)
  (port-count-lines! i2)
  (test-values '(1 0 1) (lambda () (port-next-location i)))
  (test-values '(2 0 2) (lambda () (port-next-location i2)))
  (test (char->integer #\x) read-byte i)
  (test-values '(1 1 2) (lambda () (port-next-location i)))
  (test-values '(2 2 4) (lambda () (port-next-location i2)))
  (test (file-stream-buffer-mode i) file-stream-buffer-mode i2))

;; --------------------------------------------------

(let-values ([(in out) (make-pipe)])
  (let ([in2 (dup-input-port in #f)]
        [out2 (dup-output-port out #f)])
    (port-count-lines! in2)
    (test-values (list 1 0 1) (lambda ()
                                (port-next-location in2)))
    (display "\"hel\u03BBo\"\n" out)
    (test "hel\u03BBo" read in2)
    (test-values (list 1 7 8)
                 (lambda ()
                   (port-next-location in2)))
    (test #\newline read-char in2)
    (test-values (list 2 0 9) 
                 (lambda () 
                   (port-next-location in2)))
    (close-output-port out2)
    (test #f char-ready? in2)
    (close-input-port in2)
    (display "x " out)
    (test 'x read in)))
    
;; --------------------------------------------------
;; Test port-commit-peeked and position counting

(let ([check
       (lambda (in [d 0] [first-three-bytes (bytes->list #"123")] [char-len 3])
         (test d file-position in)
         (let-values ([(l c p) (port-next-location in)])
           (test p add1 d)
           (test (car first-three-bytes) peek-byte-or-special in 0)
           (test (cadr first-three-bytes) peek-byte-or-special in 1)
           (test (caddr first-three-bytes) peek-byte-or-special in 2)
           (test d file-position in)
           (let-values ([(l2 c2 p2) (port-next-location in)])
             (test (list l c p) list l2 c2 p2))
           (port-commit-peeked 3 (port-progress-evt in) always-evt in)
           (test (+ d 3) file-position in)
           (let-values ([(l2 c2 p2) (port-next-location in)])
             (test (list l (and c (+ c char-len)) (+ p (if c char-len 3))) 
                   list l2 c2 p2))
           (test #\4 read-char in)))])
  (define (check-all count-lines!)
    (define (check-made first-three-bytes char-len
                        [get-loc #f] [on-consume void]
                        [init-pos 1]
                        #:wrap [wrap values])
      (define stream (append first-three-bytes (list (char->integer #\4))))
      (define p (wrap
                 (make-input-port/read-to-peek
                  'made
                  (lambda (bstr)
                    (let ([b (car stream)])
                      (set! stream (cdr stream))
                      (if (byte? b)
                          (begin
                            (bytes-set! bstr 0 b)
                            1)
                          (lambda (srcloc line col pos) b))))
                  #f
                  void
                  get-loc
                  void
                  init-pos
                  #f
                  #f
                  on-consume)))
      (count-lines! p)
      (check p (sub1 init-pos) first-three-bytes char-len))
    (check-made (bytes->list #"123") 3)
    (check-made (bytes->list #"123") 3 #:wrap (lambda (in) (make-limited-input-port in 5)))
    (check-made (list (char->integer #\1) 'special (char->integer #\3)) 3)
    (check-made (bytes->list (string->bytes/utf-8 "1\u3BB")) 2)
    (let ()
      (define line 1) (define col 0) (define pos 1)
      (check-made (bytes->list (string->bytes/utf-8 "123"))
                  1 ;; claim that "123" is a single character
                  (lambda () (values line col pos))
                  (lambda (n)
                    (let ([n (if (= col 0) 
                                 1 ;; "123" is a single character
                                 n)])
                      (set! col (+ col n))
                      (set! pos (+ pos n))))))
    (void))
  (check-all void)
  (check-all port-count-lines!))

;; --------------------------------------------------
;; Check that commit-based reading counts against the limit:

(let ([p (make-limited-input-port
          (open-input-string "A\nB\nC\nD\n") 
          4)])
  (test `((#"A" 2) (#"B" 4) (,eof 4) (,eof 4))
        list
        (list (sync (read-bytes-line-evt p))
              (file-position p))
        (list (sync (read-bytes-line-evt p))
              (file-position p))
        (list (sync (read-bytes-line-evt p))
              (file-position p))
        (list (sync (read-bytes-line-evt p))
              (file-position p))))
    
;; --------------------------------------------------

;; check that commit-based reading counts against a port limit;
;; this test also checks an interaction of `make-limited-input-port'
;; and progress evts, so run it several times
(for ([i 100])
  (let* ([p (make-limited-input-port
             (open-input-string "A\nB\nC\nD\n") 
             4)]
         [N 6]
         [chs (for/list ([i N])
                (let ([ch (make-channel)])
                  (thread
                   (lambda ()
                     (channel-put ch (list (sync (read-bytes-line-evt p))
                                           (file-position p)))))
                  ch))]
         [r (for/list ([ch chs])
              (channel-get ch))])
    (test #t list? r)))

;; check proper locking for concurrent access:
(for ([mk-p (list
             (lambda ()
               (open-input-string "A\nB\n"))
             (lambda ()
               (make-limited-input-port
                (open-input-string "A\nB\nC\nD\n") 
                4)))])
  (for ([i 100])
    (let* ([p (mk-p)]
           [N 6]
           [chs (for/list ([i N])
                  (let ([ch (make-channel)])
                    (thread
                     (lambda ()
                       (when (even? i) (sleep))
                       (channel-put ch (list (sync (read-bytes-line-evt p))
                                             (file-position p)
                                             (let ()
                                               (define-values (l c pos) (port-next-location p))
                                               (sub1 pos))))))
                    ch))]
           [rs (for/list ([ch chs])
                 (channel-get ch))])
      (test 2 apply + (for/list ([r rs]) (if (bytes? (car r)) 1 0)))
      (for ([r rs]) 
        (if (eof-object? (car r))
            (test 4 cadr r)
            (let ([memq? (lambda (a l) (and (memq a l) #t))])
              (test #t memq? (cadr r) '(2 4))
              (test #t = (cadr r) (caddr r))))))))

(let-values ([(in out) (make-pipe-with-specials)])
  (struct str (v)    
          #:property prop:custom-write
          (lambda (a-str port mode)
            (let ([recur (case mode
                           [(#t) write]
                           [(#f) display]
                           [else
                            (lambda (p port)
                              (print p port mode))])])
              (recur (str-v a-str) port))))
    (write (str "hello world") out)
    (flush-output out)
    (test "hello world" read in))

;; --------------------------------------------------
;; check that `read-bytes-evt' gets

(let ()
  (define-values (i o) (make-pipe))
  (define res #f)
  
  (define t
    (thread
     (lambda ()
       (set! res (sync (read-bytes-evt 2 i))))))
  
  (write-bytes #"1" o)
  (sync (system-idle-evt))
  (thread-suspend t)
  (write-bytes #"2" o)
  (test #"1" read-bytes 1 i)
  (thread-resume t)
  (sleep)
  (write-bytes #"34" o)
  
  (sync t)
  (test #"23" values res))

;; --------------------------------------------------
;; check that string and byte-string evts can be reused

(let ()
  (define (check-can-reuse read-bytes-evt read-bytes write-bytes integer->byte list->bytes bytes?)
    (define N 10)
    (define M 160)
    (define PORT 5999)

    (define (make-alarm-e)
      (alarm-evt (+ (current-inexact-milliseconds) 5)))

    (define ((connection-handler in out with-alarm?))
      (let loop ((alarm-e (make-alarm-e))
                 (read-e (read-bytes-evt 16 in)))
        (sync (if with-alarm?
                  (wrap-evt alarm-e (lambda (_) (loop (make-alarm-e) read-e)))
                  never-evt)
              (wrap-evt read-e
                        (lambda (bs)
                          (when (bytes? bs)
                            (sleep 0.01)
                            (write-bytes bs out)
                            (flush-output out))
                          (loop alarm-e read-e)))
              (wrap-evt (eof-evt in)
                        (lambda (_)
                          (close-input-port in)
                          (close-output-port out))))))

    (define listener (tcp-listen PORT 4 #t))
    (define server
      (thread
       (lambda ()
         (for ([i N])
           (define-values (in out) (tcp-accept listener))
           ((connection-handler in out #t))))))

    (let ([s (list->bytes
              (for/list ([i M])
                (integer->byte (random 512))))])
      (for ([i N])
        (define-values (i o) (tcp-connect "localhost" PORT))
        (write-bytes s o)
        (close-output-port o)
        (test s read-bytes M i)
        (close-input-port i)))

    (sync server)
    (tcp-close listener))

  (let ([integer->byte (lambda (s) (bitwise-and s #xFF))])
    (check-can-reuse read-bytes-evt read-bytes write-bytes integer->byte list->bytes bytes?)
    ;; the following should work because we use the same evt only after
    ;; success or to start at the same point in the input stream:
    (check-can-reuse (lambda (n in)
                       (define bstr (make-bytes n))
                       (wrap-evt (read-bytes!-evt bstr in)
                                 (lambda (v) (if (eof-object? v) v bstr))))
                     read-bytes write-bytes integer->byte list->bytes bytes?))
  (check-can-reuse read-string-evt read-string write-string integer->char list->string string?))

;; --------------------------------------------------

(report-errs)
