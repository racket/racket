
(load-relative "loadtest.rktl")
(require compiler/find-exe)

(Section 'port)

(define (call-in-temporary-directory thunk)
  (define dir (make-temporary-file "tmp~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (parameterize ([current-directory dir])
       (thunk)))
   (lambda () (delete-directory dir))))

(define thread-procs (list thread
                           (lambda (thunk) (thread #:pool 'own thunk))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests for progress events and commits

(define (test-hello-port p commit-eof?)
  (let ([progress-evt1 (port-progress-evt p)])
    (test #t evt? progress-evt1)
    (test #t progress-evt? progress-evt1)
    (test #t progress-evt? progress-evt1 p)
    (test #f progress-evt? progress-evt1 (open-input-string ""))
    (test #f sync/timeout 0 progress-evt1)
    (test #f sync/timeout 0 progress-evt1)
    (test #\h read-char p)
    (test progress-evt1 sync/timeout 0 progress-evt1)
    (let ([progress-evt2 (port-progress-evt p)])
      (test #f sync/timeout 0 progress-evt2)
      (test #\e peek-char p)
      (test #f sync/timeout 0 progress-evt2)
      (test #f port-commit-peeked 1 progress-evt1 always-evt p)
      (test #t port-commit-peeked 1 progress-evt2 always-evt p)
      (test progress-evt1 sync/timeout 0 progress-evt1)
      (test progress-evt2 sync/timeout 0 progress-evt2)
      (let ([progress-evt3 (port-progress-evt p)])
	(test #\l peek-char p)
	(test #\l peek-char p 1)
	(test #\o peek-char p 2)
	(test eof peek-char p 3)
	(test eof peek-char p 4)
	(test #t port-commit-peeked 2 progress-evt3 always-evt p)
	(test #\o peek-char p)
	(test eof peek-char p 1)
	(let ([progress-evt4 (port-progress-evt p)])
	  (test #f sync/timeout 0 progress-evt4)
	  (test #t port-commit-peeked (if commit-eof? 2 1) progress-evt4 always-evt p)
	  (test progress-evt4 sync/timeout 0 progress-evt4)
	  (let ([progress-evt5 (port-progress-evt p)])
	    (test #f sync/timeout 0 progress-evt5)
	    (test eof peek-char p)
	    (test eof read-char p)
	    (test #f sync/timeout 0 progress-evt5)
            ;; it happens that a commit of EOF succeeds for tested ports:
	    (test #t port-commit-peeked 1 progress-evt5 always-evt p)))))))

(test-hello-port (open-input-string "hello") #f)
(test-hello-port (open-input-string "hello") #t)
(let ([test-pipe
       (lambda (commit-eof?)
	 (let-values ([(r w) (make-pipe)])
	   (write-string "hello" w)
	   (close-output-port w)
	   (test-hello-port r commit-eof?)))])
  (test-pipe #f)
  (test-pipe #t))
(let ([test-file
       (lambda (commit-eof?)
         (call-in-temporary-directory
          (lambda ()
            (with-output-to-file "tmp8"
              #:exists 'truncate/replace
              (lambda () (write-string "hello")))
            (define p (open-input-file "tmp8"))
            (test-hello-port p commit-eof?)
            (close-input-port p)
            (delete-file "tmp8"))))])
  (test-file #f)
  (test-file #t))

(let-values ([(r w) (make-pipe)])
  (test #t pipe-port? r)
  (test #t pipe-port? w)
  (write-byte 200 w)
  (test #t byte-ready? r)
  (test #f char-ready? r))

;; Progress evts for a closed port should start out ready:
(let ()
  (define-values (i o) (make-pipe))
  (close-input-port i)
  (test #t evt? (sync/timeout 0 (port-progress-evt i)))
  (test 0 peek-bytes-avail! (make-bytes 10) 0 (port-progress-evt i) i))

(test #f pipe-port? (open-input-string ""))
(test #f pipe-port? (open-output-string))

(test #t string-port? (open-input-string ""))
(test #t string-port? (open-input-bytes #""))
(test #t string-port? (open-output-bytes))
(test #t string-port? (open-output-string))

;; concurrent close on input fails
(for ([thread (in-list thread-procs)])
  (define-values (i o) (make-pipe))
  (thread (lambda ()
            (sync (system-idle-evt))
            (close-input-port i)))
  (err/rt-test
   (peek-bytes-avail! (make-bytes 10) 0 #f i)
   exn:fail?))

;; concurrent close on input triggers progress
(for ([thread (in-list thread-procs)])
  (define-values (i o) (make-pipe))
  (thread (lambda ()
            (sync (system-idle-evt))
            (close-input-port i)))
  (test 0 peek-bytes-avail! (make-bytes 10) 0 (port-progress-evt i) i))

;; concurrent close on output fails
(for ([thread (in-list thread-procs)])
  (define-values (i o) (make-pipe 4096))
  (thread (lambda ()
            (sync (system-idle-evt))
            (close-output-port o)))
  (err/rt-test
   (let loop ()
     (write-bytes #"hello" o)
     (loop))
   exn:fail?))

;; concurrent close of input unblocks limited output
(for ([thread (in-list thread-procs)])
  (define-values (i o) (make-pipe 4096))
  (define done? #f)
  (thread (lambda ()
            (sync (system-idle-evt))
            (set! done? #t)
            (close-input-port i)))

  ;; Shouldn't get stuck:
  (let loop ()
    (write-bytes #"hello" o)
    (unless done?
      (loop))))

(test '(#t #t #t #t #t #t #t #t #t #t)
      'concurrent-read
      (let ()
        (define-values (i o) (make-pipe 4096))
        (map
         thread-wait
         (for/list ([k (in-range 10)])
           (thread #:pool 'own
                   #:keep 'results
                   (lambda ()
                     (for/and ([j (in-range 1000)])
                       (write-bytes #"a" o)
                       (equal? #"a" (read-bytes 1 i)))))))))

(test 6000
      'concurrent-write
      (let ()
        (define o (open-output-bytes))
        (for-each
         thread-wait
         (for/list ([k (in-range 6)])
           (thread #:pool 'own
                   (lambda ()
                     (for ([j (in-range 1000)])
                       (write-bytes #"a" o))))))
        (bytes-length (get-output-bytes o))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Based on the Racket manual...

;; A port with no input...
;; Easy: \scheme{(open-input-bytes #"")}
;; Hard:
(define /dev/null-in 
  (make-input-port 'null
		   (lambda (s) eof)
		   (lambda (skip s progress-evt) eof)
		   void
		   (lambda () never-evt)
		   (lambda (k progress-evt done-evt)
		     (error "no successful peeks!"))))
(test eof read-char /dev/null-in)
(test eof peek-char /dev/null-in)
(test eof read-byte-or-special /dev/null-in)
(test eof peek-byte-or-special /dev/null-in 100)
(test #t evt? (port-progress-evt /dev/null-in))
(test #f sync/timeout 0 (port-progress-evt /dev/null-in))
(err/rt-test (port-commit-peeked 100 (port-progress-evt /dev/null-in) always-evt /dev/null-in)
	     exn:fail?)
(err/rt-test (port-commit-peeked 100 never-evt always-evt /dev/null-in))

;; A port that produces a stream of 1s, but always
;; though an evt:
(let ()
  (define stubborn-infinite-ones
    (make-input-port
     'ones
     (lambda (s)
       (wrap-evt always-evt
                 (lambda (ae)
                   (bytes-set! s 0 (char->integer #\1))
                   1)))
     (lambda (s skip-n progress-evt)
       (wrap-evt always-evt
                 (lambda (ae)
                   (bytes-set! s 0 (char->integer #\1))
                   1)))
     void))
  (test "11111" read-string 5 stubborn-infinite-ones)
  (test "11111" peek-string 5 0 stubborn-infinite-ones)
  (test #t byte-ready? stubborn-infinite-ones)
  (test #t char-ready? stubborn-infinite-ones)
  (test "11111" read-string 5 stubborn-infinite-ones)
  (test stubborn-infinite-ones sync/timeout 0 stubborn-infinite-ones))

;; A port that produces a stream of 1s:
(define infinite-ones 
  (make-input-port
   'ones
   (lambda (s) 
     (bytes-set! s 0 (char->integer #\1)) 1)
   #f
   void))
(test "11111" read-string 5 infinite-ones)

;; An infinite stream of 1s with a specific peek procedure:
(define infinite-ones 
  (let ([one! (lambda (s) 
	        (bytes-set! s 0 (char->integer #\1)) 1)])
    (make-input-port
     'ones
     one!
     (lambda (s skip progress-evt) (one! s))
     void)))
(test "11111" read-string 5 infinite-ones)

;; Now we can peek ahead arbitrarily far:
(test "11111" peek-string 5 (expt 2 5000) infinite-ones)

;; The port doesn't supply procedures to implement progress events:
(test #f port-provides-progress-evts? infinite-ones)
(err/rt-test (port-progress-evt infinite-ones) exn:application:mismatch?)

;; This port produces 0, 1, 2, 0, 1, 2, etc,
;;  but it is not thread-safe, because multiple
;;  threads might read and change n
(define mod3-peeked? #f)
(define mod3-cycle/one-thread
  (let* ([n 2]
	 [mod! (lambda (s delta)
		 (bytes-set! s 0 (+ 48 (modulo (+ n delta) 3)))
                 1)])
    (make-input-port
     'mod3-cycle/not-thread-safe
     (lambda (s)
       (set! n (modulo (add1 n) 3))
       (mod! s 0))
     (lambda (s skip progress-evt)
       (set! mod3-peeked? #t)
       (mod! s (add1 skip)))
     void)))
(test "01201" read-string 5 mod3-cycle/one-thread)
(test #f values mod3-peeked?)
(test "20120" peek-string 5 (sub1 (expt 2 5000)) mod3-cycle/one-thread)

;; Same thing, but thread-safe and kill-safe, and with progress
;; events. Only the server thread touches the stateful part
;; directly. (See the output port examples for a simpler thread-safe
;; example, but this one is more general.)
(define (make-mod3-cycle)
  (define read-req-ch (make-channel))
  (define peek-req-ch (make-channel))
  (define progress-req-ch (make-channel))
  (define commit-req-ch (make-channel))
  (define close-req-ch (make-channel))
  (define closed? #f)
  (define n 0)
  (define progress-sema #f)
  (define (mod! s delta)
    (bytes-set! s 0 (+ 48 (modulo (+ n delta) 3)))
    1)
  (define (remq v l)
    (if (eq? (car l) v)
	(cdr l)
	(cons (car l) (remq v (cdr l)))))
  ;; ----------------------------------------
  ;; The server has a list of outstanding commit requests,
  ;;  and it also must service each port operation (read, 
  ;;  progress-evt, etc.)
  (define (serve commit-reqs response-evts)
    (apply
     sync
     (handle-evt read-req-ch (handle-read commit-reqs response-evts))
     (handle-evt progress-req-ch (handle-progress commit-reqs response-evts))
     (handle-evt commit-req-ch (add-commit commit-reqs response-evts))
     (handle-evt close-req-ch (handle-close commit-reqs response-evts))
     (append
      (map (make-handle-response commit-reqs response-evts) response-evts)
      (map (make-handle-commit commit-reqs response-evts) commit-reqs))))
  ;; Read/peek request: fill in the string and commit
  (define ((handle-read commit-reqs response-evts) r)
    (let ([s (car r)]
	  [skip (cadr r)]
	  [ch (caddr r)]
	  [nack (cadddr r)]
	  [peek? (car (cddddr r))]
	  [progress-evt (cdr (cddddr r))])
      (let ([nothing? (or closed?
			  (and progress-evt
			       (sync/timeout 0 progress-evt)))])
	(unless nothing?
	  (mod! s skip)
	  (unless peek?
	    (commit! 1)))
	;; Add an event to respond:
	(serve commit-reqs
	       (cons (choice-evt nack
				 (channel-put-evt ch (if nothing? 
							 #f
							 (if closed? 0 1))))
		     response-evts)))))
  ;; Progress request: send a peek evt for the current 
  ;;  progress-sema
  (define ((handle-progress commit-reqs response-evts) r)
    (let ([ch (car r)]
	  [nack (cdr r)])
      (unless progress-sema
	(set! progress-sema (make-semaphore (if closed? 1 0))))
      ;; Add an event to respond:
      (serve commit-reqs
	     (cons (choice-evt nack
			       (channel-put-evt
				ch
				(semaphore-peek-evt progress-sema)))
		   response-evts))))
  ;; Commit request: add the request to the list
  (define ((add-commit commit-reqs response-evts) r)
    (serve (cons r commit-reqs) response-evts))
  ;; Commit handling: watch out for progress, in which case
  ;;  the response is a commit failure; otherwise, try
  ;;  to sync for a commit. In either event, remove the
  ;;  request from the list
  (define ((make-handle-commit commit-reqs response-evts) r)
    (let ([k (car r)]
	  [progress-evt (cadr r)]
	  [done-evt (caddr r)]
	  [ch (cadddr r)]
	  [nack (cddddr r)])
      ;; Note: we don't check that k is $\leq$ the sum of
      ;;  previous peeks, because the entire stream is actually
      ;;  known, but we could send an exception in that case.
      (choice-evt
       (handle-evt progress-evt
		   (lambda (x) 
		     (sync nack (channel-put-evt ch #f))
		     (serve (remq r commit-reqs) response-evts)))
       ;; Only create an event to satisfy done-evt if progress-evt
       ;;  isn't already ready.
       ;; Afterward, if progress-evt becomes ready, then this
       ;;  event-making function will be called again.) We know this
       ;;  only because the server control all posts to progress-evt.
       (if (sync/timeout 0 progress-evt)
	   never-evt
	   (handle-evt done-evt
		       (lambda (v)
			 (commit! k)
			 (sync nack (channel-put-evt ch #t))
			 (serve (remq r commit-reqs) response-evts)))))))
  ;; Response handling: as soon as the respondee listerns,
  ;;  remove the response
  (define ((make-handle-response commit-reqs response-evts) evt)
    (handle-evt evt
		(lambda (x)
		  (serve commit-reqs
			 (remq evt response-evts)))))
  ;; Close handling: post the progress sema, if any, and set
  ;;   the \scheme{closed?} flag
  (define ((handle-close commit-reqs response-evts) r)
    (let ([ch (car r)]
	  [nack (cdr r)])
      (set! closed? #t)
      (when progress-sema
	(semaphore-post progress-sema))
      (serve commit-reqs
	     (cons (choice-evt nack
			       (channel-put-evt ch (void)))
		   response-evts))))
  ;; Helper for reads and post-peek commits:
  (define (commit! k)
    (when progress-sema
      (semaphore-post progress-sema)
      (set! progress-sema #f))
    (set! n (+ n k)))
  ;; Start the server thread:
  (define server-thread (thread (lambda () (serve null null))))
  ;; ----------------------------------------
  ;; Client-side helpers:
  (define (req-evt f)
    (nack-guard-evt
     (lambda (nack)
       ;; Be sure that the server thread is running:
       (thread-resume server-thread (current-thread))
       ;; Create a channel to hold the reply:
       (let ([ch (make-channel)])
	 (f ch nack)
	 ch))))
  (define (read-or-peek-evt s skip peek? progress-evt)
    (req-evt (lambda (ch nack)
	       (channel-put read-req-ch (list* s skip ch nack peek? progress-evt)))))
  ;; Make the port:
  (make-input-port 'mod3-cycle
		   ;; Each handler for the port just sends
		   ;;  a request to the server
		   (lambda (s) (read-or-peek-evt s 0 #f #f))
		   (lambda (s skip progress-evt) (read-or-peek-evt s skip #t progress-evt))
		   (lambda () ; close
		     (sync (req-evt
			    (lambda (ch nack)
			      (channel-put progress-req-ch (list* ch nack))))))
		   (lambda () ; progress-evt
		     (sync (req-evt
			    (lambda (ch nack)
			      (channel-put progress-req-ch (list* ch nack))))))
		   (lambda (k progress-evt done-evt)  ; commit
		     (sync (req-evt
			    (lambda (ch nack)
			      (channel-put commit-req-ch
					   (list* k progress-evt done-evt ch nack))))))))

(for ([thread (in-list thread-procs)])
  (let ([mod3-cycle (make-mod3-cycle)])
    (port-progress-evt mod3-cycle)
    (let ([result1 #f]
          [result2 #f])
      (let ([t1 (thread (lambda ()
                          (set! result1 (read-string 5 mod3-cycle))))]
            [t2 (thread (lambda ()
                          (set! result2 (read-string 5 mod3-cycle))))])
        (thread-wait t1)
        (thread-wait t2)
        (test 11 string-length (string-append result1 "," result2))))
    (let ([s (make-bytes 1)]
          [progress-evt (port-progress-evt mod3-cycle)])
      (test 1 peek-bytes-avail! s 0 progress-evt mod3-cycle)
      (test #"1" values s)
      (test #t 
            port-commit-peeked 1 progress-evt (make-semaphore 1)
            mod3-cycle)
      (test #t evt? (sync/timeout 0 progress-evt))
      (test 0 peek-bytes-avail! s 0 progress-evt mod3-cycle)
      (test #f 
            port-commit-peeked 1 progress-evt (make-semaphore 1) 
            mod3-cycle))
    (close-input-port mod3-cycle)))

;; Non-byte port results:
(define infinite-voids
  (make-input-port
   'voids
   (lambda (s) (lambda args 'void))
   (lambda (skip s progress-evt) (lambda args 'void))
   void))
(err/rt-test (read-char infinite-voids) exn:application:mismatch?)
(test 'void read-byte-or-special infinite-voids)
(test 'void read-char-or-special infinite-voids)
(test 'void peek-char-or-special infinite-voids 0)
(test 'special peek-char-or-special infinite-voids 0 'special)
(let ([p (make-input-port
          'voids
          (lambda (s) (lambda args 'void))
          (lambda (skip s progress-evt) (lambda args (error "oops")))
          void)])
  (test 'special peek-char-or-special infinite-voids 0 'special)
  (test 'void read-char-or-special infinite-voids))
(let ([go
       (lambda (get-avail!)
	 (define (get)
	   (if (procedure-arity-includes? get-avail! 1)
	       (get-avail! (make-bytes 10) infinite-voids)
	       (get-avail! (make-bytes 10) 0 #f infinite-voids)))
	 (let ([p (get)])
	   (test #t procedure? p)
	   (test 4 procedure-arity p)
	   (test 'void p 'apple 1 0 1)
	   (err/rt-test (p 'apple 1 0 1) exn:fail:contract?))
	 (let ([p (get)])
	   (err/rt-test (p 'apple 0 0 1))
	   (err/rt-test (p 'apple 1 0 0))
	   (err/rt-test (p 'apple 1 -1 1))))])
  (go read-bytes-avail!)
  (go read-bytes-avail!*)
  (go read-bytes-avail!/enable-break)
  (go peek-bytes-avail!)
  (go peek-bytes-avail!*)
  (go peek-bytes-avail!/enable-break))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Also based on the Racket manual...

(define should-be-breakable? #t)

(define /dev/null-out
  (make-output-port 
   'null
   always-evt
   (lambda (s start end non-block? breakable?)
     (test should-be-breakable? 'breakable breakable?)
     (- end start))
   void
   (lambda (special non-block? breakable?) 
     (test should-be-breakable? 'spec-breakable breakable?)
     #t)
   (lambda (s start end) (wrap-evt
			  always-evt
			  (lambda (x)
			    (- end start))))
   (lambda (special) always-evt)))
(test (void) display "hello" /dev/null-out)
(set! should-be-breakable? #f)
(test 5 write-bytes-avail #"hello" /dev/null-out)
(set! should-be-breakable? #t)
(test #t write-special 'hello /dev/null-out)
(test 5 sync (write-bytes-avail-evt #"hello" /dev/null-out))
(set! should-be-breakable? #f)
(test 5 write-bytes-avail/enable-break #"hello" /dev/null-out)
(test #t write-special-avail* 'hello /dev/null-out)
(parameterize-break #f
  (set! should-be-breakable? #f)
  (test 5 write-bytes-avail/enable-break #"hello" /dev/null-out)
  (test #t write-special-avail* 'hello /dev/null-out)
  (test 5 write-bytes-avail #"hello" /dev/null-out))

(for ([pre? (in-list '(#f #t))])
  (let ()
    (define exe (find-exe))
    (define-values (sp stdout-in stdin-out stderr-in)
      (subprocess #f #f #f exe "-q" "-n"))
    (subprocess-wait sp)
    (when pre?
      ;; write some buffered bytes
      (write-bytes #"ok" stdin-out))
    ;; make sure `write-bytes-avail-evt` doesn't try to buffer
    (err/rt-test/once (sync (write-bytes-avail-evt #"hello" stdin-out))
                      exn:fail:filesystem?)))

;; A part that accumulates bytes as characters in a list,
;;  but not in a thread-safe way:
(define accum-list null)
(define accumulator/not-thread-safe
  (make-output-port 
   'accum/not-thread-safe
   always-evt
   (lambda (s start end non-block? breakable?)
     (set! accum-list
           (append accum-list
                   (map integer->char
                        (bytes->list (subbytes s start end)))))
     (- end start))
   void))
(display "hello" accumulator/not-thread-safe)
(test '(#\h #\e #\l #\l #\o) values accum-list)

;; Same as before, but with simple thread-safety:
(define accum-list null)
(define accumulator 
  (let* ([lock (make-semaphore 1)]
	 [lock-peek-evt (semaphore-peek-evt lock)])
    (make-output-port
     'accum
     lock-peek-evt
     (lambda (s start end non-block? breakable?)
       (let loop ()
	 (if (semaphore-try-wait? lock)
	     (begin
	       (set! accum-list
		     (append accum-list
			     (map integer->char
				  (bytes->list (subbytes s start end)))))
	       (semaphore-post lock)
	       (- end start))
	     ;; Cheap strategy: block until the list is unlocked,
	     ;;   then try again
	     (wrap-evt
	      lock-peek
	      (lambda (x) (loop))))))
     void)))
(display "hello" accumulator)
(test '(#\h #\e #\l #\l #\o) values accum-list)

;; A port that transforms data before sending it on
;;  to another port. Atomic writes exploit the
;;  underlying port's ability for atomic writes.
(define (make-latin-1-capitalize port)
  (define (byte-upcase s start end)
    (list->bytes
     (map (lambda (b) (char->integer
		       (char-upcase
			(integer->char b))))
	  (bytes->list (subbytes s start end)))))
  (make-output-port
   'byte-upcase
   ;; This port is ready when the original is ready:
   port
   ;; Writing procedure:
   (lambda (s start end non-block? breakable?)
     (let ([s (byte-upcase s start end)])
       (if non-block?
           (write-bytes-avail* s port)
           (begin
             (display s port)
             (bytes-length s)))))
   ;; Close procedure --- close original port:
   (lambda () (close-output-port port))
   #f
   ;; Write event:
   (and (port-writes-atomic? port)
	(lambda (s start end)
	  (write-bytes-avail-evt (byte-upcase s start end) port)))))
(define orig-port (open-output-string))
(define cap-port (make-latin-1-capitalize orig-port))
(display "Hello" cap-port)
(test "HELLO" get-output-string orig-port)
(test 3 sync (write-bytes-avail-evt #"Bye" cap-port))
(test "HELLOBYE" get-output-string orig-port)

;; Make sure output ports get immutable byte strings
(let ()
  (define i? #f)
  (define p (make-output-port
             'test
             always-evt
             (lambda (bstr start end buffer? enable-break?)
               (set! i? (immutable? bstr))
               (- end start))
             void
             (lambda (v buffer? enable-break?)
               1)
             (lambda (bstr start end)
               (set! i? (immutable? bstr))
               (wrap-evt always-evt (lambda (v) (- end start))))
             (lambda (v)
               (wrap-evt always-evt (lambda (v) 1)))))
  (test 5 write-bytes (bytes-copy #"hello") p)
  (test #t values i?)
  (set! i? #f)
  (test 5 write-bytes #"hello" p)
  (test #t values i?)
  (set! i? #f)
  (test #t evt? (write-bytes-avail-evt #"hello" p))
  (test #t values i?)
  (set! i? #f)
  (test #t evt? (write-bytes-avail-evt (bytes-copy #"hello") p))
  (test #t values i?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Peeking in a limited pipe extends the limit:

(let-values ([(in out) (make-pipe 3)])
  (test 3 write-bytes-avail #"12345" out)
  (test #f sync/timeout 0 out)
  (test #\1 peek-char in)
  (test out sync/timeout 0 out)
  (test 1 write-bytes-avail #"12345" out)
  (test #f sync/timeout 0 out)
  (test #\1 peek-char in)
  (test 0 write-bytes-avail* #"12345" out)
  (test #\2 peek-char in 1)
  (test 1 write-bytes-avail* #"12345" out)
  (let ([s (make-bytes 6 (char->integer #\-))])
    (test 5 read-bytes-avail! s in)
    (test #"12311-" values s))
  (test 3 values
        (let loop ([n 0])
          (define v (write-bytes-avail* #"1234" out))
          (if (zero? v)
              n
              (loop (+ n v))))))

;; Further test of peeking in a limited pipe (shouldn't get stuck):
(for ([thread (in-list thread-procs)])
  (let-values ([(i o) (make-pipe 50)]
               [(s) (make-semaphore)])
    (define t
      (thread (lambda ()
                (peek-bytes 100 0 i)
                (semaphore-wait s)
                (peek-bytes 200 0 i))))
    (display (make-bytes 100 65) o)
    (sync (system-idle-evt))
    (semaphore-post s)
    (display (make-bytes 100 66) o)
    (sync t)
    (test 'not-stuck values 'not-stuck)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide a location proc:

(let ([mk
       (lambda (adjust-locs)
	 (let ([p (open-input-string "Hello\n\n world")])
	   (test #f port-counts-lines? p)
	   (port-count-lines! p)
	   (test #t port-counts-lines? p)
	   (let ([p2 (make-input-port 'with-loc
				      (lambda (s) (read-bytes-avail! s p))
				      (lambda (s skip progress-evt)
					(peek-bytes-avail! s skip progress-evt p))
				      void
				      (lambda () (prot-progress-evt p))
				      (lambda (k progress-evt done-evt)
					(port-commit-peeked k progress-evt done-evt p))
				      (lambda ()
					(let-values ([(line col pos) (port-next-location p)])
					  (adjust-locs line col pos))))])
	     (port-count-lines! p2)
	     p2)))])
  (let ([plain (let ([p (open-input-string "Hello\n\n world 1\n2")])
		 (port-count-lines! p)
		 p)]
	[double (mk (lambda (l c p)
		      (values (* 2 l) (* 2 c) (* 2 p))))]
	[none (mk (lambda (l c p) (values #f #f #f)))]
	[bad (mk (lambda (l c p) #f))])
    (test-values '(1 0 1) (lambda () (port-next-location plain)))
    (test-values '(2 0 2) (lambda () (port-next-location double)))
    (test-values '(#f #f #f) (lambda () (port-next-location none)))
    (err/rt-test (port-next-location bad) exn:fail:contract:arity?)
    
    (test 'Hello read plain)
    (test 'Hello read double)
    (test 'Hello read none)
    (err/rt-test (read bad) exn:fail:contract:arity?)

    (test-values '(1 5 6) (lambda () (port-next-location plain)))
    (test-values '(2 10 12) (lambda () (port-next-location double)))
    (test-values '(#f #f #f) (lambda () (port-next-location none)))
    (err/rt-test (port-next-location bad))

    ;; `set-port-next-location!' should have no effect on custom ports:
    (set-port-next-location! double 1 1 1)
    (test-values '(2 10 12) (lambda () (port-next-location double)))
    (set-port-next-location! none 1 1 1)
    (test-values '(#f #f #f) (lambda () (port-next-location none)))
    (set-port-next-location! bad 1 1 1)
    (err/rt-test (port-next-location bad))

    (let ([stx (read-syntax #f plain)])
      (test 3 syntax-line stx)
      (test 1 syntax-column stx)
      (test 9 syntax-position stx))
    (let ([stx (read-syntax #f double)])
      (test 6 syntax-line stx)
      ;; The next two should be 2 and 18, but the reader
      ;;  actually reads the character first, the subtracts
      ;;  1 from the column and position.
      (test 3 syntax-column stx)
      (test 19 syntax-position stx))
    (let ([stx (read-syntax #f none)])
      (test #f syntax-line stx)
      (test #f syntax-column stx)
      (test #f syntax-position stx))
    (err/rt-test (read-syntax #f bad) exn:fail:contract:arity?)
    
    (test-values '(3 6 14) (lambda () (port-next-location plain)))
    (test-values '(6 12 28) (lambda () (port-next-location double)))
    (test-values '(#f #f #f) (lambda () (port-next-location none)))

    ;; Check `set-port-next-location!':
    (set-port-next-location! plain 100 13 1023)
    (test-values '(100 13 1023) (lambda () (port-next-location plain)))
    (let ([stx (read-syntax #f plain)])
      (test 100 syntax-line stx)
      (test 14 syntax-column stx)
      (test 1024 syntax-position stx))
    (let ([stx (read-syntax #f plain)])
      (test 101 syntax-line stx)
      (test 0 syntax-column stx)
      (test 1026 syntax-position stx))))

;; Test provided by @wcs4217
(let ([p (open-input-bytes #"\rx\ny")])
  (port-count-lines! p)
  (test-values '(1 0 1) (lambda () (port-next-location p)))
  (test #\return read-char p)
  (test-values '(2 0 2) (lambda () (port-next-location p)))
  (test #\x read-char p)
  (test-values '(2 1 3) (lambda () (port-next-location p)))
  (test #\newline read-char p)
  (test-values '(3 0 4) (lambda () (port-next-location p)))
  (test #\y read-char p)
  (test-values '(3 1 5) (lambda () (port-next-location p)))
  (test eof read-char p)
  (test-values '(3 1 5) (lambda () (port-next-location p))))

(let ([p (open-input-bytes #"\rx\ny")])
  (port-count-lines! p)
  (test-values '(1 0 1) (lambda () (port-next-location p)))
  (test #"\rx" read-bytes 2 p)
  (test-values '(2 1 3) (lambda () (port-next-location p)))
  (test #\newline read-char p)
  (test-values '(3 0 4) (lambda () (port-next-location p)))
  (test #\y read-char p)
  (test-values '(3 1 5) (lambda () (port-next-location p)))
  (test eof read-char p)
  (test-values '(3 1 5) (lambda () (port-next-location p))))

;; more port location tessting
(let ()
  (define-values (i o) (make-pipe))
  (port-count-lines! i)
  (port-count-lines! o)
  (define (next-location p)
    (define-values (line col pos) (port-next-location p))
    (list line col pos))
  (test '(1 0 1) next-location i)
  (test '(1 0 1) next-location o)

  (write-bytes #"a" o)
  (test '(1 1 2) next-location o)
  (write-bytes #"\n b" o)
  (test '(2 2 5) next-location o)

  (test #"a" read-bytes 1 i)
  (test '(1 1 2) next-location i)
  (test #"\n" read-bytes 1 i)
  (test '(2 0 3) next-location i)
  (test #" b" read-bytes 2 i)
  (test '(2 2 5) next-location i)

  (write-bytes #"x\r" o)
  (test '(3 0 7) next-location o)
  (write-bytes #"\n" o)
  (test '(3 0 7) next-location o)
  (write-bytes #"!" o)
  (test '(3 1 8) next-location o)

  (test #"x\r" read-bytes 2 i)
  (test '(3 0 7) next-location i)
  (test #"\n!" read-bytes 2 i)
  (test '(3 1 8) next-location i)

  (write-string "e\u300" o)
  (test '(3 3 10) next-location o)
  (test #"e" read-bytes 1 i)
  (test '(3 2 9) next-location i)
  (test #"\314" read-bytes 1 i)
  (test '(3 3 10) next-location i) ; tentatively incremented mid-UTF-8
  (test #"\200" read-bytes 1 i)
  (test '(3 3 10) next-location i)  ; UTF-8 concluded

  (write-string "!" o)
  (test '(3 4 11) next-location o)
  (test #"!" read-bytes 1 i)
  (test '(3 4 11) next-location i)

  (write-string "\r" o)
  (test '(4 0 12) next-location o)
  (test #"\r" read-bytes 1 i)
  (test '(4 0 12) next-location i)

  (write-string "\n" o)
  (test '(4 0 12) next-location o)
  (test #"\n" read-bytes 1 i)
  (test '(4 0 12) next-location i)

  (write-string "." o)
  (test '(4 1 13) next-location o)
  (test #"." read-bytes 1 i)
  (test '(4 1 13) next-location i)

  (write-string "app\u03BBe" o)
  (test '(4 6 18) next-location o)
  (test "app\u03BBe" read-string 5 i)
  (test '(4 6 18) next-location i)

  (void))

(let ()
  (define i (open-input-string "\u0019\u0000\u000E"))
  (port-count-lines! i)
  (define (next-location p)
    (define-values (line col pos) (port-next-location p))
    (list line col pos))

  (test '(1 0 1) next-location i)
  (test "\u0019\u0000\u000E" read-string 3 i)
  (test '(1 3 4) next-location i))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Check that if the initial commit thread is killed, then
;;  another commit thread is broken, that the second doesn't
;;  assume that the initial commit thread is still there:

(for ([thread (in-list thread-procs)])
  (define-values (r w) (make-pipe))
  (define ch (make-channel))
  (display "hi" w)
  (peek-byte r)
  (let ([t (thread (lambda ()
		     (port-commit-peeked 1 (port-progress-evt r) ch r)))])
    (sync (system-idle-evt))
    (let ([t2
	   (thread (lambda ()
		     (port-commit-peeked 1 (port-progress-evt r) ch r)))])
      (sync (system-idle-evt))
      (test #t thread-running? t)
      (test #t thread-running? t2)
      (thread-suspend t2)
      (break-thread t2)
      (kill-thread t)
      (thread-resume t2)
      (sleep)))
  (test (char->integer #\h) peek-byte r))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Check that breaks are enabled properly:

(let ([try
       (lambda (read-char)
	 (let ([p (make-input-port
		   'test
		   (lambda (bstr) never-evt)
		   (lambda (bstr skip-count progress-evt) never-evt)
		   void)])
	   (let ([t (thread (lambda () (with-handlers ([exn:break? void])
					 (read-char p))))])
	     (sync (system-idle-evt))
	     (break-thread t)
	     (sync (system-idle-evt))
	     (test #f thread-running? t))))])
  (try sync)
  (try sync/enable-break)
  (parameterize-break #f (try sync/enable-break))
  (try read-char)
  (try peek-char)
  (try (lambda (x) (read-bytes-avail! (make-bytes 10) x)))
  (try (lambda (x) (read-bytes-avail!/enable-break (make-bytes 10) x)))
  (parameterize-break 
   #f 
   (try (lambda (x) (read-bytes-avail!/enable-break (make-bytes 10) x)))))

;; Check nonblock? and break? interaction (again):
(let ()
  (define status '())
  (define p (make-output-port
             'p
             always-evt
             (lambda (bstr start end nonblock? break?)
               (set! status (list nonblock? break? (break-enabled)))
               (- end start))
             void
             (lambda (v nonblock? break?)
               (set! status (list 'special nonblock? break? (break-enabled)))
               #t)))
  (write-bytes #"hi" p)
  (test '(#f #t #f) values status)
  (parameterize-break
   #f
   (write-bytes #"hi" p))
  (test '(#f #f #f) values status)
  (write-bytes-avail #"hi" p)
  (test '(#t #f #f) values status)
  (write-bytes-avail* #"hi" p)
  (test '(#t #f #f) values status)
  (write-special 'any p)
  (test '(special #f #t #f) values status)
  (write-special-avail* 'any p)
  (test '(special #t #f #f) values status))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check port shortcuts for `make-input-port' and `make-output-port'

(for ([thread (in-list thread-procs)])
  (let-values ([(i o) (make-pipe 5)])
    (define i2 (make-input-port
                (object-name i)
                i
                i
                void))
    (define o2 (make-output-port
                (object-name o)
                o
                o
                void))
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
      (test #"6789a\0" values s))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that an uncooperative output port doesn't keep breaks
;; disabled too long:

(for ([thread (in-list thread-procs)])
  (test 'ok
        'stuck-port
        (let ([p (make-output-port 'stumper
                                   always-evt
                                   (lambda args
                                     never-evt)
                                   void)]
              [t (current-thread)])
          (thread (lambda () 
                    (sync (system-idle-evt))
                    (break-thread t)))
          (with-handlers ([exn:break? (lambda (exn) 'ok)])
            (write-byte 0 p)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test port-commit-peeked and position counting

(let ([check
       (lambda (in [d 0] [first-three-bytes #"123"] [char-len 3])
         (test d file-position in)
         (test d file-position* in)
         (let-values ([(l c p) (port-next-location in)])
           (test p add1 d)
           (test first-three-bytes peek-bytes 3 0 in)
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
    (let ()
      (define s (open-input-string "12345"))
      (count-lines! s)
      (check s))
    (let ()
      (define s (open-input-string "012345"))
      (count-lines! s)
      (read-byte s)
      (check s 1))
    (let ()
      (define s (open-input-string "1\u03BB45"))
      (count-lines! s)
      (check s 0 (string->bytes/utf-8 "1\u3BB") 2))
    (let ()
      (define-values (in out) (make-pipe))
      (display "12345" out)
      (count-lines! in)
      (check in))
    (let ()
      (call-in-temporary-directory
       (lambda ()
         (with-output-to-file "tmp8" 
           #:exists 'truncate/replace
           (lambda () (display "12345")))
         (define in (open-input-file "tmp8"))
         (count-lines! in)
         (check in)
         (close-input-port in)
         (delete-file "tmp8")))))
  (check-all void)
  (check-all port-count-lines!))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; attempting to read from or write to a closed byte-string port

(let ()
  (define (check proc)
    (define p (open-input-bytes #"x"))
    (close-input-port p)
    (err/rt-test (proc p) exn:fail? #rx"closed")
    (err/rt-test (proc p)  (lambda (e) (not (exn:fail:contract? e)))))
  (check read-byte)
  (check peek-byte)
  (check (lambda (p) (peek-byte p 10)))
  (check (lambda (p) (read-bytes 10 p)))
  (check read-char)
  (check read-char-or-special)
  (check peek-char)
  (check (lambda (p) (read-string 10 p)))
  (check read)
  (check (lambda (p) (read-syntax (object-name p) p))))

(let ()
  (define (check proc)
    (define p (open-output-bytes))
    (close-output-port p)
    (err/rt-test (proc p) exn:fail? #rx"closed")
    (err/rt-test (proc p) (lambda (e) (not (exn:fail:contract? e)))))
  (check (lambda (p) (write-byte 10 p)))
  (check (lambda (p) (write-bytes #"hello" p)))
  (check (lambda (p) (write-char #\x p)))
  (check (lambda (p) (write-string "hello" p)))
  (check flush-output))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; port-closed events

(for ([thread (in-list thread-procs)])
  (define-values (i o) (make-pipe))
  (define ic (port-closed-evt i))
  (define oc (port-closed-evt o))
  (test #f sync/timeout 0 ic oc)
  (define t (thread (lambda () (sync ic))))
  (sync (system-idle-evt))
  (test #f sync/timeout 0 ic oc t)
  (close-input-port i)
  (test t sync t)
  (test ic sync/timeout 0 ic oc)
  (test #f sync/timeout 0 oc)
  (define t2 (thread (lambda () (sync oc))))
  (sync (system-idle-evt))
  (test #f sync/timeout 0 oc t2)
  (close-output-port o)
  (test t2 sync t2)
  (test oc sync/timeout 0 oc))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eof should not advance port position

(let ()
  (define (check read-byte)
    (define-values (i o) (make-pipe))
    (test 0 file-position i)
    (write-byte 10 o)
    (close-output-port o)
    (test 0 file-position i)
    (test 10 read-byte i)
    (test 1 file-position i)
    (test eof read-byte i)
    (test 1 file-position i))
  (check read-byte)
  (check (lambda (i) 
           (define c (read-char i))
           (if (char? c)
               (char->integer c)
               c))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ([p (open-input-bytes #"123")]
       [p2 (make-input-port
            (object-name p)
            p
            p
            void
            #f #f #f void
            #f)])
  (test #f file-position* p2)
  (test #\1 read-char p2)
  (test #f file-position* p2)
  (err/rt-test (file-position p2) exn:fail:filesystem?))

(let ([i (open-input-bytes #"")])
  (test i sync/timeout 0 i)
  (test i sync/timeout #f i)
  (test #t byte-ready? i)
  (test #t char-ready? i))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode, file positions, and buffers

(let ()
  (define path (make-temporary-file "test~a.txt"))

  (define ofile (open-output-file path #:mode 'text #:exists 'replace))
  (fprintf ofile "abc\ndef\nghi\n")
  (test 'block file-stream-buffer-mode ofile)
  (test (void) file-stream-buffer-mode ofile 'line)
  (test 'line file-stream-buffer-mode ofile)
  (close-output-port ofile)

  (test (if (eq? 'windows (system-type))
            #"abc\r\ndef\r\nghi\r\n"
            #"abc\ndef\nghi\n")
        call-with-input-file path (lambda (i) (read-bytes 100 i)))

  (for ([i '(none block)])
    (define ifile (open-input-file path #:mode 'text))
    (file-stream-buffer-mode ifile)
    (test "abc" read-line ifile)
    (define pos (file-position ifile))
    (test "def" read-line ifile)
    (file-position ifile pos)
    (test "def" read-line ifile)
    (close-input-port ifile))

  (let* ([bs (call-with-input-file path
	       #:mode 'text 
	       (lambda (p) (read-bytes (file-size path) p)))])
    ;; Check text-mode conversion at every boundary:
    (for ([i (in-range (add1 (bytes-length bs)))])
	 (define p (open-input-file path #:mode 'text))
	 (file-stream-buffer-mode p 'none)
	 (define a (read-bytes i p))
	 (define b (read-bytes (- (bytes-length bs) i) p))
	 (test #t eof-object? (read-byte p))
	 (close-input-port p)
	 (test bs bytes-append a b)))

  (delete-file path))

;; Check `file-position`, OS-level pipes, and peek
(when (and (memq (system-type) '(unix macosx))
           (file-exists? "/bin/cat"))
  (define-values (sp stdout-in stdin-out stderr-in) (subprocess #f #f #f "/bin/cat"))
  (write-bytes #"abcd\n" stdin-out)
  (close-output-port stdin-out)
  (test 0 file-position stdout-in)
  (test #"abc" peek-bytes 3 0 stdout-in)
  (test 0 file-position stdout-in)
  (test #\a read-char stdout-in)
  (test 1 file-position stdout-in)
  (close-input-port stdout-in)
  (close-input-port stderr-in)
  (subprocess-wait sp))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check reader error-message formatting for a struct port

(let ()
  (define-struct wrapper (other port)
    #:property prop:input-port 1)
  (err/rt-test
   (read (wrapper #f
                  (make-input-port "wrapped"
                                   (lambda (bstr)
                                     (bytes-set! bstr 0 (char->integer #\)))
                                     1)
                                   (lambda (bstr d evt)
                                     (bytes-set! bstr 0 (char->integer #\)))
                                     1)
                                   void)))
   exn:fail:read?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check whether `read` counts correctly
;; (because `read` may cheat internally with a private "unget")

(let ()
  (define p 
    (let ([in (open-input-string "(…)abcdef")])
      (make-input-port
       "unicode"
       (lambda (s)
         (read-bytes-avail!* s in))
       (lambda (s skip default)
         (peek-bytes-avail!* s skip #f in))
       void
       #f
       #f
       #f
       void
       1)))

  (test 0 file-position p)
  (void (read p))
  (test 5 file-position p)
  (test  "abcde" read-string 5 p))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that lines, columns, and positions stay at #f when set to #f

(let ()
  (define (check-srcloc line col pos)
    (define stx-port (open-input-string "A\n B"))
    (port-count-lines! stx-port)
    (set-port-next-location! stx-port line col pos)
    (define a (read-syntax #f stx-port))
    (define b (read-syntax #f stx-port))
    (test line syntax-line a)
    (test col syntax-column a)
    (test pos syntax-position a)
    (test (and line (add1 line)) syntax-line b)
    (test (and col 1) syntax-column b)
    (test (and pos (+ 3 pos)) syntax-position b))
  (check-srcloc #f #f #f)
  (check-srcloc #f #f 29)
  (check-srcloc 1 #f 29)
  (check-srcloc #f 3 29)
  (check-srcloc #f 3 #f)
  (check-srcloc 1 3 29))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression test for interaction of `read-line`, "\r\n", and
;; input buffering

(let ()
  (define f1 (make-temporary-file "line-feed1~a.txt"))
  (define f2 (make-temporary-file "line-feed2~a.txt"))

  (define p1 (open-output-file f1 #:exists 'truncate))
  (define p2 (open-output-file f2 #:exists 'truncate))

  (define (write-prefix p)
    (for ([i 1364])
      (write-bytes #"x\r\n" p)))

  (write-prefix p1)
  (write-prefix p2)
  (write-bytes #"\r\ny\r\ny\r\n" p1)
  (write-bytes #"y\r\ny\r\ny\r\n" p2)

  (close-output-port p1)
  (close-output-port p2)

  (define (count f)
    (call-with-input-file f
                          (lambda (in)
                            (let loop ()
                              (define v (read-line in 'any))
                              (if (eof-object? v)
                                  0
                                  (add1 (loop)))))))

  (test 1367 count f1)
  (test 1367 count f2)

  (delete-file f1)
  (delete-file f2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression test for peek-string! issue #4156.

(let ()
  (define in (open-input-string "hello"))
  (define str (make-string 10))
  (test 5 peek-string! str 0 in)
  (test "hello" substring str 0 5))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(parameterize ([current-input-port (open-input-nowhere)])
  (test eof read)
  (test eof read-char)
  (test eof read-byte)
  (test eof read-line)
  (test eof read-char-or-special))

(test 'nowhere object-name (open-input-nowhere))
(test 'apple   object-name (open-input-nowhere 'apple))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for ([poll-proc (in-list (list
                           (lambda (i)
                             (byte-ready? i))
                           (lambda (i)
                             (peek-bytes-avail!* (make-bytes 1) 0 #f i))))])
  (define peeked? #f)
  (define polled? #f)
  (define i
    (make-input-port
     'test
     (lambda (bstr)
       never-evt)
     (lambda (bstr skip evt)
       (set! peeked? #t)
       (poll-guard-evt
        (lambda (poll?)
          (when poll?
            (set! polled? #t))
          (wrap-evt always-evt (lambda (v) 0)))))
     void))
  ;; should trigger a poll on an evt:
  (poll-proc i)
  (test #t values peeked?)
  (test #t values polled?))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
