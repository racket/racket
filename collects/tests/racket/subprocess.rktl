
(load-relative "testing.rktl")

(require mzlib/process)

(Section 'subprocess)

(define self
  (parameterize ([current-directory (find-system-path 'orig-dir)])
    (find-executable-path (find-system-path 'exec-file) #f)))
(define cat (find-executable-path 
	     (if (eq? 'windows (system-type)) 
		 "cat.exe"
		 "cat")
	     #f))
(define tmpfile (build-path (find-system-path 'temp-dir) "cattmp"))
(define tmpfile2 (build-path (find-system-path 'temp-dir) "cattmp2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process* tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Simple `process' tests using "cat"

(let ([p (process* cat)])
  (fprintf (cadr p) "Hello~n")
  (close-output-port (cadr p))
  (test "Hello" read-line (car p))
  (test eof read-line (car p))
  (test eof read-line (cadddr p))

  ((list-ref p 4) 'wait)
  (test 'done-ok (list-ref p 4) 'status)

  (close-input-port (car p))
  (close-input-port (cadddr p)))

;; Generate output to stderr as well as stdout

(let ([p (process* cat "-" "nosuchfile")])
  (fprintf (cadr p) "Hello~n")
  (close-output-port (cadr p))
  (test "Hello" read-line (car p))
  (test eof read-line (car p))
  (test '("nosuchfile")
	regexp-match "nosuchfile" (read-line (cadddr p)))
  (test eof read-line (cadddr p))

  ((list-ref p 4) 'wait)
  (test 'done-error (list-ref p 4) 'status)

  (close-input-port (car p))
  (close-input-port (cadddr p)))

;; Supply file for stdout

(let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
  (let ([p (process*/ports f #f #f cat)])
    (test #f car p)

    (fprintf (cadr p) "Hello~n")
    (close-output-port (cadr p))
    (test eof read-line (cadddr p))

    ((list-ref p 4) 'wait)
    (test 'done-ok (list-ref p 4) 'status)

    (close-output-port f)
    (test 6 file-size tmpfile)
    (test 'Hello with-input-from-file tmpfile read)

    (close-input-port (cadddr p))))

;; Supply file for stdout & stderr, only stdout writes

(let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
  (let ([p (process*/ports f #f f cat)])
    (test #f car p)
    (test #f cadddr p)

    (fprintf (cadr p) "Hello~n")
    (close-output-port (cadr p))

    ((list-ref p 4) 'wait)
    (test 'done-ok (list-ref p 4) 'status)

    (close-output-port f)
    (test 6 file-size tmpfile)
    (test 'Hello with-input-from-file tmpfile read)))

;; Supply file for stderr

(let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
  (let ([p (process*/ports #f #f f cat "nosuchfile")])
    (test #f cadddr p)

    (close-output-port (cadr p))

    (test eof read-line (car p))
    
    ((list-ref p 4) 'wait)
    (test 'done-error (list-ref p 4) 'status)

    (close-output-port f)
    (test '("nosuchfile")
	  regexp-match "nosuchfile" (with-input-from-file tmpfile
				      read-line))
    
    (close-input-port (car p))))

;; Supply file for stdout & stderr, only stderr writes

(let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
  (let ([p (process*/ports f #f f cat "nosuchfile")])
    (test #f car p)
    (test #f cadddr p)

    (close-output-port (cadr p))

    ((list-ref p 4) 'wait)
    (test 'done-error (list-ref p 4) 'status)
    
    (close-output-port f)
    (test '("nosuchfile")
	  regexp-match "nosuchfile" (with-input-from-file tmpfile
				      read-line))))

;; Supply file for stdout & stderr, both write

(let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
  (let ([p (process*/ports f #f f cat "-" "nosuchfile")])
    (test #f car p)
    (test #f cadddr p)

    (fprintf (cadr p) "First line~n")
    (close-output-port (cadr p))

    ((list-ref p 4) 'wait)
    (test 'done-error (list-ref p 4) 'status)
    
    (close-output-port f)
    (with-input-from-file tmpfile
      (lambda ()
	(test "First line" read-line)
	(test '("nosuchfile") regexp-match "nosuchfile" (read-line))
	(test eof read-line)))))

;; Supply separate files for stdout & stderr

(let ([f (open-output-file tmpfile #:exists 'truncate/replace)]
      [f2 (open-output-file tmpfile2 #:exists 'truncate/replace)])
  (let ([p (process*/ports f #f f2 cat "-" "nosuchfile")])
    (test #f car p)
    (test #f cadddr p)

    (fprintf (cadr p) "The line~n")
    (close-output-port (cadr p))

    ((list-ref p 4) 'wait)
    (test 'done-error (list-ref p 4) 'status)
    
    (close-output-port f)
    (close-output-port f2)

    (with-input-from-file tmpfile
      (lambda ()
	(test "The line" read-line)
	(test eof read-line)))
    
    (with-input-from-file tmpfile2
      (lambda ()
	(test '("nosuchfile") regexp-match "nosuchfile" (read-line))
	(test eof read-line)))))

;; Supply file for stdin

(let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
  (fprintf f "Howdy~n")
  (close-output-port f))
(let ([f (open-input-file tmpfile)])
  (let ([p (process*/ports #f f #f cat)])
    (test #f cadr p)

    (test "Howdy" read-line (car p))
    (test eof read-line (car p))
    (test eof read-line (cadddr p))

    (close-input-port f)

    (close-input-port (car p))
    (close-input-port (cadddr p))))

;; Files for everyone

(let ([f (open-input-file tmpfile)]
      [f2 (open-output-file tmpfile2 #:exists 'truncate/replace)])
  (let ([p (process*/ports f2 f f2 cat "-" "nosuchfile")])
    (test #f car p)
    (test #f cadr p)
    (test #f cadddr p)

    ((list-ref p 4) 'wait)
    (test 'done-error (list-ref p 4) 'status)
    
    (close-input-port f)
    (close-output-port f2)

    (with-input-from-file tmpfile2
      (lambda ()
	(test "Howdy" read-line)
	(test '("nosuchfile") regexp-match "nosuchfile" (read-line))))))

;; strings for everyone

(let ([f (open-input-string (string-append
			     "1"
			     (make-string 50000 #\0)
			     "\n"))]
      [f2 (open-output-string)])
  (let ([p (process*/ports f2 f f2 cat "-" "nosuchfile")])
    (test #f car p)
    (test #f cadr p)
    (test #f cadddr p)

    ((list-ref p 4) 'wait)
    (test 'done-error (list-ref p 4) 'status)
    
    (let ([p (open-input-string (get-output-string f2))])
      (test (expt 10 50000) read p)
      (test "" read-line p)
      (test '("nosuchfile") regexp-match "nosuchfile" (read-line p)))))

;; Check error cases

(let ([f (open-input-file tmpfile)]
      [f2 (open-output-file tmpfile2 #:exists 'truncate/replace)])

  (let ([test
	 (lambda (o i e)
	   (err/rt-test (process*/ports o i e cat)))])
    (test f #f #f)
    (test #f f2 #f)
    (test #f #f f)

    (test f f f2)
    (test f2 f2 f2)
    (test f2 f f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nested tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (r w id e f)
  (apply values (process* self
			  "-e" 
			  "(let loop () (unless (eof-object? (eval (read))) (loop)))")))

(define (test-line out in)
  (fprintf w "~a~n" in)
  (flush-output w)
  (when out
    (test out (lambda (ignored) (read-line r)) in)))

(test-line "17" "(display 17) (newline) (flush-output)")

(close-input-port r)
(close-input-port e)
(close-output-port w)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custodians
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ([try
       (lambda (post-shutdown)
         (let ([c (make-custodian)])
           (let ([l (parameterize ([current-custodian c])
                      (process* self
                                "-e"
                                "(let loop () (loop))"))])
             (test 'running (list-ref l 4) 'status)
             (custodian-shutdown-all c)
             (sleep 0.1)
             (test post-shutdown (list-ref l 4) 'status)
             ((list-ref l 4) 'kill))))])
  (try 'running)
  (parameterize ([current-subprocess-custodian-mode 'kill])
    (try 'done-error))
  (parameterize ([current-subprocess-custodian-mode 'interrupt])
    (try (if (eq? 'windows (system-type)) 'running 'done-error))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; process groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (eq? 'windows (system-type))
  (let ([try
         (lambda (post-shutdown?)
           (let ([l (parameterize ([subprocess-group-enabled (not post-shutdown?)])
                      (process* self
                                "-e"
                                (format "(define l (process* \"~a\" \"-e\" \"(let loop () (loop))\"))" self)
                                "-e"
                                "(displayln (list-ref l 2))"
                                "-e"
                                "(flush-output)"
                                "-e"
                                "(let loop () (loop))"))]
                 [running? (lambda (sub-pid)
                             (equal?
                              (list (number->string sub-pid))
                              (regexp-match
                               (format "(?m:^~a(?=[^0-9]))" sub-pid)
                               (let ([s (open-output-string)])
                                 (parameterize ([current-output-port s])
                                   (system (format "ps x")))
                                 (get-output-string s)))))])
             (let ([sub-pid (read (car l))])
               (test 'running (list-ref l 4) 'status)
               (test #t running? sub-pid)
               ((list-ref l 4) 'kill)
               ((list-ref l 4) 'wait)
               (test 'done-error (list-ref l 4) 'status)
               (test post-shutdown? running? sub-pid)
               (when post-shutdown?
                 (system (format "kill ~a" sub-pid))))))])
    (try #t)
    (try #f)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
