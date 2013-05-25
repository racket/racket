
(load-relative "testing.rktl")

(require racket/system
         racket/file)

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
  (fprintf (cadr p) "Hello\n")
  (close-output-port (cadr p))
  (test "Hello" read-line (car p))
  (test eof read-line (car p))
  (test eof read-line (cadddr p))

  ((list-ref p 4) 'wait)
  (test 'done-ok (list-ref p 4) 'status)

  (close-input-port (car p))
  (close-input-port (cadddr p)))

;; Generate output to stderr as well as stdout

(define (nosuchfile-test dash nosuchfile)
  (let ([p (process* cat dash nosuchfile)])
    (fprintf (cadr p) "Hello\n")
    (close-output-port (cadr p))
    (test "Hello" read-line (car p))
    (test eof read-line (car p))
    (test '("nosuchfile")
          regexp-match "nosuchfile" (read-line (cadddr p)))
    (test eof read-line (cadddr p))

    ((list-ref p 4) 'wait)
    (test 'done-error (list-ref p 4) 'status)

    (close-input-port (car p))
    (close-input-port (cadddr p))))
(nosuchfile-test "-" "nosuchfile")
(nosuchfile-test #"-" (string->path "nosuchfile"))
(nosuchfile-test (string->path "-") "nosuchfile")

;; Redirect stderr to stdout:

(let ([p (process*/ports #f #f 'stdout cat "-" "nosuchfile")])
  (test #t file-stream-port? (car p))
  (test #t file-stream-port? (cadr p))
  (test #f cadddr p)

  (fprintf (cadr p) "Hello\n")
  (close-output-port (cadr p))
  (test "Hello" read-line (car p))
  (test '("nosuchfile")
        regexp-match "nosuchfile" (read-line (car p)))
  (test eof read-line (car p))

  ((list-ref p 4) 'wait)
  (test 'done-error (list-ref p 4) 'status)
  
  (close-input-port (car p)))

;; Supply file for stdout

(let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
  (let ([p (process*/ports f #f #f cat)])
    (test #f car p)

    (fprintf (cadr p) "Hello\n")
    (close-output-port (cadr p))
    (test eof read-line (cadddr p))

    ((list-ref p 4) 'wait)
    (test 'done-ok (list-ref p 4) 'status)

    (close-output-port f)
    (test 6 file-size tmpfile)
    (test 'Hello with-input-from-file tmpfile read)

    (close-input-port (cadddr p))))

;; Supply file for stdout & stderr, only stdout writes

(define (stderr-to-stdout-test stderr-filter)
  (let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
    (let ([p (process*/ports f #f (stderr-filter f) cat)])
      (test #f car p)
      (test #f cadddr p)

      (fprintf (cadr p) "Hello\n")
      (close-output-port (cadr p))

      ((list-ref p 4) 'wait)
      (test 'done-ok (list-ref p 4) 'status)

      (close-output-port f)
      (test 6 file-size tmpfile)
      (test 'Hello with-input-from-file tmpfile read))))
(stderr-to-stdout-test values)
(stderr-to-stdout-test (lambda (x) 'stdout))

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

    (fprintf (cadr p) "First line\n")
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

    (fprintf (cadr p) "The line\n")
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
  (fprintf f "Howdy\n")
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

(let ([all-strings
       (lambda (stderr-is-stdout?)
         (let ([f (open-input-string (string-append
                                      "1"
                                      (make-string 50000 #\0)
                                      "\n"))]
               [f2 (open-output-string)]
               [f3 (if stderr-is-stdout?
                       'stdout
                       (open-output-string))])
           (let ([p (process*/ports f2 f f3 cat "-" "nosuchfile")])
             (test #f car p)
             (test #f cadr p)
             (test #f cadddr p)

             ((list-ref p 4) 'wait)
             (test 'done-error (list-ref p 4) 'status)
             
             (let ([p (open-input-string (get-output-string f2))])
               (test (expt 10 50000) read p)
               (test "" read-line p)
               (let ([p (if (eq? f3 'stdout)
                            p
                            (open-input-string (get-output-string f3)))])
                 (test '("nosuchfile") regexp-match "nosuchfile" (read-line p)))))))])
  (all-strings #t)
  (all-strings #f))

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

;; system* ------------------------------------------------------

(let ([out (open-output-string)])
  (test #t 'system*
        (parameterize ([current-input-port (open-input-string "Hi\n")]
                       [current-output-port out])
          (system* cat "-")))
  (test "Hi\n" get-output-string out))

(let ([out (open-output-string)])
  (test 0 'system*/exit-code
        (parameterize ([current-input-port (open-input-string "Hi\n")]
                       [current-output-port out])
          (system*/exit-code cat "-")))
  (test "Hi\n" get-output-string out))

;; shells ------------------------------------------------------

(let ([go
       (lambda (path->string)
         (let ([p (process (path->string cat))])
           (fprintf (cadr p) "Hi\n")
           (close-output-port (cadr p))
           (test "Hi" read-line (car p) 'any)
           (close-input-port (car p))
           (close-input-port (cadddr p))
           ((list-ref p 4) 'wait)
           (test 'done-ok (list-ref p 4) 'status)))])
  (go path->string)
  (go path->bytes))

(let ([p (process/ports #f (open-input-string "Hi\n") 'stdout (path->string cat))])
  (test "Hi" read-line (car p) 'any)
  (close-input-port (car p))
  (test #f cadddr p)
  ((list-ref p 4) 'wait)
  (test 'done-ok (list-ref p 4) 'status))

(let ([out (open-output-string)])
  (test #t 'system
        (parameterize ([current-input-port (open-input-string "Hi\n")]
                       [current-output-port out])
          (system (path->string cat))))
  (test "Hi\n" get-output-string out))

(let ([out (open-output-string)])
  (test 0 'system
        (parameterize ([current-input-port (open-input-string "Hi\n")]
                       [current-output-port out])
          (system/exit-code (path->string cat))))
  (test "Hi\n" get-output-string out))

;; empty strings and nul checks ------------------------------------------------------

(err/rt-test (subprocess #f #f #f ""))
(err/rt-test (process* ""))
(err/rt-test (system* ""))

(let ([no-nuls (lambda (thunk)
                 (err/rt-test (thunk) (lambda (exn)
                                        (regexp-match? #rx"bytes-no-nuls[?]" (exn-message exn)))))])
  (no-nuls (lambda () (subprocess #f #f #f cat "\0")))
  (no-nuls (lambda () (subprocess #f #f #f cat #"\0")))
  (no-nuls (lambda () (process "\0")))
  (no-nuls (lambda () (process* cat "\0")))
  (no-nuls (lambda () (process*/ports #f #f #f cat "\0")))
  (no-nuls (lambda () (system "\0")))
  (no-nuls (lambda () (system* cat "\0"))))

(let ([call-empty-arg
       (lambda (empty-arg)
         (let ([out (open-output-string)])
           (test #t 'system-empty-string
                 (parameterize ([current-input-port (open-input-string "Hi\n")]
                                [current-output-port out])
                   (system* self "-e" "(current-command-line-arguments)" empty-arg)))
           (test "'#(\"\")\n" get-output-string out)))])
  (call-empty-arg "")
  (call-empty-arg #""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nested tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (r w id e f)
  (apply values (process* self
			  "-e" 
			  "(let loop () (unless (eof-object? (eval (read))) (loop)))")))

(define (test-line out in)
  (fprintf w "~a\n" in)
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
       (lambda (post-shutdown wait?)
         (let ([c (make-custodian)])
           (let ([l (parameterize ([current-custodian c])
                      (process* self
                                "-e" "0"
                                "-e"
                                "(let loop () (loop))"))])
             (test 'running (list-ref l 4) 'status)
             (read-line (car l)) ; wait until running
             (custodian-shutdown-all c)
             (sleep 0.1)
             (when (and wait?
                        (eq? post-shutdown 'done-error))
               ((list-ref l 4) 'wait))
             (test post-shutdown (list-ref l 4) 'status)
             ((list-ref l 4) 'kill))))])
  (try 'running #f)
  (parameterize ([current-subprocess-custodian-mode 'kill])
    (try 'done-error #f))
  (parameterize ([current-subprocess-custodian-mode 'interrupt])
    (try (if (eq? 'windows (system-type)) 'running 'done-error) #t)))

;; check that #f is an allowed mode:
(parameterize ([current-subprocess-custodian-mode #f])
  (test #f current-subprocess-custodian-mode))

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
                             (regexp-match?
                              (format "(?m:^ *~a(?=[^0-9]))" sub-pid)
                              (let ([s (open-output-string)])
                                (parameterize ([current-output-port s]
                                               [current-input-port (open-input-string "")])
                                  (system (format "ps x")))
                                (get-output-string s))))])
             (let ([sub-pid (read (car l))])
               (test 'running (list-ref l 4) 'status)
               (test #t running? sub-pid)
               ((list-ref l 4) 'kill)
               ((list-ref l 4) 'wait)
               (test 'done-error (list-ref l 4) 'status)
               (test post-shutdown? running? sub-pid)
               (when post-shutdown?
                 (parameterize ([current-input-port (open-input-string "")])
                   (system (format "kill ~a" sub-pid)))))))])
    (try #t)
    (try #f)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check status result

(unless (eq? (system-type) 'windows)
  (parameterize ([current-input-port (open-input-string "")])
    (test 3 system/exit-code "exit 3")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment variables:

(let ([out (open-output-bytes)])
  (parameterize ([current-input-port (open-input-string "Hi\n")]
                 [current-output-port out]
                 [current-environment-variables
                  (environment-variables-copy
                   (current-environment-variables))])
    (environment-variables-set! (current-environment-variables) #"Hola" #"hi, there")
    (system* self "-e" "(getenv \"Hola\")"))
  (test "\"hi, there\"\n" get-output-string out))

;; Check setting of PWD and initializing `current-directory' from
;; PWD, when it involves a soft link:
(when (member (system-type) '(unix macosx))
  (let ([dir (make-temporary-file "sub~a" 'directory)])
    (make-directory (build-path dir "a"))
    (make-file-or-directory-link "a" (build-path dir "b"))
    (current-directory (build-path dir "b"))
    
    (define o (open-output-bytes))
    (parameterize ([current-output-port o])
      (system* self "-e" "(current-directory)"))
    (test (format "~s\n" (path->directory-path (build-path dir "b"))) get-output-string o)

    (define o2 (open-output-bytes))
    (parameterize ([current-output-port o2])
      (system* self "-e" "(current-directory)" #:set-pwd? #f))
    (test (format "~s\n" (path->directory-path (normalize-path (build-path dir "a")))) get-output-string o2)
    
    (delete-directory/files dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for ([f (list tmpfile tmpfile2)] #:when (file-exists? f)) (delete-file f))

(report-errs)
