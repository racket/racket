
(load-relative "testing.rktl")

(require racket/system
         racket/file
         ffi/unsafe/port)

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

(unless cat
  (error "\"cat\" executable not found"))

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
               (test #t '1e50001 (equal? (expt 10 50000) (read p)))
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
    (test f2 f f))

  (close-input-port f)
  (close-output-port f2))

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

;; Check that a subprocess is removed from its custodian as
;; soon as it's known to be done:
(let* ([c (make-custodian)]
       [c2 (make-custodian c)])
  (define-values (sp i o e)
    (parameterize ([current-custodian c2]
                   [current-subprocess-custodian-mode 'kill])
      (subprocess #f #f #f self "-e" "(read-byte)")))
  (test #t pair? (member sp (custodian-managed-list c2 c)))
  (close-output-port o)
  (subprocess-wait sp)
  (test #f pair? (member sp (custodian-managed-list c2 c)))
  (custodian-shutdown-all c))

;; Check custodian-boxes are omitted by custodian-managed-list:
(let* ([c (make-custodian)]
       [c2 (make-custodian c)])
  (define cb (make-custodian-box c2 'value))
  (test '() custodian-managed-list c2 c)
  (test 'value custodian-box-value cb)
  (custodian-shutdown-all c2)
  (test #f custodian-box-value cb))

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
               (unless post-shutdown?
                 ;; May need to wait for the init process to reap the
                 ;; sub-pid process (since that's a sub-sub-process to
                 ;; us)
                 (let loop ()
                   (when (running? sub-pid)
                     (sleep 0.05)
                     (loop))))
               (test post-shutdown? running? sub-pid)
               (when post-shutdown?
                 (parameterize ([current-input-port (open-input-string "")])
                   (system (format "kill ~a" sub-pid))))
               (close-input-port (car l))
               (close-output-port (cadr l))
               (close-input-port (cadddr l)))))])
    (try #t)
    (try #f))

  (define (try-add-to-group kill-second?)
    (define-values (p1 o1 i1 e1) (subprocess #f #f #f 'new "/bin/cat"))
    (define-values (p2 o2 i2 e2) (subprocess #f #f #f p1 "/bin/cat"))
    
    (test 'running subprocess-status p1)
    (test 'running subprocess-status p2)

    (when kill-second?
      (subprocess-kill p2 #t)
      (test p2 sync p2)
      (test 'running subprocess-status p1))

    (subprocess-kill p1 #t)
    (test p1 sync p1)
    (test p2 sync p2)
    
    (test (subprocess-status p1) subprocess-status p2)

    (close-input-port o1)
    (close-input-port o2)
    (close-input-port e1)
    (close-input-port e2)
    (close-output-port i1)
    (close-output-port i2))

  (try-add-to-group #f)
  (try-add-to-group #t))

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
  (parameterize ([current-directory (current-directory)])
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
      
      (delete-directory/files dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that the reading from a file descriptor from `subprocess`
;; doesn't fail to unregister it in the fd-semaphore table, which
;; could cause this test to block waiting on a semaphore that
;; won't get posted due to recycling a now-closed fd

(for ([i 25])
  (let ([p (process* cat)])
    (define t
      (thread (lambda ()
                (read-bytes 100 (car p)))))
    (sync (system-idle-evt))
    (when (even? i) (kill-thread t))
    (close-output-port (cadr p))
    (thread-wait t)
    ((list-ref p 4) 'wait)
    (test 'done-ok (list-ref p 4) 'status)
    (close-input-port (car p))
    (close-input-port (cadddr p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check the process state for subprocesses to ensure that, for example,
;; SIGCHLD is not blocked

(unless (eq? (system-type) 'windows)
  (let* ([dir (make-temporary-file "sub~a" 'directory)]
         [exe (build-path dir "check")]
         [cc-path (or (find-executable-path "cc")
                      (find-executable-path "gcc"))])
    (when (and cc-path
               (system* cc-path
                        "-o"
                        exe
                        (path->complete-path "unix_check.c" (or (current-load-relative-directory)
                                                                (current-directory)))))
      (test #t 'subprocess-state (let ([o (open-output-bytes)])
                                   (or (parameterize ([current-output-port o])
                                         (system* exe))
                                       (get-output-bytes o)))))
    (delete-directory/files dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure that closing file descriptors in the parent process doesn't
;; cause problems

(let ()
  (define out (open-output-string))
  (define err (open-output-string))
  (test #t 'subprocess-closed-stdin
        (parameterize ([current-output-port out]
                       [current-error-port err])
          (system* self "-e"
                   (format "~s" '(let ()
                                   (define self (vector-ref (current-command-line-arguments) 0))
                                   (close-input-port (current-input-port))
                                   (define-values (subp out in err)
                                     (subprocess #f #f #f self "-e"
                                                 (format "~s" '(begin
                                                                 (display (read-line))
                                                                 (display (read-line)
                                                                          (current-error-port))))))
                                   (displayln "hello" in)
                                   (displayln "goodbye" in)
                                   (close-output-port in)
                                   (copy-port out (current-output-port))
                                   (copy-port err (current-error-port))
                                   (subprocess-wait subp)
                                   (exit (subprocess-status subp))))
                   "--" self)))
  (test "hello" get-output-string out)
  (test "goodbye" get-output-string err))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check Windows command-line parsing

(when (eq? 'windows (system-type))
  (define (try-arg cmdline-str result-str)
    (let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
      (define-values (sp i o no-e)
	(subprocess #f #f f self 'exact
		    (string-append (regexp-replace* #rx" " (path->string self) "\" \"")
                                   " -l racket/base"
                                   " -e \"(write (vector-ref (current-command-line-arguments) 0))\""
                                   " " cmdline-str)))
      (close-output-port o)
      (test result-str read i)
      (subprocess-wait sp) 
      (close-output-port f)
      (close-input-port i))
    ;; Check encoding by `subprocess`, too
    (let ([f (open-output-file tmpfile #:exists 'truncate/replace)])
      (define-values (sp i o no-e)
	(subprocess #f #f f self
		    "-l" "racket/base"
                    "-e" "(write (vector-ref (current-command-line-arguments) 0))"
		    result-str))
      (close-output-port o)
      (test result-str read i)
      (subprocess-wait sp) 
      (close-output-port f)
      (close-input-port i)))

  (try-arg "x" "x")
  (try-arg "\"x\"" "x")
  (try-arg "\"a \"\"b\"\" c\"" "a \"b\" c")
  (try-arg "\"a \"\"b\"\" c" "a \"b\" c")
  (try-arg "\"a\\\"" "a\"")
  (try-arg "a\\\"" "a\"")
  (try-arg "a\\\"b" "a\"b")
  (try-arg "a\\\\\"b" "a\\b")
  (try-arg "a\\\\\\\"b" "a\\\"b")
  (try-arg "a\\\\\\\\\"b" "a\\\\b")
  (try-arg "a\\\\\\\\\\\"b" "a\\\\\"b"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check file-descriptor sharing

;; This test includes the questionable action of creating a bad file
;; descriptor and expecting the OS to tell us that it's bad (implicit
;; in `read-char`). As of Mac OS 13.2 Ventura, the select() system
;; call only complains about bad file descriptors up to number 24; if
;; a bad 25 or up is supplied, it select() seems to ignore bad
;; descriptors. So, take care that this test is not run with too many
;; unclosed ports.

(define (check-sharing keep-mode)
  (define fn (make-temporary-file))
  (call-with-output-file*
   fn
   #:exists 'update
   (lambda (o)
     (display "123" o)))

  (define f (open-input-file fn))
  (define fd (unsafe-port->file-descriptor f))

  (define o (open-output-bytes))
  (define e (open-output-bytes))

  (define ok?
    (parameterize ([current-output-port o]
                   [current-error-port e]
                   [current-subprocess-keep-file-descriptors keep-mode])
      (system* self
               "-l" "racket/base"
               "-e"
               "(displayln 'y)"
               "-l" "ffi/unsafe/port"
               "-e"
               (format "(define f (unsafe-file-descriptor->port ~a 'in '(read)))" fd)
               "-e"
               "(displayln (read-char f))")))

  (close-input-port f)
  (delete-directory/files fn)

  (list ok? (get-output-bytes o) (regexp-match? #rx"error reading" (get-output-bytes e))))

(unless (eq? 'windows (system-type))
  (test '(#t #"y\n1\n" #f) check-sharing 'all))
(test '(#f #"y\n" #t) check-sharing 'inherited)
(test '(#f #"y\n" #t) check-sharing '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check for cleaning up a subprocess without waiting for it to complete:
(for ([j 10])
  (for-each
   (lambda (f) (f))
   (for/list ([i 10])
     (define-values (sp o i e) (subprocess #f #f #f cat))
     (subprocess-kill sp 'kill)
     (lambda ()
       (close-output-port i)
       (close-input-port o)
       (close-input-port e))))
  (collect-garbage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Check that a-exit custodians are run on exit, even if the

(for ([cust (list '(current-custodian)
                  '(make-custodian)
                  '(make-custodian (make-custodian)))])
  (define-values (sp o i e) (subprocess #f #f #f self
                                        "-l" "racket/base"
                                        "-l" "ffi/unsafe/custodian"
                                        "-W" "error"
                                        "-e" "(eval (read))"))
  (write `(register-custodian-shutdown 'hello
                                       (lambda (x)
                                         (log-error "bye"))
                                       ,cust
                                       #:at-exit? #t)
         i)
  (close-output-port i)
  (read-bytes 1024 o)
  (close-input-port o)
  (test #"bye\n" read-bytes 1024 e)
  (close-input-port e)
  (subprocess-wait sp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check that `--eval` and similar can set the namespace

(let ()
    (define-values (sp o i e) (subprocess #f #f #f self
                                          "-e" (format "~s"
                                                       '(let ([ns (make-base-namespace)])
                                                          (eval '(define here "yes") ns)
                                                          (current-namespace ns)))
                                          "-e" "(displayln here)"))
    (close-output-port i)
    (test "yes" read-line o)
    (read-bytes 1024 o)
    (read-bytes 1024 e)
    (sync sp)
    (close-input-port e)
    (close-input-port o))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for ([f (list tmpfile tmpfile2)] #:when (file-exists? f)) (delete-file f))


(report-errs)
