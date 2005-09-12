#cs
(module handin-server mzscheme
  (require (lib "thread.ss")
	   (lib "port.ss")
	   (lib "mzssl.ss" "openssl")
	   (lib "file.ss")
	   (lib "date.ss")
	   (lib "list.ss")
	   (lib "string.ss")
	   "md5.ss"
	   "lock.ss"
	   "web-status-server.ss"
	   "run-status.ss")

  (define log-port (open-output-file "log.ss" 'append))

  (define current-session (make-parameter 0))

  (define (ffprintf port str . args)
    (apply fprintf port str args)
    (flush-output port))

  (define (LOG str . args)
    ;; Assemble log into into a single string, to make
    ;;  interleaved log lines unlikely:
    (let ([line
	   (format "(~a ~s ~s)~n"
		   (current-session)
		   (parameterize ([date-display-format 'iso-8601])
		     (date->string (seconds->date (current-seconds)) #t))
		   (apply format str args))])
      (display line log-port)
      (flush-output log-port)))

  (define (get-config which default)
    (get-preference which (lambda () default) #f "config.ss"))

  (define PORT-NUMBER       (get-config 'port-number 7979))
  (define HTTPS-PORT-NUMBER (get-config 'https-port-number (add1 PORT-NUMBER)))
  (define SESSION-TIMEOUT   (get-config 'session-timeout 300))
  (define SESSION-MEMORY-LIMIT (get-config 'session-memory-limit 40000000))
  (define DEFAULT-FILE-NAME (get-config 'default-file-name "handin.scm"))
  (define MAX-UPLOAD        (get-config 'max-upload 500000))
  (define MAX-UPLOAD-KEEP   (get-config 'max-upload-keep 9))
  (define USER-REGEXP       (get-config 'user-regexp #rx"^[a-z][a-z0-9]+$"))
  (define USER-DESC         (get-config 'user-desc "alphanumeric string"))
  (define USERNAME-CASE-SENSITIVE? (get-config 'username-case-sensitive? #f))
  (define ID-REGEXP         (get-config 'id-regexp #rx"^.*$"))
  (define ID-DESC           (get-config 'id-desc #f))
  (define EMAIL-REGEXP      (get-config 'email-regexp #rx"^[^@<>\"`',]+@[a-zA-Z0-9_.-]+[.][a-zA-Z]+$"))
  (define EMAIL-DESC        (get-config 'email-desc "a valid email address"))
  (define ALLOW-NEW-USERS?  (get-config 'allow-new-users #f))
  (define MASTER-PASSWD     (get-config 'master-password #f))

  (define orig-custodian (current-custodian))

  ;; On startup, check that the prefs file is not locked:
  (put-preferences null null 
		   (lambda (f)
		     (delete-file f)
		     (put-preferences null null 
				      (lambda (f)
					(error 'handin-server
					       "unable to clean up lock file: ~s" f))
				      "users.ss"))
		   "users.ss")

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ATTEMPT-DIR "ATTEMPT")

  (define (success-dir n)
    (format "SUCCESS-~a" n))
  (define (make-success-dir-available n)
    (let ([name (success-dir n)])
      (when (directory-exists? name)
	(if (< n MAX-UPLOAD-KEEP)
	    (begin
	      (make-success-dir-available (add1 n))
	      (rename-file-or-directory name (success-dir (add1 n))))
	    (delete-directory/files name)))))

  (define ATTEMPT-RE (regexp (format "^~a$" ATTEMPT-DIR)))
  (define SUCCESS-RE (regexp (format "^~a$" (success-dir "[0-9]+"))))
  (define SUCCESS-GOOD (map success-dir '(0 1)))

  (define-syntax careful-switch-directory-switch
    (syntax-rules ()
      [(_ ?dir body ...)
       (let ([dir (with-handlers ([void (lambda _ #f)]) (normalize-path ?dir))])
         (when (and dir (directory-exists? dir))
           (parameterize ([current-directory (current-directory)])
             (when (with-handlers ([void (lambda _ #f)])
                     (current-directory dir) #t)
               body ...))))]))

  (define (cleanup-submission-body)
    ;; Find the newest SUCCESS dir -- ignore ATTEMPT, since if it exist it
        ;; means that there was a failed submission and the next one will
        ;; re-create ATTEMPT.
        (let* ([dirlist (map path->string (directory-list))]
               [dir (quicksort
                     (filter (lambda (d)
                               (and (directory-exists? d)
                                    (regexp-match SUCCESS-RE d)))
                             dirlist)
                     string<?)]
               [dir (and (pair? dir) (car dir))])
          (when dir
            (unless (member dir SUCCESS-GOOD)
              (LOG "*** USING AN UNEXPECTED SUBMISSION DIRECTORY: ~a"
                   (build-path (current-directory) dir)))
            ;; We have a submission directory -- copy all newer things (extra
            ;; things that exist in the main submission directory but not in
            ;; SUCCESS, or things that are newer in the main submission
            ;; directory are kept (but subdirs in SUCCESS will are copied as
            ;; is))
            (for-each
             (lambda (f)
               (define dir/f (build-path dir f))
               (cond [(not (or (file-exists? f) (directory-exists? f)))
                      ;; f is in dir but not in the working directory
                      (copy-directory/files dir/f f)]
                     [(or (<= (file-or-directory-modify-seconds f)
                              (file-or-directory-modify-seconds dir/f))
                          (and (file-exists? f) (file-exists? dir/f)
                               (not (= (file-size f) (file-size dir/f)))))
                      ;; f is newer in dir than in the working directory
                      (delete-directory/files f)
                      (copy-directory/files dir/f f)]))
             (directory-list dir)))))

  (define cleanup-sema (make-semaphore 1))
  (define (cleanup-submission dir)
    ;; This is called at a lock cleanup, so it is important that it does not
    ;; throw an exception, or the whole server will be locked down.  It is
    ;; invoked just before the lock is released, so fine to assume that we have
    ;; exclusive access to the directory contents.
    (with-handlers ([void
                     (lambda (e)
                       (LOG "*** ERROR DURING (cleanup-submission ~s) : ~a"
                            dir (if (exn? e) (exn-message e) e)))])
      (parameterize ([current-directory dir])
        (call-with-semaphore cleanup-sema cleanup-submission-body))))

  (define (cleanup-all-submissions)
    (LOG "Cleaning up all submission directories")
    (for-each (lambda (top)
                (when (directory-exists? top)
                  (parameterize ([current-directory top])
                    (for-each (lambda (pset)
                                (when (directory-exists? pset) ; filter non-dirs
                                  (parameterize ([current-directory pset])
                                    (for-each (lambda (sub)
                                                (when (directory-exists? sub)
                                                  (cleanup-submission sub)))
                                              (directory-list)))))
                              (directory-list)))))
              '("active" "inactive")))

  ;; On startup, we scan all submissions, then repeat at random intervals (only
  ;; if clients connected in that time), and check often for changes in the
  ;; active/inactive directories and run a cleanup if there was a change
  (define connection-num 0)
  (thread (lambda ()
            (define last-active/inactive #f)
            (define last-connection-num #f)
            (let loop ()
              (let loop ([n (+ 20 (random 20))]) ; 10-20 minute delay
                (when (>= n 0)
                  (let ([new (map directory-list '("active" "inactive"))])
                    (if (equal? new last-active/inactive)
                      (begin (sleep 30) (loop (sub1 n)))
                      (begin (set! last-active/inactive new)
                             (set! last-connection-num #f))))))
              (unless (equal? last-connection-num connection-num)
                (cleanup-all-submissions)
                (set! last-connection-num connection-num))
              (loop))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (save-submission s part)
    (with-output-to-file part
      (lambda () (display s))))

  (define (accept-specific-submission users assignment r r-safe w)
    ;; Note: users are always sorted
    (define dirname
      (apply string-append (car users)
             (map (lambda (u) (string-append "+" u)) (cdr users))))
    (define len (read r-safe))
    (unless (and (number? len) (integer? len) (positive? len))
      (error 'handin "bad length: ~s" len))
    (unless (len . < . MAX-UPLOAD)
      (error 'handin
             "max handin file size is ~s bytes, file to handin is too big (~s bytes)"
             MAX-UPLOAD len))
    (parameterize ([current-directory (build-path "active" assignment)])
      (wait-for-lock dirname
        (let ([dir (build-path (current-directory) dirname)])
          (lambda () (cleanup-submission dir))))
      (when (and (pair? users) (pair? (cdr users)))
        ;; two or more users -- lock each one
        (for-each wait-for-lock users))
      (ffprintf w "go\n")
      (unless (regexp-match #rx"[$]" r-safe)
        (error 'handin "did not find start-of-content marker"))
      (let ([s (read-bytes len r)])
        (unless (and (bytes? s) (= (bytes-length s) len))
          (error 'handin "error uploading (got ~e, expected ~s bytes)"
                 (if (bytes? s) (bytes-length s) s) len))
        ;; we have a submission, need to create a directory if needed, make
        ;; sure that no users submitted work with someone else
        (unless (directory-exists? dirname)
          (for-each
           (lambda (dir)
             (for-each
              (lambda (d)
                (when (member d users)
                  (error 'handin
                         "bad submission: ~a has an existing submission (~a)"
                         d dir)))
              (regexp-split #rx" *[+] *" (path->string dir))))
           (directory-list))
          (make-directory dirname))
        (parameterize ([current-directory dirname])
          ;; Clear out old ATTEMPT, if any, and make a new one:
          (when (directory-exists? ATTEMPT-DIR)
            (delete-directory/files ATTEMPT-DIR))
          (make-directory ATTEMPT-DIR)
          (save-submission s (build-path ATTEMPT-DIR "handin"))
          (LOG "checking ~a for ~a" assignment users)
          (let ([part
                 ;; Result is either a string or list of strings:
                 (let ([checker (build-path 'up "checker.ss")])
                   (if (file-exists? checker)
                     (let ([checker (path->complete-path checker)])
                       (parameterize ([current-directory ATTEMPT-DIR])
                         ((dynamic-require checker 'checker) users s)))
                     DEFAULT-FILE-NAME))])
            (ffprintf w "confirm\n")
            (let ([v (read (make-limited-input-port r 50))])
              (if (eq? v 'check)
                (begin
                  (LOG "saving ~a for ~a" assignment users)
                  (parameterize ([current-directory ATTEMPT-DIR])
                    (rename-file-or-directory "handin" (if (pair? part) (car part) part)))
                  ;; Shift successful-attempt directories so that there's
                  ;;  no SUCCESS-0:
                  (make-success-dir-available 0)
                  (rename-file-or-directory ATTEMPT-DIR (success-dir 0))
                  (if (pair? part)
                    (write (list 'result (cadr part)) w)
                    (fprintf w "done\n"))
                  (flush-output w))
                (error 'handin "upload not confirmed: ~s" v))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (put-user key val)
    ;; Although we don't have to worry about trashing the
    ;;  prefs file, we do have to worry about a thread
    ;;  getting killed while it locks the pref file.
    ;; Avoid the problem by using orig-custodian.
    (call-in-nested-thread
     (lambda ()
       (put-preferences (list key)
			(list val)
			(lambda (f)
			  (error 
			   'handin 
			   "user database busy; please try again, and alert the adminstrator if problems persist"))
			"users.ss"))
     orig-custodian))

  (define (add-new-user username r-safe w)
    (thread (lambda () (sleep 5) (close-input-port r-safe)))
    (let ([full-name (read r-safe)]
	  [id        (read r-safe)]
	  [email     (read r-safe)]
	  [passwd    (read r-safe)])
      (unless (and (string? full-name)
		   (string? id)
		   (string? email)
		   (string? passwd))
	(error 'handin "bad user-addition request"))
      (unless (regexp-match USER-REGEXP username)
        (error 'handin "bad username: \"~a\"~a" username
               (if USER-DESC (format "; need ~a" USER-DESC) "")))
      ;; Since we're going to use the username in paths:
      (when (regexp-match #rx"[/\\:|\"<>]" username)
	(error 'handin "username must not contain one of the following: / \\ : | \" < >"))
      (when (regexp-match #rx"^((nul)|(con)|(prn)|(aux)|(clock[$])|(com[1-9])|(lpt[1-9]))[.]?" 
			  (string-foldcase username))
	(error 'handin "username must not be a Windows special file name"))
      (when (regexp-match #rx"^[ .]|[ .]$" username)
	(error 'handin "username must not begin or end with a space or period"))
      (when (regexp-match #rx"^solution" username)
	(error 'handin "the username prefix \"solution\" is reserved"))
      (when (string=? "checker.ss" username)
	(error 'handin "the username \"checker.ss\" is reserved"))
      (unless (regexp-match ID-REGEXP id)
	(error 'handin "id has wrong format: ~a~a" id
               (if ID-DESC (format "; need ~a for id" ID-DESC) "")))
      (unless (regexp-match EMAIL-REGEXP email)
        (error 'handin "email has wrong format: ~a~a" email
               (if EMAIL-DESC (format "; need ~a" EMAIL-DESC) "")))
      (LOG "create user: ~a" username)
      (put-user (string->symbol username)
		(list (md5 passwd) id full-name email))
      (fprintf w "ok~n")))

  (define (change-user-passwd username r-safe w old-user-data)
    (let ([new-passwd (read r-safe)])
      (LOG "change passwd for ~a" username)
      (unless (string? new-passwd)
	(error 'handin "bad password-change request"))
      (put-user (string->symbol username)
		(cons (md5 new-passwd) (cdr old-user-data)))
      (fprintf w "ok~n")))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (accept-submission-or-update active-assignments r r-safe w)
    (fprintf w "~s~n" active-assignments)
    ;; Get usernames and password:
    (let* ([user-string
            (let ([s (read r-safe)])
              (and (string? s)
                   (if USERNAME-CASE-SENSITIVE?
                     s
		     (string-foldcase s))))]
           [usernames
            ;; Username lists must always be sorted
            (if user-string
              (quicksort (regexp-split #rx" *[+] *" user-string) string<?)
              '())]
           [user-datas (map (lambda (u)
                              (get-preference (string->symbol u)
                                              (lambda () #f) #f "users.ss"))
                            usernames)]
           [passwd (read r-safe)])
      (cond
       [(eq? passwd 'create)
	(wait-for-lock "+newuser+")
        (unless ALLOW-NEW-USERS?
          (error 'handin "new users not allowed: ~a" user-string))
        (unless (= 1 (length usernames))
          (error 'handin "username must not contain a \"+\": ~a" user-string))
        ;; we now know that there is a single username, and (car usernames) is
        ;; the same at user-string
        (when (car user-datas)
          (error 'handin "username already exists: `~a'" user-string))
        (add-new-user user-string r-safe w)]
       [(and (pair? user-datas)
             (not (memq #f user-datas))
             (string? passwd)
             (let ([pw (md5 passwd)])
               (ormap (lambda (p) (equal? p pw))
                      (cons MASTER-PASSWD (map car user-datas)))))
        (LOG "login: ~a" usernames)
        (let ([assignment (read r-safe)])
          (LOG "assignment for ~a: ~a" usernames assignment)
          (if (eq? assignment 'change)
            (if (= 1 (length usernames))
              (change-user-passwd (car usernames) r-safe w (car user-datas))
              (error 'handin "cannot change a password on a joint login"))
            (if (member assignment active-assignments)
              (begin
                (fprintf w "ok\n")
                (accept-specific-submission usernames assignment r r-safe w))
              (error 'handin "not an active assignment: ~a" assignment))))]
       [else
        (LOG "failed login: ~a" user-string)
        (error 'handin "bad username or password for ~a" user-string)])))

  (define (assignment-list)
    (quicksort (map path->string (directory-list "active")) string<?))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define no-limit-warning? #f)

  (define (with-watcher w proc)
    (if no-limit-warning?
	;; Already know watcher doesn't work:
	(proc void)
	;; Try making a watcher:
	(let ([session-cust (make-custodian)]
	      [session-channel (make-channel)]
	      [status-box (box #f)])
	  (let ([watcher
		 (with-handlers ([exn:fail:unsupported? 
				  (lambda (x) 
				    (set! no-limit-warning? #t)
				    (LOG "WARNING: per-session memory limit not supported by MrEd")
				    #f)])
		   (custodian-limit-memory session-cust SESSION-MEMORY-LIMIT session-cust)
		   (parameterize ([current-custodian orig-custodian])
		     (thread (lambda ()
			       (let ([session-thread (channel-get session-channel)])
				 (let loop ()
				   (if (sync/timeout 3 session-thread)
				       (begin
					 (LOG "session killed while ~s" (unbox status-box))
					 (fprintf w "~s\n"
						  (format
						   "handin terminated due to excessive memory computation~a"
						   (if (unbox status-box)
						       (format " while ~a" (unbox status-box))
						       "")))
					 (close-output-port w)
					 (channel-put session-channel 'done))
				       (begin
					 (collect-garbage)
					 (LOG "running ~a (~a  ~a)"
					      (current-memory-use session-cust)
					      (current-memory-use orig-custodian)
					      (current-memory-use))
					 (loop)))))))))])
	    (if watcher
		;; Run proc in a thread under session-cust:
		(let ([session-thread
		       (parameterize ([current-custodian session-cust]
				      [current-run-status-box status-box])
			 (thread
			  (lambda ()
			    (proc (lambda ()
				    ;; Proc has succeeded...
				    (parameterize ([current-custodian orig-custodian])
				      (kill-thread watcher))))
			    (channel-put session-channel 'done-normal))))])
		  (channel-put session-channel session-thread)
		  ;; Wait until the proc is done or killed (and kill is reported):
		  (channel-get session-channel))
		;; Watcher didn't work:
		(proc void))))))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (LOG "server started ------------------------------")

  (define stop-status (serve-status HTTPS-PORT-NUMBER get-config))

  (define session-count 0)

  (parameterize ([error-display-handler
		  (lambda (msg exn)
		    (LOG msg))])
    (run-server
     PORT-NUMBER
     (lambda (r w)
       (set! connection-num (add1 connection-num))
       (when ((current-memory-use) . > . SESSION-MEMORY-LIMIT)
	 (collect-garbage))
       (parameterize ([current-session (begin
					 (set! session-count (add1 session-count))
					 session-count)])
	 (let-values ([(here there) (ssl-addresses r)])
	   (LOG "connect from ~a" there))
	 (with-watcher
	  w
	  (lambda (kill-watcher)
	    (let ([r-safe (make-limited-input-port r 1024)])
	      (ffprintf w "handin\n")
	      ;; Check protocol:
	      (with-handlers ([exn:fail?
			       (lambda (exn)
				 (let ([msg (if (exn? exn)
						(exn-message exn)
						(format "~e" exn))])
				   (kill-watcher)
				   (LOG "ERROR: ~a" msg)
				   (ffprintf w "~s\n" msg)
				   ;; see note on close-output-port below
				   (close-output-port w)))])
		(let ([protocol (read r-safe)])
		  (if (eq? protocol 'original)
		      (ffprintf w "original\n")
		      (error 'handin "unknown protocol: ~s" protocol)))
		(accept-submission-or-update (assignment-list) r r-safe w)
		(LOG "normal exit")
		(kill-watcher)
		;; This close-output-port should not be necessary, and it's
		;; here due to a deficiency in the SLL binding.
		;; The problem is that a custodian shutdown of w is harsher
		;; for SSL output than a normal close. A normal close
		;; flushes an internal buffer that's not supposed to exist, while
		;; the shutdown gives up immediately.
		(close-output-port w)))))))
     SESSION-TIMEOUT
     (lambda (exn)
       (printf "~a~n" (if (exn? exn)
			  (exn-message exn)
			  exn)))
     (lambda (port-k cnt reuse?)
       (let ([l (ssl-listen port-k cnt #t)])
	 (ssl-load-certificate-chain! l "server-cert.pem")
	 (ssl-load-private-key! l "private-key.pem")
	 l))
     ssl-close
     ssl-accept
     ssl-accept/enable-break)))
