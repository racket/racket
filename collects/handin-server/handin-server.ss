#cs
(module handin-server mzscheme
  (require (lib "thread.ss")
	   (lib "port.ss")
	   (lib "mzssl.ss" "openssl")
	   (lib "file.ss")
	   (lib "date.ss")
	   (lib "list.ss")
	   "md5.ss"
	   "web-status-server.ss"
	   "run-status.ss")

  (define log-port (open-output-file "log.ss" 'append))

  (define current-session (make-parameter 0))

  (define (LOG str . args)
    ;; Assemble log into into a single string, to make
    ;;  interleaved log lines unlikely:
    (let ([line
	   (format "(~a ~s ~s)~n"
		   (current-session)
		   (parameterize ([date-display-format 'iso-8601])
		     (date->string (seconds->date (current-seconds)) #t))
		   (apply format str args))])
      (display line log-port)))

  (define (get-config which default)
    (get-preference which 
		    (lambda () default)
		    #f
		    "config.ss"))

  (define PORT-NUMBER (get-config 'port-number 7979))
  (define HTTPS-PORT-NUMBER (get-config 'https-port-number (add1 PORT-NUMBER)))
  (define SESSION-TIMEOUT (get-config 'session-timeout 300))
  (define SESSION-MEMORY-LIMIT (get-config 'session-memory-limit 40000000))
  (define DEFAULT-FILE-NAME (get-config 'default-file-name "handin.scm"))
  (define MAX-UPLOAD (get-config 'max-upload 500000))
  (define MAX-UPLOAD-KEEP (get-config 'max-upload-keep 9))
  (define ID-REGEXP (get-config 'id-regexp #rx"^.*$"))
  (define ID-DESC (get-config 'id-desc "anything"))
  (define ALLOW-NEW-USERS? (get-config 'allow-new-users #f))
  (define MASTER-PASSWD (get-config 'master-password #f))

  (define (check-id s)
    (regexp-match ID-REGEXP s))

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

  (define backup-prefix "BACKUP-")
  (define backup-dir-re (regexp (format "^~a[0-9]+$" backup-prefix)))
  (define (backup n) (format "~a~a" backup-prefix n))
  (define (files+backups)
    (let* ([files (directory-list)]
           [backups (filter (lambda (f)
                              (and (directory-exists? f)
                                   (regexp-match backup-dir-re f)))
                            files)])
      (values (remove* backups files) backups)))
  (define (do-backups)
    (let-values ([(files backups) (files+backups)])
      (define (make-backup-available n)
        (when (member (backup n) backups)
          (if (< n MAX-UPLOAD-KEEP)
            (begin (make-backup-available (add1 n))
                   (rename-file-or-directory (backup n) (backup (add1 n))))
            (delete-directory/files (backup n)))))
      (unless (null? files)
        (LOG "backing up ~a" files)
        (make-backup-available 0)
        (make-directory (backup 0))
        (for-each (lambda (file)
                    (rename-file-or-directory file (build-path (backup 0) file)))
                  files))))
  (define (undo-backup)
    ;; It is ok to just move BACKUP-0 to the real directory, the above will
    ;; just find it available on later backups.
    (let-values ([(files backups) (files+backups)])
      (LOG "undoing backup")
      (for-each delete-directory/files files)
      (when (member (backup 0) backups)
        (for-each (lambda (file)
                    (rename-file-or-directory (build-path (backup 0) file) file))
                  (directory-list (backup 0)))
        (delete-directory (backup 0)))))

  (define (save-submission s part)
    (with-output-to-file part
      (lambda () (display s))))

  (define (accept-specific-submission user assignment r r-safe w)
    (parameterize ([current-directory (build-path "active" assignment)])
      (unless (directory-exists? user)
	(make-directory user))
      (parameterize ([current-directory user])
	(let ([len (read r-safe)])
	  (unless (and (number? len)
		       (integer? len)
		       (positive? len))
	    (error 'handin "bad length: ~s" len))
	  (unless (len . < . MAX-UPLOAD)
	    (error 'handin
		   "max handin file size is ~s bytes, file to handin is too big (~s bytes)"
		   MAX-UPLOAD len))
	  (fprintf w "go\n")
	  (unless (regexp-match #rx"[$]" r-safe)
	    (error 'handin
		   "did not find start-of-content marker"))
	  (let ([s (read-string len r)])
	    (unless (and (string? s) (= (string-length s) len))
	      (error 'handin
		     "error uploading (got ~s, expected ~s bytes)"
		     (if (string? s) (string-length s) s)
		     len))
            (do-backups)
	    (LOG "checking ~a for ~a" assignment user)
	    (with-handlers ([void (lambda (e) (undo-backup) (raise e))])
              (let ([part
                     (let ([checker (build-path 'up "checker.ss")])
                       (if (file-exists? checker)
			 ((dynamic-require `(file ,(path->complete-path checker)) 'checker)
			  user s)
			 DEFAULT-FILE-NAME))])
                (fprintf w "confirm\n")
                (let ([v (read (make-limited-input-port r 50))])
                  (if (eq? v 'check)
		    (begin
		      (LOG "saving ~a for ~a" assignment user)
		      (save-submission s part)
		      (fprintf w "done\n"))
		    (error 'handin "upload not confirmed: ~s" v))))))))))

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
			   "user database busy; please try again, and alert the adminstrator is problems persist"))
			"users.ss"))
     orig-custodian))

  (define (add-new-user username r-safe w)
    (let ([full-name (read r-safe)]
	  [id (read r-safe)]
	  [passwd (read r-safe)])
      (unless (and (string? full-name)
		   (string? id)
		   (string? passwd))
	(error 'handin "bad user-addition request"))
      ;; Since we're going to use the username in paths:
      (when (regexp-match #rx"[/\\:]" username)
	(error 'handin "username must not contain a slash, backslash, or colon"))
      (when (regexp-match #rx"^((NUL)|(CON)|(PRN)|(AUX)|(CLOCK[$])|(COM[1-9])|(LPT[1-9]))[.]?" 
			  (list->string (map char-upcase (string->list username))))
	(error 'handin "username must not be a Windows special file name"))
      (when (string=? "solution" username)
	(error 'handin "the username \"solution\" is reserved"))
      (unless (check-id id)
	(error 'handin "id has wrong format: ~a; need ~a for id" id ID-DESC))
      (put-user (string->symbol username)
		(list (md5 passwd) id full-name))
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
    ;; Get username and password:
    (let ([username (read r-safe)]
	  [passwd (read r-safe)])
      (let ([user-data
	     (and (string? username)
		  (get-preference (string->symbol username)
				  (lambda () #f)
				  #f
				  "users.ss"))])
	(cond
	 [(eq? passwd 'create)
	  (when user-data
	    (error 'handin "username already exists: ~a" username))
	  (unless ALLOW-NEW-USERS?
	    (error 'handin "new users not allowed: ~a" username))
	  (LOG "create user: ~a" username)
	  (add-new-user username r-safe w)]
	 [(and user-data
	       (string? passwd)
	       (let ([pw (md5 passwd)])
		 (or (equal? pw (car user-data))
		     (equal? pw MASTER-PASSWD))))
	  (LOG "login: ~a" username)
	  (let ([assignment (read r-safe)])
	    (LOG "assignment for ~a: ~a" username assignment)
	    (if (eq? assignment 'change)
		(change-user-passwd username r-safe w user-data)
		(if (member assignment active-assignments)
		    (begin
		      (fprintf w "ok\n")
		      (accept-specific-submission username assignment r r-safe w))
		    (error 'handin "not an active assignment: ~a" assignment))))]
	 [else
	  (LOG "failed login: ~a" username)
	  (error 'handin "bad username or password for ~a" username)]))))
  
  (define assignment-list
    (quicksort (directory-list "active") string<?))

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

  (define stop-status (serve-status HTTPS-PORT-NUMBER))
  
  (define session-count 0)

  (parameterize ([error-display-handler
		  (lambda (msg exn)
		    (LOG msg))])
    (run-server
     PORT-NUMBER
     (lambda (r w)
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
	      (fprintf w "handin\n")
	      ;; Check protocol:
	      (with-handlers ([exn:fail?
			       (lambda (exn)
				 (let ([msg (if (exn? exn)
						(exn-message exn)
						(format "~e" exn))])
				   (kill-watcher)
				   (LOG "ERROR: ~a" msg)
				   (fprintf w "~s\n" msg)
				   ;; see note on close-output-port below
				   (close-output-port w)))])
		(let ([protocol (read r-safe)])
		  (if (eq? protocol 'original)
		      (fprintf w "original\n")
		      (error 'handin "unknown protocol: ~s" protocol)))
		(accept-submission-or-update assignment-list r r-safe w)
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
     (lambda (port-k)
       (let ([l (ssl-listen port-k 5 #t)])
	 (ssl-load-certificate-chain! l "server-cert.pem")
	 (ssl-load-private-key! l "private-key.pem")
	 l))
     ssl-close
     ssl-accept
     ssl-accept/enable-break)))
