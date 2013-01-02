#lang racket/base

(require mzlib/thread
         racket/port
         openssl
         racket/file
         "private/logger.rkt"
         "private/config.rkt"
         "private/lock.rkt"
         "private/md5.rkt"
         "private/run-status.rkt"
         "private/reloadable.rkt"
         "private/hooker.rkt"
         (prefix-in web: "web-status-server.rkt")
         ;; this sets some global parameter values, and this needs
         ;; to be done in the main thread, rather than later in a
         ;; user session thread (that will make the global changes
         ;; not to be global.)
         "sandbox.rkt"
         ;; workaround for a confusing problem: without this, the gui
         ;; gets initialized in a handler (since checks use it, and
         ;; they're being required dynamically), and further handlers
         ;; will fail with "queue-callback: eventspace is shutdown",
         ;; requiring it here makes it avoids killing the eventspace
         racket/gui/base)

(install-logger-port)

;; errors to the user: no need for a "foo: " prefix
(define (error* fmt . args)
  (error (apply format fmt args)))

(define (write+flush port . xs)
  (for ([x (in-list xs)]) (write x port) (newline port))
  (flush-output port))

(define-struct alist (name [l #:mutable]))
(define (a-set! alist key val)
  (let ([l (alist-l alist)])
    (cond [(assq key l) => (lambda (p) (set-box! (cdr p) val))]
          [else (set-alist-l! alist (cons (cons key (box val)) l))])))
(define (a-ref alist key . default)
  (cond [(assq key (alist-l alist)) => (lambda (x) (unbox (cdr x)))]
        [(pair? default) (car default)]
        [else (error (alist-name alist) "no value for `~s'" key)]))

(define orig-custodian (current-custodian))

;; On startup, check that the users file is not locked:
(put-preferences null null
  (lambda (f)
    (delete-file f)
    (put-preferences null null
                     (lambda (f)
                       (error 'handin-server
                              "unable to clean up lock file: ~s" f))
                     "users.rktd"))
  "users.rktd")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ATTEMPT-DIR "ATTEMPT")
(define (success-dir n) (format "SUCCESS-~a" n))

(define (make-success-dir-available n)
  (let ([name (success-dir n)])
    (when (directory-exists? name)
      (if (< n (get-conf 'max-upload-keep))
        (begin (make-success-dir-available (add1 n))
               (rename-file-or-directory name (success-dir (add1 n))))
        (delete-directory/files name)))))

(define ATTEMPT-RE (regexp (format "^~a$" ATTEMPT-DIR)))
(define SUCCESS-RE (regexp (format "^~a$" (success-dir "[0-9]+"))))
(define SUCCESS-GOOD (map success-dir '(0 1)))

(define (cleanup-submission-body)
  ;; Find the newest SUCCESS dir -- ignore ATTEMPT, since if it exist it
  ;; means that there was a failed submission and the next one will
  ;; re-create ATTEMPT.
  (let* ([dirlist (map path->string (directory-list))]
         [dir (sort (filter (lambda (d)
                              (and (directory-exists? d)
                                   (regexp-match SUCCESS-RE d)))
                            dirlist)
                    string<?)]
         [dir (and (pair? dir) (car dir))])
    (when dir
      (unless (member dir SUCCESS-GOOD)
        (log-line "*** USING AN UNEXPECTED SUBMISSION DIRECTORY: ~a"
                  (build-path (current-directory) dir)))
      ;; We have a submission directory -- copy all newer things (extra
      ;; things that exist in the main submission directory but not in
      ;; SUCCESS, or things that are newer in the main submission
      ;; directory are kept (but subdirs in SUCCESS will are copied as
      ;; is))
      (for ([f (in-list (directory-list dir))])
        (define dir/f (build-path dir f))
        (cond [(not (or (file-exists? f) (directory-exists? f)))
               ;; f is in dir but not in the working directory
               (copy-directory/files dir/f f)]
              [(or (<= (file-or-directory-modify-seconds f)
                       (file-or-directory-modify-seconds dir/f))
                   ;; just in case, check the size too:
                   (and (file-exists? f) (file-exists? dir/f)
                        (not (= (file-size f) (file-size dir/f)))))
               ;; f is newer in dir than in the working directory
               (delete-directory/files f)
               (copy-directory/files dir/f f)])))))

(define cleanup-sema (make-semaphore 1))
(define (cleanup-submission dir)
  ;; This is called at a lock cleanup, so it is important that it does not
  ;; throw an exception, or the whole server will be locked down.  It is
  ;; invoked just before the lock is released, so fine to assume that we have
  ;; exclusive access to the directory contents.
  (with-handlers ([void
                   (lambda (e)
                     (log-line "*** ERROR DURING (cleanup-submission ~s) : ~a"
                               dir (if (exn? e) (exn-message e) e)))])
    (when (directory-exists? dir) ; submissions can fail before mkdir
      (parameterize ([current-directory dir])
        (call-with-semaphore cleanup-sema cleanup-submission-body)))))

(define (cleanup-all-submissions)
  (log-line "Cleaning up all submission directories")
  (for ([pset (in-list (get-conf 'all-dirs))]
        #:when (directory-exists? pset)) ; just in case
    (parameterize ([current-directory pset])
      (for ([sub (in-list (directory-list))]
            #:when (directory-exists? sub)) ; filter non-dirs
        (cleanup-submission sub)))))

;; On startup, we scan all submissions, then repeat at random intervals (only
;; if clients connected in that time), and check often for changes in the
;; active/inactive directories and run a cleanup if there was a change
(define connection-num 0)
(void
 (thread (lambda ()
           (define last-all-dirs #f)
           (define last-connection-num #f)
           (let loop ()
             (let loop ([n (+ 20 (random 20))]) ; 10-20 minute delay
               (when (>= n 0)
                 (let ([new (get-conf 'all-dirs)])
                   (if (equal? new last-all-dirs)
                     (begin (sleep 30) (loop (sub1 n)))
                     (begin (set! last-all-dirs new)
                            (set! last-connection-num #f))))))
             (unless (equal? last-connection-num connection-num)
               (cleanup-all-submissions)
               (set! last-connection-num connection-num))
             (loop)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-submission s part)
  (with-output-to-file part
    (lambda () (display s))))

(define (users->dirname users)
  (apply string-append (car users)
         (map (lambda (u) (string-append "+" u)) (cdr users))))

(define (accept-specific-submission data r r-safe w)
  ;; Note: users are always sorted
  (define users       (a-ref data 'usernames))
  (define assignments (a-ref data 'assignments))
  (define assignment  (a-ref data 'assignment))
  (define dirname     (users->dirname users))
  (define len #f)
  (unless (member assignment assignments)
    (error* "not an active assignment: ~a" assignment))
  (log-line "assignment for ~a: ~a" users assignment)
  (hook 'submission-received `([usernames ,users] [assignment ,assignment]))
  (write+flush w 'ok)
  (set! len (read r-safe))
  (unless (and (number? len) (integer? len) (positive? len))
    (error* "bad length: ~s" len))
  (let ([max (get-conf 'max-upload)])
    (unless (len . < . max)
      (error* "max handin file size is ~s bytes, ~a (~s bytes)"
              max "file to handin is too big" len)))
  (parameterize ([current-directory (assignment<->dir assignment)])
    (wait-for-lock dirname
      (let ([dir (build-path (current-directory) dirname)])
        (lambda () (cleanup-submission dir))))
    (when (and (pair? users) (pair? (cdr users)))
      ;; two or more users -- lock each one
      (for-each wait-for-lock users))
    (write+flush w 'go)
    (unless (regexp-match #rx"[$]" r-safe)
      (error* "did not find start-of-content marker"))
    (let ([s (read-bytes len r)])
      (unless (and (bytes? s) (= (bytes-length s) len))
        (error* "error uploading (got ~e, expected ~s bytes)"
                (if (bytes? s) (bytes-length s) s) len))
      ;; we have a submission, need to create a directory if needed, make
      ;; sure that no users submitted work with someone else
      (unless (directory-exists? dirname)
        (for* ([dir (directory-list)]
               [d (regexp-split #rx" *[+] *" (path->string dir))])
          (when (member d users)
            (error* "bad submission: ~a has an existing submission (~a)"
                    d dir)))
        (make-directory dirname))
      (parameterize ([current-directory dirname]
                     [current-messenger
                      (case-lambda
                        [(msg) (write+flush w 'message msg)]
                        [(msg styles)
                         (if (eq? 'final styles)
                           (write+flush w 'message-final msg)
                           (begin (write+flush w 'message-box msg styles)
                                  (read (make-limited-input-port r 50))))])])
        ;; Clear out old ATTEMPT, if any, and make a new one:
        (when (directory-exists? ATTEMPT-DIR)
          (delete-directory/files ATTEMPT-DIR))
        (make-directory ATTEMPT-DIR)
        (save-submission s (build-path ATTEMPT-DIR "handin"))
        (timeout-control 'reset)
        (log-line "checking ~a for ~a" assignment users)
        (let* ([checker* (path->complete-path (build-path 'up "checker.rkt"))]
               [checker* (and (file-exists? checker*)
                              (parameterize ([current-directory server-dir])
                                (auto-reload-value
                                 `(file ,(path->string checker*))
                                 'checker)))])
          (define-values (pre checker post)
            (cond [(not checker*) (values #f #f #f)]
                  [(procedure? checker*) (values #f checker* #f)]
                  [(and (list? checker*) (= 3 (length checker*)))
                   (apply values checker*)]
                  [else (error* "bad checker value: ~e" checker*)]))
          (when pre
            (let ([dir (current-directory)])
              (with-handlers
                  ([void (lambda (e)
                           (parameterize ([current-directory dir])
                             (unless (ormap (lambda (d)
                                              (and (directory-exists? d)
                                                   (regexp-match
                                                    SUCCESS-RE
                                                    (path->string d))))
                                            (directory-list))
                               (parameterize ([current-directory ".."])
                                 (when (directory-exists? dirname)
                                   (delete-directory/files dirname)))))
                           (raise e))])
                (parameterize ([current-directory ATTEMPT-DIR])
                  (pre users s)))))
          (let ([part (if checker
                        (parameterize ([current-directory ATTEMPT-DIR])
                          (checker users s))
                        (get-conf 'default-file-name))])
            (write+flush w 'confirm)
            (let ([v (read (make-limited-input-port r 50))])
              (if (eq? v 'check)
                (begin
                  (log-line "saving ~a for ~a" assignment users)
                  (parameterize ([current-directory ATTEMPT-DIR])
                    (cond [part (unless (equal? part "handin")
                                  (rename-file-or-directory "handin" part))]
                          [(file-exists? "handin") (delete-file "handin")]))
                  ;; Shift successful-attempt directories so that there's
                  ;;  no SUCCESS-0:
                  (make-success-dir-available 0)
                  (rename-file-or-directory ATTEMPT-DIR (success-dir 0))
                  (hook 'submission-committed
                        `([usernames ,users] [assignment ,assignment]))
                  (when post
                    (parameterize ([current-directory (success-dir 0)])
                      (post users s))))
                (error* "upload not confirmed: ~s" v)))))))))

(define (retrieve-specific-submission data w)
  ;; Note: users are always sorted
  (define users       (a-ref data 'usernames))
  (define assignments (a-ref data 'assignments))
  (define assignment  (a-ref data 'assignment))
  (define dirname     (users->dirname users))
  (define submission-dir (build-path (assignment<->dir assignment) dirname))
  (unless (member assignment assignments)
    (error* "not an active assignment: ~a" assignment))
  (unless (directory-exists? submission-dir)
    (error* "no ~a submission directory for ~a" assignment users))
  (log-line "retrieving assignment for ~a: ~a" users assignment)
  (parameterize ([current-directory submission-dir])
    (define magics '(#"WXME"
                     #"<<<MULTI-SUBMISSION-FILE>>>"
                     #"#reader(lib\"read.ss\"\"wxme\")WXME"
                     #"#reader(lib\"read.rkt\"\"wxme\")WXME"))
    (define mlen (apply max (map bytes-length magics)))
    (define file
      ;; find the newest wxme file
      (let loop ([files (directory-list)] [file #f] [time #f])
        (if (null? files)
          file
          (let ([f (car files)])
            (if (and (file-exists? f)
                     (let ([m (with-input-from-file f
                                (lambda () (read-bytes mlen)))])
                       (ormap (lambda (magic)
                                (and (>= (bytes-length m) (bytes-length magic))
                                     (equal? magic
                                             (subbytes m 0
                                                       (bytes-length magic)))))
                              magics))
                     (or (not file)
                         (> (file-or-directory-modify-seconds f) time)))
              (loop (cdr files) f (file-or-directory-modify-seconds f))
              (loop (cdr files) file time))))))
    (if file
      (let ([len (file-size file)])
        (write+flush w len)
        (display "$" w)
        (display (with-input-from-file file (lambda () (read-bytes len))) w)
        (flush-output w)
        (hook 'submission-retrieved
              `([usernames ,users] [assignment ,assignment])))
      (error* "no ~a submission file found for ~a" assignment users))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (put-user-data username data)
  ;; Although we don't have to worry about trashing the
  ;;  prefs file, we do have to worry about a thread
  ;;  getting killed while it locks the pref file.
  ;; Avoid the problem by using orig-custodian.
  (call-in-nested-thread
   (lambda ()
     (put-preferences
      (list (string->symbol username)) (list data)
      (lambda (f)
        (error* "user database busy; please try again, and alert the administrator if problems persist"))
      "users.rktd"))
   orig-custodian))

(define (get-user-data username)
  (get-preference (string->symbol username) (lambda () #f) 'timestamp
                  "users.rktd"))
(define (check-field value field-re field-name field-desc)
  (unless (cond [(or (string? field-re) (regexp? field-re))
                 (regexp-match field-re value)]
                [(list? field-re) (member value field-re)]
                [(not field-re) #t]
                [(eq? field-re '-) #t] ; -> hidden field, no check
                [else (error* "bad spec: field-regexp is ~e" field-re)])
    (error* "bad ~a: \"~a\"~a" field-name value
            (if field-desc (format "; need ~a" field-desc) ""))))

;; Utility for the next two functions: reconstruct a full list of
;; extra-fields from user-fields, using "" for hidden fields
(define (add-hidden-to-user-fields user-fields)
  (let ([user-field-name->user-field
         (map cons (get-conf 'user-fields) user-fields)])
    (map (lambda (f)
           (cond [(assq f user-field-name->user-field) => cdr]
                 [else ""]))
         (get-conf 'extra-fields))))

(define (add-new-user data)
  (define username     (a-ref data 'username/s))
  (define passwd       (a-ref data 'password))
  (define user-fields  (a-ref data 'user-fields))
  (define extra-fields (add-hidden-to-user-fields user-fields))
  (unless (get-conf 'allow-new-users)
    (error* "new users not allowed: ~a" username))
  (check-field username (get-conf 'user-regexp) "username"
               (get-conf 'user-desc))
  ;; Since we're going to use the username in paths, and + to split names:
  (when (regexp-match #rx"[+/\\:|\"<>]" username)
    (error* "username must not contain these characters: + / \\ : | \" < >"))
  (when (regexp-match
         #rx"^((nul)|(con)|(prn)|(aux)|(clock[$])|(com[1-9])|(lpt[1-9]))[.]?"
         (string-foldcase username))
    (error* "username must not be a Windows special file name"))
  (when (regexp-match #rx"^[ .]|[ .]$" username)
    (error* "username must not begin or end with a space or period"))
  (when (regexp-match #rx"^solution" username)
    (error* "the username prefix \"solution\" is reserved"))
  (when (string=? "checker.rkt" username)
    (error* "the username \"checker.rkt\" is reserved"))
  (when (get-user-data username)
    (error* "username already exists: `~a'" username))
  (for ([str (in-list extra-fields)]
        [info (get-conf 'extra-fields)])
    (check-field str (cadr info) (car info) (caddr info)))
  (wait-for-lock "+newuser+")
  (log-line "create user: ~a" username)
  (hook 'user-create `([username ,username] [fields ,extra-fields]))
  (put-user-data username (cons passwd extra-fields)))

(define (change-user-info data)
  (define usernames    (a-ref data 'usernames))
  (define user-datas   (a-ref data 'user-datas))
  (define passwd       (a-ref data 'new-password))
  (define user-fields  (a-ref data 'user-fields))
  (define extra-fields (add-hidden-to-user-fields user-fields))
  (unless (= 1 (length usernames))
    (error* "cannot change a password for multiple users: ~a" usernames))
  ;; the new data is the same as the old one for every empty string (includes
  ;; hidden fields)
  (let* ([username (car usernames)]
         [old-data (car user-datas)]
         [new-data (map (lambda (old new) (if (equal? "" new) old new))
                        old-data (cons passwd extra-fields))])
    (unless (or (get-conf 'allow-change-info)
                (equal? (cdr new-data) (cdr old-data)))
      (error* "changing information not allowed: ~a" username))
    (when (equal? new-data old-data)
      (error* "no fields changed: ~a" username))
    (for ([str (in-list (cdr new-data))]
          [info (in-list (get-conf 'extra-fields))])
      (check-field str (cadr info) (car info) (caddr info)))
    (log-line "change info for ~a ~s -> ~s" username old-data new-data)
    (unless (equal? (cdr new-data) (cdr old-data)) ; not for password change
      (hook 'user-change `([username ,username]
                           [old ,(cdr old-data)]
                           [new ,(cdr new-data)])))
    (put-user-data username new-data)))

(define (get-user-info data)
  (define usernames  (a-ref data 'usernames))
  (unless (= 1 (length usernames))
    (error* "cannot get user-info for multiple users: ~a" usernames))
  ;; filter out hidden fields
  (let ([all-data (cdar (a-ref data 'user-datas))])
    (filter values (map (lambda (d f)
                          (and (memq f (get-conf 'user-fields)) d))
                        all-data (get-conf 'extra-fields)))))

(define crypt
  (let ([c #f] [sema (make-semaphore 1)])
    ;; use only when needed so it doesn't blow up on non-unix platforms
    (lambda (passwd salt)
      (unless c (set! c (dynamic-require 'ffi/crypt 'crypt)))
      ;; crypt is not reentrant
      (call-with-semaphore sema
        (lambda () (bytes->string/utf-8 (c passwd salt)))))))
(define (has-password? raw md5 passwords)
  (define (good? passwd)
    (define (bad-password msg)
      (log-line "ERROR: ~a -- ~s" msg passwd)
      (error* "bad password in user database"))
    (cond [(string? passwd) (equal? md5 passwd)]
          [(and (list? passwd) (= 2 (length passwd))
                (symbol? (car passwd)) (string? (cadr passwd)))
           (case (car passwd)
             [(plaintext) (equal? raw (cadr passwd))]
             [(unix)
              (let ([salt (regexp-match #rx"^([$][^$]+[$][^$]+[$]|..)"
                                        (cadr passwd))])
                (unless salt (bad-password "badly formatted unix password"))
                (equal? (crypt raw (car salt)) (cadr passwd)))]
             [else (bad-password "bad password type in user database")])]
          [else (bad-password "bad password value in user database")]))
  (or (member md5 passwords) ; very cheap search first
      (ormap good? passwords)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (handle-connection r r-safe w)
  (define msg #f)
  (define active-assignments (assignment-list))
  (define data
    (make-alist 'protocol-data `((assignments . ,(box active-assignments)))))
  (define (perror fmt . args) (apply error 'handin-protocol fmt args))
  (let loop ()
    (set! msg (read r-safe))
    (case msg
      ;; ----------------------------------------
      ;; getting information from the client
      [(set)
       (let* ([key (read r-safe)] [val (read r-safe)])
         (unless (symbol? key) (perror "bad key value: ~e" key))
         (unless (if (eq? 'user-fields key)
                   (and (list? val)
                        (- (length val) (length (get-conf 'user-fields)))
                        (andmap string? val))
                   (string? val))
           (perror "bad value for set: ~e" val))
         (when (a-ref data key #f) (perror "multiple values for ~e" key))
         (case key
           [(username/s)
            (unless (get-conf 'username-case-sensitive)
              (set! val (string-foldcase val)))
            (let ([usernames
                   ;; Username lists must always be sorted, and never empty
                   ;; (regexp-split will not return an empty list)
                   (sort (regexp-split #rx" *[+] *" val) string<?)])
              (a-set! data 'usernames usernames)
              (a-set! data 'user-datas (map get-user-data usernames)))]
           [(password new-password)
            ;; empty passwords are left empty for change-user-info to re-use
            ;; an existing password value
            (when (eq? key 'password) (a-set! data 'raw-password val))
            (unless (equal? "" val) (set! val (md5 val)))]
           [(usernames user-datas raw-password assignments)
            ;; forbid setting these directly
            (perror "bad key for `set': ~e" key)])
         (a-set! data key val))
       (loop)]
      ;; ----------------------------------------
      ;; sending information to the client
      [(get-active-assignments)
       (write+flush w active-assignments)
       (loop)]
      [(get-user-fields)
       (write+flush w (map car (get-conf 'user-fields)))
       (loop)]
      ;; ----------------------------------------
      ;; action handlers
      ;; (don't loop back except get-user-info which needs authorization)
      [(create-user) (add-new-user data)]
      [(bye) #t] ; <- general disconnection
      ;; other messages require a login: valid users and a good password
      [else
       (when (eof-object? msg)
         (let ([username/s (a-ref data 'username/s #f)])
           (apply error 'handin
                  (if username/s `("hangup (~a)" ,username/s) `("hangup")))))
       (let ([usernames  (a-ref data 'usernames #f)]
             [user-datas (a-ref data 'user-datas #f)])
         (when (or (memq #f user-datas)
                   (not (has-password?
                         (a-ref data 'raw-password)
                         (a-ref data 'password)
                         (let ([mp (get-conf 'master-password)]
                               [up (map car user-datas)])
                           (if mp (cons mp up) up)))))
           (log-line "failed login: ~a" (a-ref data 'username/s))
           (error* "bad username or password for ~a"
                   (a-ref data 'username/s)))
         (log-line "login: ~a" usernames)
         (hook 'login `([usernames ,usernames])))
       (case msg
         [(change-user-info) (change-user-info data)]
         [(save-submission) (accept-specific-submission data r r-safe w)]
         [(get-submission) (retrieve-specific-submission data w)]
         [(get-user-info) (write+flush w (get-user-info data)) (loop)]
         [else (perror "bad message `~a'" msg)])]))
  (write+flush w 'ok)) ; final confirmation for *all* actions

(define (assignment-list)
  (map assignment<->dir (get-conf 'active-dirs)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define no-limit-warning? #f) ; will be set to #t if no memory limits

(define current-timeout-control (make-parameter #f))
(provide timeout-control)
(define (timeout-control msg)
  (log-line "timeout-control: ~s" msg)
  ((current-timeout-control) msg))

(define (with-watcher w proc)
  (define session-cust (make-custodian))
  (define session-channel (make-channel))
  (define timeout #f)
  (define status-box (box #f))
  (define (mem m)
    (let loop ([m m] [q 'B] [qs '(KB MB GB TB)])
      (if (and (>= m 1024) (pair? qs))
        (loop (round (/ m 1024)) (car qs) (cdr qs))
        (format "~a~a" m q))))
  (define (watch-loop)
    (define session-thread (channel-get session-channel))
    (let loop ([timed-out? #f])
      (cond [(sync/timeout 3 session-thread)
             (let* ([status (unbox status-box)]
                    [status (if status (format " while ~a" status) "")])
               (log-line "session killed ~a~a"
                         (if timed-out? "(timeout)" "(memory)")
                         status)
               (write+flush
                w (format "handin terminated due to ~a ~a~a"
                          (if timed-out? "time limit" "excessive memory use")
                          "(program doesn't terminate?)"
                          status))
               (close-output-port w)
               (channel-put session-channel 'done))]
            [(let ([t timeout]) ; grab value to avoid races
               (and t ((current-inexact-milliseconds) . > . t)))
             ;; Shutdown here to get the handin-terminated error
             ;;  message, instead of relying on a timeout at the
             ;;  run-server level
             (custodian-shutdown-all session-cust)
             (loop #t)]
            [else (collect-garbage)
                  (log-line "running ~a ~a"
                            (mem (current-memory-use session-cust))
                            (if no-limit-warning?
                              "(total)"
                              (list (mem (current-memory-use orig-custodian))
                                    (mem (current-memory-use)))))
                  (loop #f)])))
  (define (timeout-control msg)
    (if (rational? msg)
      (set! timeout (+ (current-inexact-milliseconds) (* 1000 msg)))
      (case msg
        [(reset) (timeout-control (get-conf 'session-timeout))]
        [(disable #f) (set! timeout #f)]
        [else (error 'timeout-control "bad argument: ~s" msg)])))
  (current-timeout-control timeout-control)
  (timeout-control 'reset)
  (unless no-limit-warning?
    (with-handlers ([exn:fail:unsupported?
                     (lambda (x)
                       (set! no-limit-warning? #t)
                       (log-line "WARNING: per-session memory limit ~a"
                                 "not supported by GRacket"))])
      (custodian-limit-memory
       session-cust (get-conf 'session-memory-limit) session-cust)))
  (let ([watcher (parameterize ([current-custodian orig-custodian])
                   (thread watch-loop))])
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
      (channel-get session-channel))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define web-controller (web:run))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define session-count 0)

(define (handle-handin-request r w)
  (set! connection-num (add1 connection-num))
  (when ((current-memory-use) . > . (get-conf 'session-memory-limit))
    (collect-garbage))
  (parameterize ([current-session
                  (begin (set! session-count (add1 session-count))
                         session-count)])
    (let-values ([(here there) (ssl-addresses r)])
      (log-line "connect from ~a" there)
      (hook 'server-connect `([from ,there])))
    (with-watcher
     w
     (lambda (kill-watcher)
       (let ([r-safe (make-limited-input-port r (* 4 1024))])
         (with-handlers ([exn:fail?
                          (lambda (exn)
                            (let ([msg (if (exn? exn)
                                         (exn-message exn)
                                         (format "~.s" exn))])
                              (kill-watcher)
                              (log-line "ERROR: ~a" msg)
                              (write+flush w msg)
                              ;; see note on close-output-port below
                              (close-output-port w)))])
           ;; Initiate handin protocol (the 'handin token was already peeked)
           (unless (eq? 'handin (read r-safe))
             (error 'handin "internal error, didn't get peeked string"))
           (write+flush w 'handin)
           ;; Check version:
           (let ([ver (read r-safe)])
             (if (eq? 'ver1 ver)
               (write+flush w 'ver1)
               (error 'handin "unknown handin version: ~e" ver)))
           (handle-connection r r-safe w)
           (log-line "normal exit")
           (kill-watcher)))))))

(define (handle-http-request r w)
  (let ([s (make-semaphore 0)])
    (web-controller 'connect r w s)
    (semaphore-wait s)))

(define (handle-*-request r w)
  (let* ([proto (regexp-match-peek #rx#"^[^\r\n]*(?=\r?\n)" r 0 (* 4 1024))]
         [proto (and proto (car proto))])
    ((cond [(not proto) (error 'handin "no protocol line")]
           [(equal? #"handin" proto) handle-handin-request]
           [(regexp-match? #rx#"(?i:http/[0-9.]+)$" proto) handle-http-request]
           [else (error 'handin "unknown protocol: ~e" proto)])
     r w)))

(define default-context-length (error-print-context-length))
(parameterize ([error-display-handler (lambda (msg exn) (log-line msg))]
               [error-print-context-length 0]
               [current-directory server-dir])
  (define port (get-conf 'port-number))
  (define (start-notify)
    (log-line "*** handin server started on port ~a" port)
    (hook 'server-start `([port ,port])))
  (run-server
   port
   (lambda (r w)
     (error-print-context-length default-context-length)
     (handle-*-request r w)
     ;; This close-output-port should not be necessary, and it's here
     ;; due to a deficiency in the SSL binding.  The problem is that a
     ;; custodian shutdown of w is harsher for SSL output than a normal
     ;; close. A normal close flushes an internal buffer that's not
     ;; supposed to exist, while the shutdown gives up immediately.
     (close-output-port w))
   #f ; `with-watcher' handles our timeouts
   (lambda (exn)
     (log-line "ERROR: ~a" (if (exn? exn) (exn-message exn) exn)))
   (lambda (port-k cnt reuse?)
     (let ([l (ssl-listen port-k cnt #t)])
       (ssl-load-certificate-chain! l "server-cert.pem")
       (ssl-load-private-key! l "private-key.pem")
       (start-notify)
       l))
   (lambda (l)
     (log-line "shutting down")
     (web-controller 'shutdown)
     (ssl-close l))
   ssl-accept ssl-accept/enable-break))
