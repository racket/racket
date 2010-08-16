
;; This module implements the mail-reading window as a unit. The
;; unit is instantiated once for each window.

;; General notes:
;;
;;   * Always use `as-background' when communicating with the
;;     server. That way, the user can kill the window if necessary.
;;     use `enable-main-frame' for the first argument to `as-background'.
;;     The `as-background' function is defined in "utilr.ss".
;;

(module readr mzscheme
  (require mzlib/unit
	   mzlib/class
           mzlib/file
	   mred/mred-sig
           framework
           mzlib/process)

  (require mzlib/string
           mzlib/list
	   mzlib/thread
           "spell.ss")

  (require "sirmails.ss")

  (require "pref.ss")

  (require net/imap-sig
	   net/smtp-sig
	   net/head-sig
	   net/base64-sig
	   net/mime-sig
	   net/qp-sig
           browser/htmltext)

  (require mrlib/hierlist/hierlist-sig)

  (require net/sendurl)

  (require openssl/mzssl)

  ;; Constant for messages without a title:
  (define no-subject-string "<No subject>")

  (provide read@)
  (define-unit read@
      (import sirmail:options^
	      sirmail:environment^
	      sirmail:utils^
	      sirmail:send^
	      mred^
	      imap^
	      smtp^
	      head^
	      base64^
	      (prefix mime: mime^)
	      qp^
	      hierlist^)
      (export sirmail:read^)
      
      ;; This will be set to the frame object
      (define main-frame #f)
      (define done? #f)

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Error Handling                                         ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; It's possible that SirMail can't even start with
      ;;  the default preference values. This flag lets us
      ;;  give the user a chance.
      (define got-started? #f)

      (define (show-error x)
	(show-error-message-box x main-frame)
	(when (not got-started?)
	  (when (eq? 'yes (confirm-box "Startup Error"
				       (string-append
					"Looks like you didn't even get started. "
					"Set preferences (so you're ready to try again)?")
				       #f
				       '(app)))
	    (show-pref-dialog))))

      (uncaught-exception-handler
       (lambda (x)
         (show-error x)
	 ((error-escape-handler))))

      ;; Install std bindings global for file dialog, etc.
      (let ([km (make-object keymap%)])
	(add-text-keymap-functions km)
	(keymap:setup-global km)
	(let ([f (current-text-keymap-initializer)])
	  (current-text-keymap-initializer
	   (lambda (k)
	     (send k chain-to-keymap km #f)
	     (f k)))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Mailbox List                                           ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; In case this is the first run...
      (unless (directory-exists? (LOCAL-DIR))
	(make-directory (LOCAL-DIR)))

      ;; The "mailboxes" file tells us where to find local copies
      ;;  of the mailbox content
      (define mailboxes
	(with-handlers ([void (lambda (x) '(("Inbox" #"inbox")))])
	  (with-input-from-file (build-path (LOCAL-DIR) "mailboxes")
	    read)))

      (unless (assoc mailbox-name mailboxes)
	(error 'sirmail "No local mapping for mailbox: ~a" mailbox-name))
      
      (define (string/bytes->path s)
	(if (string? s)
	    (string->path s)
	    (bytes->path s)))

      ;; find the mailbox for this window:
      (define mailbox-dir (build-path (LOCAL-DIR) 
				      (string/bytes->path (cadr (assoc mailbox-name mailboxes)))))
      
      (unless (directory-exists? mailbox-dir)
	(make-directory mailbox-dir))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Message data structure                                 ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; We use a lists so they can be easily read and written
      
      ;; type message = (list ... message attributes, see selectors below ...)

      (define uid-validity #f)

      ;; mailbox : (listof message)
      ;; mailboxes holds the list of messages reflected in the top list
      ;; in the GUI. When modifying this value (usually indirectly), use
      ;; `header-chganging-action'. Mutate the variable, but not the list!
      (define mailbox (let* ([mailbox-file (build-path mailbox-dir "mailbox")]
                             [l (with-handlers ([void (lambda (x)
                                                        (message-box "SirMail"
                                                                     (format
                                                                      "error reading mailbox ~s, ~a\n"
                                                                      mailbox-file
                                                                      (exn-message x)))
                                                        null)])
                                  (with-input-from-file mailbox-file
                                    read))])
                        (when (eof-object? l)
                          (message-box "SirMail" (format "mailbox ~s was eof\n" mailbox-file))
                          (set! l '()))
			;; If the file's list start with an integer, that's
			;;  the uidvalidity value. Otherwise, for backward
			;;  compatibility, we allow the case that it wasn't
			;;  recorded.
			(let ([l (if (and (pair? l)
                                          (or (not (car l)) (integer? (car l))))
                                     (begin
                                       (set! uid-validity (car l))
                                       (cdr l))
                                     l)])
                          ;; Convert each entry to a vector:
                          (map list->vector l))))

      (define mailbox-ht #f)
      (define (rebuild-mailbox-table!)
	(set! mailbox-ht (make-hash-table 'equal))
	(for-each (lambda (m) (hash-table-put! mailbox-ht (vector-ref m 0) m))
		  mailbox))
      (rebuild-mailbox-table!)

      (define (find-message id)
	(hash-table-get mailbox-ht id (lambda () #f)))
      
      (define (message-uid m) (vector-ref m 0))
      (define (message-position m) (vector-ref m 1))
      (define (message-downloaded? m) (vector-ref m 2))
      (define (message-from m) (vector-ref m 3))
      (define (message-subject m) (vector-ref m 4))
      (define (message-flags m) (vector-ref m 5))
      (define (message-size m)
        ;; For backward compatibility:
        (if ((vector-length m) . < . 7)
            #f
            (vector-ref m 6)))
      (define (set-message-position! m v) (vector-set! m 1 v))
      (define (set-message-downloaded?! m v) (vector-set! m 2 v))
      (define (set-message-flags! m v) (vector-set! m 5 v))

      (define (message-marked? m) (memq 'marked (message-flags m)))
      
      (define (write-mailbox)
	(status "Saving mailbox information...")
	(with-output-to-file (build-path mailbox-dir "mailbox")
	  (lambda ()
	    (write (cons uid-validity (map vector->list mailbox))))
	  'truncate))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Connection                                             ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define-values (connect disconnect force-disconnect)
	(let ([connection #f]
	      [connection-custodian #f]
	      [message-count 0])
	  (values
	   (letrec ([connect
		     (case-lambda
                       [() (connect 'reuse)]
		       [(mode) (connect 'reuse void void)]
                       [(mode break-bad break-ok)
			
			(define (with-disconnect-handler thunk)
			  (with-handlers ([void (lambda (exn)
						  (force-disconnect)
						  (status "")
						  (raise exn))])
			    (break-ok)
			    (begin0
			     (thunk)
			     (break-bad))))
	  
	  
                        (if connection
                            
                            ;; Already Connected
                            (cond
                              [(eq? mode 'reselect)
			       (let-values ([(count new) (with-disconnect-handler
							  (lambda ()
							    (imap-noop connection)))])
				 (check-validity (or (imap-uidvalidity connection) 0) void)
				 (values connection (imap-messages connection) (imap-new? connection)))]
			      [(eq? mode 'check-new)
                               (let-values ([(count new) (with-disconnect-handler
							  (lambda ()
							    (imap-noop connection)))])
                                 (values connection message-count (imap-new? connection)))]
			      [else
                               (values connection message-count (imap-new? connection))])
                            
                            ;; New connection
                            (begin
			      (let ([pw (or (get-PASSWORD)
					    (let ([p (get-pw-from-user (USERNAME) main-frame)])
					      (unless p (raise-user-error 'connect "connection canceled"))
					      p))])
				(let*-values ([(imap count new) (let-values ([(server port-no)
									      (parse-server-name (IMAP-SERVER)
										(if (get-pref 'sirmail:use-ssl?) 993 143))])
								  (set! connection-custodian (make-custodian))
								  (parameterize ([current-custodian connection-custodian])
								    (with-disconnect-handler
								     (lambda ()
								       (if (get-pref 'sirmail:use-ssl?)
									   (let ([c (ssl-make-client-context)])
									     (let ([cert (get-pref 'sirmail:server-certificate)])
									       (when cert
										 (ssl-set-verify! c #t)
										 (ssl-load-verify-root-certificates! c cert)))
									     (let-values ([(in out) (ssl-connect server port-no c)])
									       (imap-connect* in out (USERNAME) pw mailbox-name)))
									   (parameterize ([imap-port-number port-no])
									     (imap-connect server (USERNAME) pw mailbox-name)))))))])
                                  (unless (get-PASSWORD)
				    (set-PASSWORD pw))
				  (status "(Connected, ~a messages)" count)
				  (with-disconnect-handler
				   (lambda ()
				     (check-validity (or (imap-uidvalidity imap) 0) 
						     (lambda () (imap-disconnect imap)))))
				  (set! connection imap)
				  (set! message-count count)
				  (send disconnected-msg show #f)
				  (values imap count (imap-new? imap))))))])])
	     connect)
	   (lambda ()
	     (when connection
	       (status "Disconnecting...")
	       (as-background 
		enable-main-frame
		(lambda (break-bad break-ok)
		  (with-handlers ([void (lambda (exn)
					  (force-disconnect/status)
					  (raise exn))])
		    (break-ok)
		    (imap-disconnect connection)))
		close-frame)
	       (status "")
	       (set! connection #f)))
	   (lambda ()
	     (custodian-shutdown-all connection-custodian)
	     (set! connection #f)))))
      
      (define (force-disconnect/status)
	(force-disconnect)
	(send disconnected-msg show #t)
	(set! initialized? #f)
	(set! continue? #f)
	(status ""))

      (define (check-validity v cleanup)
	(when (and uid-validity
		   (not (= uid-validity v))
		   (pair? mailbox))
	  ;; This is really very unlikely, but we checked
	  ;; to guard against disaster.
	  (cleanup)
	  (raise-user-error 'connect "UID validity changed, ~a -> ~a! SirMail can't handle it."
	   uid-validity v))
	(set! uid-validity v))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Mailbox Actions (indepdent of the GUI)                 ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define initialized? #f)
      (define new-messages? #f)
      (define current-count 0)
      (define continue? #f)

      (define (initialized count)
	(set! initialized? #t)
	(set! continue? #t)
	(set! new-messages? #f)
	(set! current-count count)
	(hide-new-mail-msg))

      ;; Syncs `mailbox' with the server
      (define (update-local break-bad break-ok)
	(status "Updating ~a from ~a..." mailbox-name (IMAP-SERVER))
	(let-values ([(imap count new?) (connect 'reselect break-bad break-ok)])
	  (imap-reset-new! imap)
	  (start-biff)
	  (status "Getting message ids...")
	  (let* ([positions (if continue?
				(let ([p (length mailbox)])
				  (map (lambda (i) (+ i p))
				       (enumerate (- count p))))
				(enumerate count))]
		 [data (with-handlers ([void (lambda (exn)
					       (force-disconnect/status)
					       (raise exn))])
			 (break-ok)
			 (begin0
			  (imap-get-messages imap 
					     positions
					     '(uid))
			  (break-bad)))]
		 [uids (map car data)]
		 [curr-uids (map (lambda (m) (vector-ref m 0)) mailbox)]
		 [deleted (if continue?
			      null
			      (remove* uids curr-uids))]
		 [position-uids (map cons uids positions)]
		 [new (if continue?
			  position-uids
			  (remove* curr-uids position-uids
				   (lambda (a b) (equal? a (car b)))))])
	    (status "~a deleted, ~a locally new" (length deleted) (length new))
	    
	    (unless (null? new)
	      (status "Getting new headers..."))
	    (let* ([new-data (with-handlers ([void (lambda (exn)
						     (force-disconnect/status)
						     (raise exn))])
			       (break-ok)
			       (begin0
				(imap-get-messages imap 
						   (map cdr new)
						   '(header size))
				(break-bad)))]
		   [new-headers (map car new-data)]
		   [new-sizes (map cadr new-data)]
		   [new-uid/size-map (map cons (map car new) new-sizes)])
	      (if (and (null? deleted) (null? new))
		  (begin
		    (initialized count)
		    (status "No new messages")
		    #f)
		  (begin
		    (unless (null? deleted)
		      (status "Deleting local messages...")
		      (for-each
		       (lambda (uid)
			 (with-handlers ([void void])
			   (let ([path (build-path mailbox-dir (format "~a" uid))])
			     (delete-file path)
			     (let ([body (string-append path "body")])
			       (when (file-exists? body)
				 (delete-file body))))))
		       deleted))
		    
		    (unless (null? new-headers)
		      (status "Saving new headers...")
		      (for-each
		       (lambda (position-uid header)
			 (with-output-to-file (build-path mailbox-dir (format "~a" (car position-uid)))
			   (lambda ()
			     (display header))
			   'truncate))
		       new new-headers))
		    
		    (set! mailbox 
			  (append
			   (if continue? mailbox null)
			   (map
			    (lambda (uid pos)
			      (let ([old (ormap (lambda (m)
                                                  (and (equal? uid (message-uid m))
                                                       m))
                                                mailbox)])
                                (list->vector
                                 `(,uid ,pos 
                                        ,(if old
                                             (message-downloaded? old)
                                             #f)
                                        ,(if old
                                             (message-from old)
                                             (extract-field "From" (get-header uid)))
                                        ,(if old
                                             (message-subject old)
                                             (extract-field "Subject" (get-header uid)))
                                        ,(if old
                                             (message-flags old)
                                             null)
                                        ,(if old
                                             (message-size old)
                                             (let ([new (assoc uid new-uid/size-map)])
                                               (if new
                                                   (cdr new)
                                                   0)))))))
			    uids positions)))
		    (rebuild-mailbox-table!)
		    (write-mailbox)
		    (initialized count)
		    (display-message-count (length mailbox))
		    (let ([len (length new-headers)])
		      (status "Got ~a new message~a" 
			      len
			      (if (= 1 len) "" "s")))
		    #t))))))
      
      (define (check-for-new break-bad break-ok)
	(status "Checking ~a at ~a..." mailbox-name (IMAP-SERVER))
	(let-values ([(imap count new?) (connect 'check-new break-bad break-ok)])
	  (set! new-messages? new?))
	(if new-messages?
	    (begin
	      (show-new-mail-msg)
	      (status "New mail")
	      #t)
	    (begin
	      (hide-new-mail-msg)
	      (status "No new mail")
	      #f))
	new-messages?)
      
      ;; gets cached header
      (define (get-header uid)
	(let ([file (build-path mailbox-dir (format "~a" uid))])
	  (with-input-from-file file
	    (lambda ()
	      (bytes->string/latin-1
	       (read-bytes (file-size file)))))))
      
      ;; gets cached body or downloads from server (and caches)
      (define (get-body uid break-bad break-ok)
	(let ([v (find-message uid)]
	      [file (build-path mailbox-dir (format "~abody" uid))])
	  (when (not v)
	    (error 'internal-error "unknown message: ~a" uid))
	  (unless (message-downloaded? v)
	    (status "Getting message ~a..." uid)
	    (let ([size (message-size v)]
		  [warn-size (WARN-DOWNLOAD-SIZE)])
	      (when (and size warn-size (> size warn-size))
		(unless (eq? 'yes
			     (confirm-box "Large Message"
					  (format "The message is ~s bytes.\nReally download?" size)
					  main-frame))
		  (status "")
		  (raise-user-error "download aborted"))))
	    (let*-values ([(imap count new?) (connect 'reuse break-bad break-ok)])
	      (let ([body (with-handlers ([void
					   (lambda (exn)
					     (force-disconnect/status)
					     (raise exn))])
			    (break-ok)
			    (begin0
			     (let ([reply (imap-get-messages 
					   imap 
					   (list (message-position v))
					   '(uid body))])
			       (if (equal? (caar reply) (message-uid v))
				   (cadar reply)
				   (raise-user-error (string-append "server UID does not match local UID; "
								    "update the message list and try again"))))
			     (break-bad)))])
		(status "Saving message ~a..." uid)
		(with-output-to-file file
		  (lambda () (write-bytes body))
		  'truncate)
                
		(set-message-downloaded?! v #t)
		(write-mailbox))))
	  (begin0
            (with-input-from-file file
              (lambda ()
                (read-bytes (file-size file))))
            (status ""))))
      
      ;; Checks that `mailbox' is synced with the server
      (define (check-positions imap msgs)
	(status "Checking message mapping...")
	(let ([ids (imap-get-messages imap (map message-position msgs) '(uid))])
	  (unless (equal? (map car ids) (map message-uid msgs))
	    (raise-user-error 
	     'position-check "server's position->id mapping doesn't match local copy. server: ~s local: ~s" 
	     (map car ids) 
	     (map message-uid msgs)))))
      
      (define (remove-delete-flags imap)
	(status "Removing old delete flags...")
	(imap-store imap '- (map message-position mailbox) (list (symbol->imap-flag 'deleted))))
      
      ;; purge-messages : (listof messages) -> void
      (define (purge-messages marked bad-break break-ok)
        (unless (null? marked)
          (let-values ([(imap count new?) (connect)])
	    (with-handlers ([void
			     (lambda (exn)
			       (force-disconnect/status)
			       (raise exn))])
	      (break-ok)
	      (check-positions imap marked)
	      (remove-delete-flags imap)
	      (status "Deleting marked messages...")
	      (imap-store imap '+ (map message-position marked)
			  (list (symbol->imap-flag 'deleted)))
	      (imap-expunge imap)
	      (unless (equal? (imap-get-expunges imap)
			      (map message-position marked))
		(error "expunge notification list doesn't match expunge request"))
	      (bad-break))
            (set! mailbox
                  (filter
                   (lambda (m) (not (memq m marked)))
                   mailbox))
	    (rebuild-mailbox-table!)
            (let loop ([l mailbox][p 1])
              (unless (null? l)
                (set-message-position! (car l) p)
                (loop (cdr l) (add1 p))))
            (write-mailbox)
            (let* ([problems null]
                   [try-delete
                    (lambda (f)
                      (with-handlers ([void 
                                       (lambda (x)
                                         (set! problems (cons x problems)))])
                        (delete-file f)))])
              (for-each
               (lambda (m)
                 (let ([uid (message-uid m)])
                   (try-delete (build-path mailbox-dir (format "~a" uid)))
                   (when (message-downloaded? m)
                     (try-delete (build-path mailbox-dir (format "~abody" uid))))))
               marked)
              (unless (null? problems)
                (message-box "Warning"
                             (apply
                              string-append
                              "There we problems deleting some local files:"
                              (map
                               (lambda (x)
                                 (string-append
                                  (string #\newline)
                                  (if (exn? x)
                                      (exn-message x)
                                      "<unknown exn>")))
			       problems))
                             main-frame))
              (display-message-count (length mailbox))
              (status "Messages deleted")))))

      ;; purge-marked : -> void
      ;; purges the marked mailbox messages.
      (define (purge-marked bad-break break-ok)
	(let* ([marked (filter message-marked? mailbox)])
	  (purge-messages marked bad-break break-ok)))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  GUI: Message List Tools                                ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define FROM-WIDTH 150)
      (define SUBJECT-WIDTH 300)
      (define UID-WIDTH 150)

      ;; update-frame-width : -> void
      ;; updates the green line's width
      ;; preferences's value of sirmail:frame-width must be 
      ;; up to date before calling this function
      (define (update-frame-width)
	(let* ([goofy-margin 15]
	       [calc-w (- (get-pref 'sirmail:frame-width) goofy-margin)])
	  (set! FROM-WIDTH (quotient calc-w 4))
          (set! UID-WIDTH (quotient calc-w 5))
	  (set! SUBJECT-WIDTH (- calc-w FROM-WIDTH UID-WIDTH)))
	
        (when (object? sorting-from-snip)
          (send sorting-from-snip set-min-width FROM-WIDTH)
          (send sorting-from-snip set-max-width FROM-WIDTH)
          (send sorting-uid-snip set-min-width UID-WIDTH)
          (send sorting-uid-snip set-max-width UID-WIDTH)
          (send sorting-subject-snip set-min-width SUBJECT-WIDTH)
          (send sorting-subject-snip set-max-width SUBJECT-WIDTH))
        
        (when (object? header-list)
	  (let ([e (send header-list get-editor)])
	    (send e begin-edit-sequence)
	    (for-each (lambda (item)
			(let* ([e (send item get-editor)]
			       [line-snip
				(let loop ([s (send e find-first-snip)])
				  (cond
                                    [(not s) #f]
                                    [(is-a? s line-snip%) s]
                                    [else (loop (send s next))]))])
			  (send line-snip set-width (+ FROM-WIDTH SUBJECT-WIDTH UID-WIDTH))))
		      (send header-list get-items))
	    (send e end-edit-sequence))))
      
      (update-frame-width)
      
      (define unselected-delta (make-object style-delta% 'change-normal-color))
      (define selected-delta (make-object style-delta%))
      (send selected-delta set-delta-foreground "BLUE")
      
      (define unread-delta (make-object style-delta% 'change-bold))
      (define read-delta (make-object style-delta% 'change-weight 'normal))
      
      (define marked-delta (make-object style-delta% 'change-italic))
      (define unmarked-delta (make-object style-delta% 'change-style 'normal))
      
      (define red-delta (make-object style-delta%))
      (send red-delta set-delta-foreground "red")
      (define green-delta (make-object style-delta%))
      (send green-delta set-delta-foreground "green")

      ;; url-delta :  style-delta
      ;; this is used to higlight urls in the editor window
      (define url-delta (make-object style-delta% 'change-underline #t))
      (send url-delta set-delta-foreground "blue")
      
      (define (apply-style i delta)
        (let ([e (send i get-editor)])
          (send e change-style delta 0 (send e last-position))))

      (define (set-standard-style t s e)
	(send t change-style (send (send t get-style-list) find-named-style "Standard")
	      s e))
      
      (define current-selected #f)
      
      (define (set-current-selected i)
	(unless (eq? current-selected i)
	  (let ([e (send header-list get-editor)])
	    (send e begin-edit-sequence)
	    (when current-selected
	      (apply-style current-selected unselected-delta))
	    (set! current-selected i)
	    (when i
	      (apply-style i selected-delta)
              ; In case we downloaded it just now:
	      (apply-style i read-delta))
	    (send e end-edit-sequence))))
      
      (define vertical-line-snipclass
	(make-object
            (class snip-class% ()
              (define/override (read s)
                (make-object vertical-line-snip%))
              (super-instantiate ()))))
      (send vertical-line-snipclass set-version 1)
      (send vertical-line-snipclass set-classname "sirmail:vertical-line%")
      (send (get-the-snip-class-list) add vertical-line-snipclass)
      (define body-pen (send the-pen-list find-or-create-pen "blue" 0 'solid))
      (define body-brush (send the-brush-list find-or-create-brush "WHITE" 'solid))
      (define vertical-line-snip%
	(class snip%
          (inherit set-snipclass get-style get-admin)
          (field
           [width 15]
           [height 10])
          [define/override get-extent
            (lambda (dc x y w-box h-box descent-box space-box lspace-box rspace-box)
              (for-each (lambda (box) (when box (set-box! box 0)))
                        (list w-box h-box lspace-box rspace-box))
              (let ([old-font (send dc get-font)])
                (send dc set-font (send (get-style) get-font))
                (let-values ([(w h descent ascent)
                              (send dc get-text-extent "yxX")])
                  (when w-box
                    (set-box! w-box width))
                  
                  ;; add one here because I know the descent for the entire
                  ;; line is going to be one more than the descent of the font.
                  (when descent-box
                    (set-box! descent-box (+ descent 1)))
                  
                  (when space-box
                    (set-box! space-box ascent))
                  (let ([text (and (get-admin)
                                   (send (get-admin) get-editor))])
                    
                    ;; add 2 here because I know lines are two pixels taller
                    ;; than the font. How do I know? I just know.
                    (set! height (+ h 2))
                    (when h-box
                      (set-box! h-box (+ h 2)))
                    
                    (send dc set-font old-font)))))]
          [define/override draw
            (lambda (dc x y left top right bottom dx dy draw-caret)
              (let ([orig-pen (send dc get-pen)]
                    [orig-brush (send dc get-brush)])
                (send dc set-pen body-pen)
                (send dc set-brush body-brush)
                
                (send dc draw-line 
                      (+ x (quotient width 2))
                      y
                      (+ x (quotient width 2))
                      (+ y (- height 1)))
                
                (send dc set-pen orig-pen)
                (send dc set-brush orig-brush)))]
          [define/override write
            (lambda (s)
              (void))]
          [define/override copy
            (lambda ()
              (let ([s (make-object vertical-line-snip%)])
                (send s set-style (get-style))
                s))]
          (super-instantiate ())
          (set-snipclass vertical-line-snipclass)))
      
      (define common-style-list #f)
      (define (single-style t)
	;; Commented out for now:
	'(if common-style-list
	     (send t set-style-list common-style-list)
	     (set! common-style-list (send t get-style-list)))
	t)
      
      (define (make-field w)
	(let ([m (instantiate editor-snip% ()
                   (editor (single-style (let ([e (make-object text% 0.0)])
                                           (send e set-keymap #f)
                                           (send e set-max-undo-history 0)
                                           e)))
                   (with-border? #f)
                   (top-margin 1)
                   (top-inset 1)
                   (bottom-margin 1)
                   (bottom-inset 1)
                   (min-width w)
                   (max-width w))])
	  (send m set-flags (remove 'handles-events (send m get-flags)))
	  m))
      
      (define first-gap 35)
      (define second-gap 15)
      (define line-space 8)
      (define extra-height 2)
      (define left-edge-space 2)

      (define line-snip%
        (class snip%
          (init-field from subject uid)
          (define/override (draw dc x y left top bottom right dx dy draw-caret)
            (let ([w (get-width)])
              (let-values ([(_1 h _2 _3) (send dc get-text-extent "yX")])
                
                (let* ([old-clip (send dc get-clipping-region)]
                       [new-clip #f]
                       [set-clip
                        (lambda (x y w h)
                          (if old-clip
                              (begin
                                (send dc set-clipping-region #f)
                                (unless new-clip
                                  (set! new-clip (make-object region% dc)))
                                (send new-clip set-rectangle x y w h)
                                (send new-clip intersect old-clip)
                                (send dc set-clipping-region new-clip))
                              (send dc set-clipping-rect x y w h)))])
                  (set-clip x y (+ FROM-WIDTH (/ first-gap 2) (- line-space)) h)
                  (send dc draw-text from (+ x left-edge-space) y #t)
                  (set-clip (+ x FROM-WIDTH (/ first-gap 2) line-space)
                            y 
                            (+ SUBJECT-WIDTH (/ second-gap 2) (- line-space))
                            h)
                  (send dc draw-text subject (+ x FROM-WIDTH (/ first-gap 2) line-space) y #t)
                  (send dc set-clipping-region old-clip)
                  (send dc draw-text
                        uid 
                        (+ x FROM-WIDTH first-gap SUBJECT-WIDTH (/ second-gap 2) line-space)
                        y
			#t))
                  
		(let ([p (send dc get-pen)])
		  (send dc set-pen body-pen)
		  (send dc draw-line 
			(+ x FROM-WIDTH (/ first-gap 2))
			y
			(+ x FROM-WIDTH (/ first-gap 2))
			(+ y h extra-height))
		  (send dc draw-line 
			(+ x FROM-WIDTH first-gap SUBJECT-WIDTH (/ second-gap 2))
			y
			(+ x FROM-WIDTH first-gap SUBJECT-WIDTH (/ second-gap 2))
			(+ y h extra-height))
		  (send dc set-pen p)))))

          (inherit get-style)
          (define/override (get-extent dc x y wb hb db sb lb rb)
            (let-values ([(w h d s) (send dc get-text-extent "yX" (send (get-style) get-font))])
              (set-box/f! hb (+ extra-height h))
              (set-box/f! wb (get-width))
	      (set-box/f! db d)
	      (set-box/f! sb s)
	      (set-box/f! lb 2)
	      (set-box/f! rb 0)))

          (inherit get-admin)
          
          (field [width 500])
          (define/public (set-width w) 
            (let ([admin (get-admin)])
              (when admin
                (send admin resized this #t)))
            (set! width w))
          (define/private (get-width) width)
          (super-new)))

      (define (set-box/f! b v) (when (box? b) (set-box! b v)))
      
      (define (add-message m)
        (let* ([i (send header-list new-item)]
	       [e (send i get-editor)]
               [one-line
                (lambda (s)
		  (regexp-replace* #rx"[ \r\n\t]+" s " "))]
	       [snip (new line-snip% 
                          (from
                           (one-line (or (parse-encoded (message-from m))
                                         "<unknown>")))
                          (subject
                           (one-line (or (parse-encoded (message-subject m))
                                         no-subject-string)))
                          (uid (format "~a" (message-uid m))))]
               [before (send e last-position)])
          (send e begin-edit-sequence)
          (send i user-data (message-uid m))
          (send e set-line-spacing 0)
          (send snip set-width (+ FROM-WIDTH SUBJECT-WIDTH UID-WIDTH))
	  (send e insert snip)
          (unless (message-downloaded? m)
            (send e change-style unread-delta before (+ before 1)))
          (when (memq 'marked (message-flags m))
            (send e change-style marked-delta before (+ before 1)))
          (send e end-edit-sequence)
          i))

      (define display-text%
        (html-text-mixin
         (text:foreground-color-mixin
          text:standard-style-list%)))
      
      ;; Class for the panel that has columns titles and
      ;; supports clicks to change the sort order
      (define sorting-list%
        (class hierarchical-list%
          (inherit get-editor selectable set-no-sublists)
          
          (define/private (find-sorting-key evt)
            (let loop ([editor (get-editor)])
              (when editor
                (let ([xb (box (send evt get-x))]
                      [yb (box (send evt get-y))])
                  (send editor global-to-local xb yb)
                  (let* ([pos (send editor find-position (unbox xb) (unbox yb))]
                         [snip (send editor find-snip pos 'after-or-none)])
                    (cond
                      [(eq? snip sorting-from-snip) 'from]
                      [(eq? snip sorting-subject-snip) 'subject]
                      [(eq? snip sorting-uid-snip) 'uid]
                      [(is-a? snip editor-snip%)
                       (loop (send snip get-editor))]
                      [else #f]))))))

	  (define tracking #f)
	  (define tracking-on? #f)
          
          (define/override (on-event evt)
            (cond
	      [(send evt button-down?)
	       (set! tracking (find-sorting-key evt))
	       (if tracking
		   (begin
		     (set! tracking-on? #t)
		     (reset-sorting-tracking)
		     (set-sorting-tracking tracking))
		   (begin
		     (set! tracking-on? #f)
		     (reset-sorting-tracking)))]
              [(and tracking
		    (send evt button-up?))
               (let ([sorting-key (find-sorting-key evt)]
		     [was-tracking tracking])
		 (set! tracking #f)
		 (set! tracking-on? #f)
		 (and (eq? sorting-key was-tracking)
		      (case sorting-key
			[(from) (sort-by-sender)]
			[(subject) (sort-by-subject)]
			[(uid) (sort-by-order-received)])))
	       (reset-sorting-tracking)]
	      [(and tracking
		    (send evt dragging?))
	       (let ([sorting-key (find-sorting-key evt)])
		 (if (eq? sorting-key tracking)
		     (unless tracking-on?
		       (set! tracking-on? #t)
		       (reset-sorting-tracking)
		       (set-sorting-tracking tracking))
		     (when tracking-on?
		       (set! tracking-on? #f)
		       (reset-sorting-tracking))))]
	      [tracking
	       (set! tracking #f)
	       (set! tracking-on? #f)
	       (reset-sorting-tracking)]))

          (super-new (style '(hide-hscroll hide-vscroll)))
	  (set-no-sublists #t)
	  (selectable #f)))
           
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  GUI: Frame, Menus, & Key Bindings                      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; Message display modes
      (define show-full-headers? #f)
      (define quote-in-reply? #t)
      (define mime-mode? #t)
      (define no-mime-inline? #f)
      (define html-mode? #t)
      (define img-mode? #f)
      (define prefer-text? (get-pref 'sirmail:prefer-text))
      
      (define global-keymap (make-object keymap%))
      (send global-keymap add-function "new-mailer"
	    (lambda (w e) (start-new-mailer #f "" "" "" "" "" null)))
      (send global-keymap add-function "disconnect"
	    (lambda (w e)
	      (disconnect)
	      (force-disconnect/status)))
      (send global-keymap add-function "get-new-mail"
	    (lambda (w e) (get-new-mail)))
      (send global-keymap add-function "archive-current"
            (lambda (w e) (send header-list archive-current-message)))
      (send global-keymap add-function "prev-msg"
	    (lambda (w e) (send header-list select-prev)))
      (send global-keymap add-function "next-msg"
	    (lambda (w e) (send header-list select-next)))
      (send global-keymap add-function "mark-msg"
	    (lambda (w e) (send header-list mark-message #t)))
      (send global-keymap add-function "unmark-msg"
	    (lambda (w e) (send header-list unmark-message #t)))
      (send global-keymap add-function "hit-msg"
	    (lambda (w e) (send header-list hit)))
      (send global-keymap add-function "scroll-down"
	    (lambda (w e) 
	      (if (send header-list selected-hit?)
		  (let*-values ([(e) (send message get-editor)]
				[(x y) (send e editor-location-to-dc-location 0 0)])
		    (send e move-position 'down #f 'page)
		    (let*-values ([(x2 y2) (send e editor-location-to-dc-location 0 0)])
		      (when (= y y2)
			(let ([current (send header-list get-selected)])
			  (send header-list select-next)
			  (unless (eq? current (send header-list get-selected))
			    (send header-list hit))))))
		  (send header-list hit))))
      (send global-keymap add-function "scroll-up"
	    (lambda (w e) 
	      (when (send header-list selected-hit?)
		(let ([e (send message get-editor)])
		  (send e move-position 'up #f 'page)))))
      (send global-keymap add-function "rewind-msg"
	    (lambda (w e) (send header-list rewind-selected)))
      (send global-keymap add-function "forward-msg"
	    (lambda (w e) (send header-list forward-selected)))      
      (send global-keymap add-function "purge"
	    (lambda (w e) 
	      (purge-marked/update-headers)))
      (send global-keymap add-function "gc"
	    (lambda (w e) (collect-garbage) (collect-garbage) (dump-memory-stats)))
      (send global-keymap add-function "show-memory-graph"
	    (lambda (w e) (show-memory-graph)))
      
      (send global-keymap map-function ":m" "new-mailer")
      (send global-keymap map-function ":g" "get-new-mail")
      (send global-keymap map-function ":a" "archive-current")
      (send global-keymap map-function ":i" "disconnect")
      (send global-keymap map-function ":n" "next-msg")
      (send global-keymap map-function ":p" "prev-msg")
      (send global-keymap map-function ":return" "hit-msg")
      (send global-keymap map-function ":d" "mark-msg")
      (send global-keymap map-function ":u" "unmark-msg")
      (send global-keymap map-function ":space" "scroll-down")
      (send global-keymap map-function ":b" "scroll-up")
      (send global-keymap map-function "#" "purge")
      (send global-keymap map-function "!" "gc")
      (send global-keymap map-function ":z" "show-memory-graph")
      (send global-keymap map-function ":m:left" "rewind-msg")
      (send global-keymap map-function ":d:left" "rewind-msg")
      (send global-keymap map-function ":m:right" "forward-msg")
      (send global-keymap map-function ":d:right" "forward-msg")
      
      (define icon (make-object bitmap% (build-path (collection-path "sirmail")
						    "postmark.bmp")))
      (define icon-mask (make-object bitmap% (build-path (collection-path "sirmail")
							 "postmark-mask.xbm")))
      (unless (and (send icon ok?)
		   (send icon-mask ok?))
	(set! icon #f))
      
      (define sm-super-frame%
        (frame:standard-menus-mixin
         frame:basic%))
      
      (define sm-frame%
	(class sm-super-frame%
          (inherit get-menu-bar set-icon)

          (define/override (file-menu:create-new?) #f)
          (define/override (file-menu:create-open?) #f)
          (define/override (file-menu:create-open-recent?) #f)

          ;; -------------------- File Menu --------------------
          
          (define/override (file-menu:between-save-as-and-print file-menu)
            (make-object menu-item% "&Get New Mail" file-menu
              (lambda (i e) (get-new-mail))
              #\g)
            (make-object menu-item% "&Download All" file-menu
              (lambda (i e) (download-all))
              #\l)
            (make-object menu-item% "Archive Message" file-menu
              (lambda (i e)  (send header-list archive-current-message)))
            (make-object separator-menu-item% file-menu)
            (make-object menu-item%
              "&Open Folders List"
              file-menu
              (lambda (x1 x2) (open-folders-window)))
            (make-object separator-menu-item% file-menu)
            (make-object menu-item% "&New Message" file-menu
              (lambda (i e) (start-new-mailer #f "" "" "" "" "" null))
              #\m)
            (make-object menu-item% "&Resume Message..." file-menu
              (lambda (i e) 
                (let ([file (get-file "Select message to resume"
                                      main-frame)])
                  (when file
                    (start-new-mailer file "" "" "" "" "" null)))))
            (instantiate menu-item% () 
              (label "Send Queued Messages")
              (parent file-menu)
              (demand-callback
               (lambda (menu-item) 
                 (send menu-item enable (enqueued-messages?))))
              (callback
               (lambda (i e)
                 (send-queued-messages))))
            
            (make-object separator-menu-item% file-menu)
            (make-object menu-item% "&Save Message As..." file-menu
              (lambda (i e)
                (let ([f (put-file "Save message to"
                                   main-frame)])
                  (when f
                    (send (send message get-editor) save-file f 'text))))))
          
          (define/override (file-menu:create-print?) #t)
          (define/override (file-menu:print-callback i e)
            (send (send message get-editor) print))
          
          (define/override (file-menu:between-print-and-close file-menu)
            (make-object separator-menu-item% file-menu)
            (make-object menu-item% "D&isconnect" file-menu
              (lambda (i e) 
                (disconnect)
		(force-disconnect/status))
              #\i))
          
          (define/override (file-menu:close-callback i e) (send main-frame on-close))
          (define/override (file-menu:create-quit?) #f)
          
          ;; -------------------- Help Menu --------------------
          
          (define/override (help-menu:after-about menu)
            (make-object menu-item% "&Help" menu
              (lambda (i e)
                (let* ([f (instantiate frame% ("Help")
                            [width 500]
                            [height 300])]
                       [e (make-object text%)]
                       [c (make-object editor-canvas% f e)])
                  (send e load-file
                        (build-path (collection-path "sirmail")
                                    "doc.txt"))
                  (send f show #t))))
            (super help-menu:after-about menu))
          
          ;; -------------------- Misc. --------------------
          
          (inherit get-edit-target-object)
          
          [define/override on-size
            (lambda (w h)
              (put-pref 'sirmail:frame-width w)
              (put-pref 'sirmail:frame-height h)
              (update-frame-width)
              (super on-size w h))]
          [define/augment can-close? (lambda () 
				       (and (send (get-menu-bar) is-enabled?)
					    (inner #t can-close?)))]
          [define/augment on-close (lambda () 
				     (logout)
                                     (set! done? #t)
				     (inner (void) on-close))]
          [define/override on-subwindow-char
            (lambda (w e)
              (or (and
                   (send (send main-frame get-menu-bar) is-enabled?)
                   (or (send global-keymap handle-key-event w e)
                       (and (eq? #\tab (send e get-key-code))
                            (member w (list header-list message))
                            (send (if (eq? w message)
                                      header-list
                                      message)
                                  focus))))
                  (super on-subwindow-char w e)))]
          (super-instantiate ())
          (when icon
            (set-icon icon icon-mask 'both))))

      ;; -------------------- Frame Creation --------------------
      
      (set! main-frame (make-object sm-frame% mailbox-name #f 
				    (get-pref 'sirmail:frame-width)
				    (get-pref 'sirmail:frame-height)))
      (define mb (send main-frame get-menu-bar))

      ;; -------------------- Message Menu --------------------
      
      (define msg-menu (make-object menu% "&Message" mb))
      
      (make-object menu-item% "&Reply" msg-menu
		   (lambda (i e) (do-reply #f quote-in-reply?))
		   #\R)
      (make-object menu-item% "&Follow Up" msg-menu
        (lambda (i e) (do-reply #t quote-in-reply?))
        #\t)
      (make-object menu-item% "F&orward" msg-menu
        (lambda (i e) (do-forward))
        #\W)
      (send (make-object checkable-menu-item% "&Quote Original" msg-menu
              (lambda (item e)
                (set! quote-in-reply? (send item is-checked?))))
	    check #t)
      (make-object separator-menu-item% msg-menu)
      (make-object menu-item% "&Mark Selected" msg-menu
		   (lambda (i e)
		     (send header-list mark-message #t))
		   #\D)
      (make-object menu-item% "&Unmark Selected" msg-menu
        (lambda (i e)
          (send header-list unmark-message #t))
        #\U)
      (define (mark-all mark? between?)
	(let* ([marked-uids (map message-uid (filter (if mark?
							 (lambda (x) (not (message-marked? x)))
							 message-marked?)
						     mailbox))]
	       [items (send header-list get-items)]
	       [selected (send header-list get-selected)])
	  (for-each
	   (lambda (i)
	     (when (member (send i user-data) marked-uids)
	       (send i select #t)
	       (if mark?
		   (send header-list mark-message #f)
		   (send header-list unmark-message #f))))
           (if between?
               (let ([drop-some
                      (lambda (items)
                        (let loop ([items items])
                          (if (null? items)
                              null
                              (if (message-marked? (find-message (send (car items) user-data)))
                                  items
                                  (loop (cdr items))))))])
                 (reverse (drop-some (reverse (drop-some items)))))
               items))
          (write-mailbox)
          (status "~aarked all" (if mark? "M" "Unm"))
	  (if selected
	      (send selected select #t)
	      (send (send header-list get-selected) select #f))))
      
      (make-object menu-item% "Mark All" msg-menu
        (lambda (i e) (mark-all #t #f)))
      (make-object menu-item% "Unmark All" msg-menu
        (lambda (i e) (mark-all #f #f)))
      (make-object menu-item% "Mark All Between Marked" msg-menu
        (lambda (i e) (mark-all #t #t)))
      
      (make-object separator-menu-item% msg-menu)
      (make-object menu-item% "&Delete Marked" msg-menu
        (lambda (i e)
          (when (eq? 'yes
                     (confirm-box
                      "Delete Marked?"
                      "Really delete the marked messages?"
                      main-frame))
            (purge-marked/update-headers))))

      (make-object (class menu-item%
                     (inherit enable set-label)
                     (define/override (on-demand)
                       (let ([folder (get-active-folder)])
                         (enable folder)
                         (when folder
                           (set-label (format "&Copy Marked to ~a" folder)))))
                     (super-instantiate ()))
        "&Copy Marked to Selected Folder" 
        msg-menu
        (lambda x
          (let ([mbox (get-active-folder)])
            (if mbox
                (copy-marked-to mbox)
                (bell)))))
      
      (make-object separator-menu-item% msg-menu)
      (define sort-menu (make-object menu% "&Sort" msg-menu))
      (let ([m (make-object menu% "Decode" msg-menu)])
        (letrec ([switch (lambda (item e)
                           (if (send item is-checked?)
                               (begin
                                 ;; Disable others:
                                 (send raw check (eq? raw item))
                                 (send mime-lite check (eq? mime-lite item))
				 (send mime check (eq? mime item))
                                 (send html check (eq? html item))
                                 (send img check (eq? img item))
                                 ;; Update flags
                                 (set! mime-mode? (or (send mime is-checked?)
						      (send mime-lite is-checked?)
                                                      (send html is-checked?)
                                                      (send img is-checked?)))
				 (set! no-mime-inline? (or (send mime-lite is-checked?)))
                                 (set! html-mode? (or (send html is-checked?)
                                                      (send img is-checked?)))
                                 (set! img-mode? (send img is-checked?))
                                 ;; Re-decode
                                 (redisplay-current))
                               ;; Turn it back on
                               (send item check #t)))]
                 [raw (make-object checkable-menu-item% "&Raw" m switch)]
                 [mime-lite (make-object checkable-menu-item% "MIME &without Inline" m switch)]
                 [mime (make-object checkable-menu-item% "&MIME" m switch)]
                 [html (make-object checkable-menu-item% "MIME and &HTML" m switch)]
                 [img (make-object checkable-menu-item% "MIME, HTML, and &Images" m switch)])
          (send (if (and mime-mode? html-mode?)
                    html
                    (if mime-mode?
                        mime
                        raw))
                check #t)
	  (make-object separator-menu-item% m)
	  (send (make-object checkable-menu-item% "Prefer &Text" m
			     (lambda (i e)
			       (put-pref 'sirmail:prefer-text (send i is-checked?))
			       (set! prefer-text? (send i is-checked?))
			       (redisplay-current)))
		check prefer-text?)))
      (define wrap-lines-item
	(make-object checkable-menu-item% "&Wrap Lines" msg-menu
		     (lambda (item e)
		       (put-pref 'sirmail:wrap-lines (send item is-checked?))
		       (send (send message get-editor) auto-wrap
			     (send item is-checked?)))))
      (make-object checkable-menu-item% "&View Full Header" msg-menu
        (lambda (i e)
          (set! show-full-headers? (send i is-checked?))
	  (redisplay-current)))

      (make-object menu-item% "by From" sort-menu (lambda (i e) (sort-by-sender)))
      (make-object menu-item% "by Subject" sort-menu (lambda (i e) (sort-by-subject)))
      (make-object menu-item% "by Date" sort-menu (lambda (i e) (sort-by-date)))
      (make-object menu-item% "by Order Received" sort-menu (lambda (i e) (sort-by-order-received)))
      (make-object menu-item% "by Size" sort-menu (lambda (i e) (sort-by-size)))
      (make-object menu-item% "by Header Field..." sort-menu (lambda (i e) (sort-by-header-field)))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  GUI: Message List                                      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define drag-cursor (make-object cursor% 'hand))      
      (define plain-cursor (make-object cursor% 'arrow))
      (define arrow+watch-cursor (make-object cursor% 'arrow+watch))
      
      (define header-list%
	(class hierarchical-list%

          (inherit get-items show-focus set-cursor select)
          (field [selected #f])
          
          (define/public (mark marked? update?)
            (when selected
              (let* ([uid (send selected user-data)]
                     [m (find-message uid)]
                     [flags (message-flags m)])
                (unless (eq? (not marked?) 
                             (not (memq 'marked flags)))
                  (set-message-flags! m (if marked?
                                            (cons 'marked flags)
                                            (remq 'marked flags)))
                  (when update?
                    (write-mailbox))
                  (apply-style selected 
                               (if marked? 
                                   marked-delta
                                   unmarked-delta))
                  (when update?
                    (status "~aarked" 
                            (if marked? "M" "Unm")))))))
          
          (define/public (archive-current-message)
            (when selected
              (let ([archive-mailbox (ARCHIVE-MAILBOX)])
                (when archive-mailbox
                  (let* ([uid (send selected user-data)]
                         [item (find-message uid)])
                    (header-changing-action
                     #f
                     (lambda ()
                       (as-background
                        enable-main-frame
                        (lambda (bad-break break-ok)
                          (with-handlers ([void no-status-handler])
                            (copy-messages-to (list item) archive-mailbox)
                            (purge-messages (list item) bad-break break-ok)))
                        close-frame))))))))
          
          (define/public (hit)
            (when selected
              (on-double-select selected)))
          
          (define/public (mark-message update?)
            (mark #t update?))
          (define/public (unmark-message update?)
            (mark #f update?))
          (define/public (selected-hit?) (eq? selected current-selected))
          (define/override (on-select i)
            (set! selected i))

          ;; -------------------- Message selection --------------------
          
	  (define past-selected null)
	  (define future-selected null)

	  (define/private (push-selected uid)
	    (unless (and (pair? past-selected)
			 (equal? uid (car past-selected)))
	      (set! future-selected (remove uid future-selected))
	      (set! past-selected (cons uid (remove uid past-selected)))))

	  (define/public (rewind-selected)
	    (when (pair? past-selected)
	      (set! future-selected (cons (car past-selected)
					  future-selected))
	      (set! past-selected (cdr past-selected)))
	    (unless (pair? past-selected)
	      (set! past-selected (reverse future-selected))
	      (set! future-selected null))
	    (set! past-selected (select-from-stack past-selected)))

	  (define/public (forward-selected)
	    (unless (pair? future-selected)
	      (set! future-selected (reverse past-selected))
	      (set! past-selected null))
	    (set! future-selected (select-from-stack future-selected))
	    (when (pair? future-selected)
	      (set! past-selected (cons (car future-selected)
					past-selected))
	      (set! future-selected (cdr future-selected))))

	  (define (select-from-stack selected)
	    (if (pair? selected)
		(let* ([uid (car selected)]
		       [i (ormap (lambda (i)
				   (and (equal? uid (send i user-data))
					i))
				 (send header-list get-items))])
		  (if i
		      (begin
			(select i)
			(do-double-select i #f)
			selected)
		      (select-from-stack (cdr selected))))
		null))

          (define/override (on-double-select i)
	    (do-double-select i #t))
	  
	  (define/private (do-double-select i push?)
            (let ([e (send message get-editor)]
                  [uid (send i user-data)])
              (dynamic-wind
               (lambda ()
                 (send e lock #f)
                 (send e begin-edit-sequence))
               (lambda ()
                 (send e erase)
                 (set-current-selected #f)
                 (let* ([h (get-header uid)]
			[small-h (get-viewable-headers h)])
                   (send e insert 
			 (string-crlf->lf small-h)
                         0 'same #f)
		   ;; Do the body (possibly mime)
		   (let ([body (as-background 
				enable-main-frame
				(lambda (break-bad break-ok) 
				  (get-body uid break-bad break-ok))
				close-frame)]
			 [insert (lambda (body delta)
				   (let ([start (send e last-position)])
				     (send e set-position start)
				     (send e insert 
					   body
					   start 'same #f)
				     (let ([end (send e last-position)])
				       (delta e start end))))])
		     (when push?
		       (push-selected uid))
		     (parse-and-insert-body h body e insert 78 img-mode?)))
                 (send e set-position 0)
                 (set-current-selected i))
               (lambda ()
                 (send e end-edit-sequence)
                 (send e lock #t)))))

          ;; -------------------- Message drag'n'drop --------------------
          
          (inherit get-editor client->screen)
          (field (dragging-item #f)
                 (dragging-title #f)
                 (last-status #f)
                 (drag-start-x 0)
                 (drag-start-y 0))
          (define/override (on-event evt)
            (cond
              [(send evt button-down?)
               (when dragging-item
                 (status "")
                 (send (get-editor) set-cursor plain-cursor)
                 (set! dragging-item #f))
               (let ([text (get-editor)])
                 (when text
                   (let ([xb (box (send evt get-x))]
                         [yb (box (send evt get-y))])
                     (send text global-to-local xb yb)
                     (let* ([pos (send text find-position (unbox xb) (unbox yb))]
                            [snip (send text find-snip pos 'after-or-none)]
                            [item (and (is-a? snip hierarchical-item-snip%)
                                       (send snip get-item))])
                       (set! dragging-title "???")
                       (set! dragging-item item)
                       (set! drag-start-x (send evt get-x))
                       (set! drag-start-y (send evt get-y))
                       (when dragging-item
                         (let* ([ud (send dragging-item user-data)]
                                [message (find-message ud)]
                                [cap-length 50])
                           (when message
                             (let ([title (message-subject message)])
                               (cond
                                 [(not title) (set! dragging-title no-subject-string)]
                                 [((string-length title) . <= . cap-length)
                                  (set! dragging-title title)]
                                 [else
                                  (set! dragging-title
                                        (string-append (substring title 0 (- cap-length 3)) "..."))])))))))))]
              [(send evt dragging?)
	       (when dragging-item
                 (when (or ((abs (- (send evt get-x) drag-start-x)) . > . 5)
                           ((abs (- (send evt get-y) drag-start-y)) . > . 5))
                   (send (get-editor) set-cursor drag-cursor))
		 (let-values ([(gx gy) (client->screen (send evt get-x) (send evt get-y))])
		   (let ([mailbox-name (send-message-to-window gx gy (list gx gy))])
		     (if (string? mailbox-name)
			 (status "Move message \"~a\" to ~a" dragging-title mailbox-name)
			 (status "")))))]
              [(send evt button-up?)
	       (when dragging-item
                 (send (get-editor) set-cursor plain-cursor)
		 (let-values ([(gx gy) (client->screen (send evt get-x) (send evt get-y))]
			      [(ditem) dragging-item])
		   (set! dragging-item #f)
		   (let ([mailbox-name (send-message-to-window gx gy (list gx gy))])
                     (if (bytes? mailbox-name)
                         (let* ([user-data (send ditem user-data)]
                                [item (find-message user-data)])
                           (when item
			     (header-changing-action
			      #f
			      (lambda ()
				(as-background
				 enable-main-frame
				 (lambda (bad-break break-ok)
				   (with-handlers ([void no-status-handler])
                                     (void)
				     (copy-messages-to (list item) mailbox-name)
                                     (purge-messages (list item) bad-break break-ok)))
				 close-frame)))))
                         (status "")))))]
              [else
               (when (and dragging-item
			  (not (and (or (send evt leaving?)
					(send evt entering?))
				    (or (send evt get-left-down)
					(send evt get-middle-down)
					(send evt get-right-down)))))
                 (set! dragging-item #f)
                 (send (get-editor) set-cursor plain-cursor)
                 (status ""))])

            (super on-event evt))

	  (super-new (style '(no-hscroll)))
          (show-focus #t)))
      
      ;; header-changing-action: bool thunk -> thunk-result
      ;; Use this function to bracket operations that change
      ;; `mailbox'. It will use before an after values to update
      ;; the message list.
      (define (header-changing-action downloads? go)
	(let ([old-mailbox mailbox])
	  (dynamic-wind
	   void
	   go
	   (lambda ()
	     (let ([items (send header-list get-items)]
		   [selected (send header-list get-selected)]
		   [need-del-selection? #f]
		   [set-selection? #f])
	       (send (send header-list get-editor) begin-edit-sequence)
	       (for-each
		(lambda (i)
		  (let ([a (find-message (send i user-data))])
		    (if a
			(begin ; Message still here
			  (when (and downloads? (message-downloaded? a))
				(apply-style i read-delta))
			  (when need-del-selection?
				(set! need-del-selection? #f)
				(send i select #t)))
			(begin ; Message gone
			  (when (eq? i selected)
				(set! need-del-selection? #t))
			  (when (eq? i current-selected)
				(let ([e (send message get-editor)])
				  (send e begin-edit-sequence)
				  (send e lock #f)
				  (send e erase)
				  (send e lock #t)
				  (send e end-edit-sequence))
				(set-current-selected #f))
			  (send header-list delete-item i)))))
		items)
               (let ([old-ids (make-hash-table 'equal)])
                 (for-each (lambda (m)
                             (hash-table-put! old-ids (message-uid m) #t))
                           old-mailbox)
                 (for-each
                  (lambda (m)
                    (unless (hash-table-get old-ids (message-uid m) #f)
                      (let ([i (add-message m)])
                        (unless set-selection?
                          (set! set-selection? #t)
                          (send i select #t)
                          (send i scroll-to)))))
                  mailbox))
	       (send (send header-list get-editor) end-edit-sequence))))))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  GUI: Message Operations                                ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;;  Closes connections and  terminates the window
      (define (logout)
	(with-handlers ([void
			 (lambda (x)
			   (show-error x)
			   (when (eq? 'yes
				      (confirm-box
				       "Error"
				       "There was an error disconnecting. Exit anyway?"
				       main-frame))
			     (exit-sirmail "read-window exception (1)")
			     (send main-frame show #f)))])
	  (disconnect)
	  (when biff (send biff stop))
	  (exit-sirmail "read-window exception (2)")
	  (send main-frame show #f)))
            
      (define (get-new-mail)
	(with-handlers ([void
			 (lambda (x)
			   (status "")
			   (if (send disconnected-msg is-shown?)
			       (raise x)
			       (begin
				 (show-error x)
				 (when (exn:fail:network? x)
				   (when (eq? 'yes
					      (confirm-box
					       "Error"
					       (format "There was an communication error.\nClose the connection?")
					       main-frame))
				     (force-disconnect/status))))))])
	  (header-changing-action
	   #f
	   (lambda ()
	     (as-background
	      enable-main-frame
	      (lambda (break-bad break-ok) 
		(when (or (not initialized?)
			  (check-for-new break-bad break-ok))
		  (update-local break-bad break-ok)))
	      close-frame)))))
      
      (define (purge-marked/update-headers)
	(header-changing-action 
	 #f
	 (lambda ()
	   (as-background 
	    enable-main-frame
	    (lambda (break-bad break-ok) 
	      (with-handlers ([void no-status-handler])
		(purge-marked break-bad break-ok)))
	    close-frame))))
      
      (define (copy-marked-to dest-mailbox-name)
	(let* ([marked (filter message-marked? mailbox)])
	  (as-background
	   enable-main-frame
	   (lambda (break-bad break-ok)
	     (copy-messages-to marked dest-mailbox-name))
	   close-frame)))
      
      (define (copy-messages-to marked dest-mailbox-name)
        (unless (null? marked)
          (let-values ([(imap count new?) (connect)])
            (check-positions imap marked)
            (status "Copying messages to ~a..." dest-mailbox-name)
	    (imap-copy imap (map message-position marked) dest-mailbox-name)
	    (status "Copied to ~a" dest-mailbox-name))))
      
      (define (auto-file)
	(as-background
	 enable-main-frame
	 (lambda (break-bad break-ok)
	   (break-ok)
	   (map
	    (lambda (auto)
	      (let* ([dest-mailbox-name (car auto)]
		     [fields (map car (cadr auto))]
		     [val-rxs (map string->regexp (map cadr (cadr auto)))])
		(with-handlers ([void no-status-handler])
		  (break-ok)
		  (status "Finding ~a messages..." dest-mailbox-name)
		  (let ([file-msgs 
			 (filter
			  (lambda (m)
			    (and (not (message-marked? m))
				 (let ([h (get-header (message-uid m))])
				   (ormap (lambda (field val-rx)
					    (let ([v (extract-field field h)])
					      (and v (regexp-match val-rx v))))
					  fields val-rxs))))
			  mailbox)])
		    (unless (null? file-msgs)
		      (status "Filing to ~a..." dest-mailbox-name)
		      (break-bad)
		      (let-values ([(imap count new?) (connect)])
			(status (format "Filing to ~a..." dest-mailbox-name))
                        ; Copy messages for filing:
			(imap-copy imap (map message-position file-msgs) dest-mailbox-name)
                        ; Mark them (let the user delete)
			(for-each (lambda (m)
				    (set-message-flags! m (cons 'marked (message-flags m)))
				    (let ([i (let ([items (send header-list get-items)]
						   [uid (message-uid m)])
					       (ormap (lambda (i) (and (eq? (send i user-data) uid)
								       i))
						      items))])
				      (apply-style i marked-delta)))
				  file-msgs)
			(write-mailbox)))))))
	    (AUTO-FILE-TABLE)))
	 close-frame)
	(status "Auto file done"))
      
      (define (download-all)
	(get-new-mail)
	(header-changing-action
	 #t
	 (lambda ()
	   (as-background
	    enable-main-frame
	    (lambda (break-bad break-ok)
	      (with-handlers ([exn:break?
			       (lambda (x) "<interrupted>")])
		(break-ok)
		(with-handlers ([exn:break? (lambda (x) (void))])
	           (for-each (lambda (message)
			       (let ([uid (message-uid message)])
				 (break-bad)
				 (get-body uid break-bad break-ok)
				 (break-ok)))
			     mailbox))))
	    close-frame))))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  GUI: Rest of Frame                                     ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define sizing-panel (make-object panel:vertical-dragable% (send main-frame get-area-container)))
      (define top-half (make-object vertical-panel% sizing-panel))
      (define button-panel (new horizontal-panel% 
                                (parent top-half)
                                (stretchable-height #f)))
      (define sorting-list (instantiate sorting-list% ()
                             (parent top-half)
                             (stretchable-height #f)
			     (vertical-inset 1)))
      (define header-list (make-object header-list% top-half))
      (send (send header-list get-editor) set-line-spacing 0)
      (define message (new canvas:color% 
			   [parent sizing-panel]
			   [style '(auto-hscroll)]))
      (send header-list min-height 20)
      (send header-list stretchable-height #t)
      (send header-list set-no-sublists #t)
      (send main-frame reflow-container)
      (with-handlers ([void void])
        (send sizing-panel set-percentages (list 1/3 2/3)))
      (let ([e (make-object display-text%)])
	((current-text-keymap-initializer) (send e get-keymap))
	(send e set-max-undo-history 0)
	(send message set-editor e)
	(make-fixed-width message e #f #f)
	(let ([b (make-object bitmap% (build-path (collection-path "icons") "return.xbm") 'xbm)])
	  (when (send b ok?)
	    (send e set-autowrap-bitmap b)))
	(send e lock #t))

      (when (get-pref 'sirmail:wrap-lines)
	(send wrap-lines-item check #t)
	(send (send message get-editor) auto-wrap #t))
      
      ;; enable-main-frame - use with `as-background'
      (define can-poll? #t)      
      (define (enable-main-frame on? refocus break-proc)
	(let ([w (send main-frame get-focus-window)])
	  (set! can-poll? on?)
	  (send sorting-list enable on?)
	  (send header-list enable on?)
	  (send message enable on?)
	  (let* ([cursor (if on? plain-cursor arrow+watch-cursor)])
	    (send main-frame set-cursor cursor)
	    (send (send sorting-list get-editor) set-cursor cursor #t)
	    (send (send header-list get-editor) set-cursor cursor #t)
	    (send (send message get-editor) set-cursor (if on? #f cursor) #t))
	  (send (send main-frame get-menu-bar) enable on?)
	  (set! cancel-button-todo break-proc)
	  (send cancel-button enable (not on?))
	  (when (and on? refocus)
	    (send refocus focus))
	  w))

      (define (close-frame)
	(send main-frame show #f))
      
      (define no-status-handler (lambda (x) (status "") (raise x)))
            
      (define disable-button-panel (make-object horizontal-panel% button-panel))
      (define mailbox-message (make-object message% (format "~a: XXXXX" mailbox-name) disable-button-panel))
      (define (display-message-count n)
	(send mailbox-message set-label (format "~a: ~a" mailbox-name n)))
      (display-message-count (length mailbox))
      (define new-mail-message%
        (class canvas%
          (inherit get-dc get-client-size get-parent
		   horiz-margin vert-margin)
          (init-field font)
          (define message "<<unset>>")
          (define/override (on-paint)
            (let ([dc (get-dc)])
              (send dc set-font font)
              (let-values ([(w h) (get-client-size)]
                           [(tw th ta td) (send dc get-text-extent message)])
                (send dc set-pen (send the-pen-list find-or-create-pen (get-panel-background) 1 'transparent))
                (send dc set-brush (send the-brush-list find-or-create-brush (get-panel-background) 'panel))
                (send dc draw-rectangle 0 0 w h)
                (send dc draw-text message
                      (- (/ w 2) (/ tw 2))
                      (- (/ h 2) (/ th 2))
		      #t))))
          (define/public (set-message n)
            (set! message 
                  (cond
                    [(get-pref 'sirmail:always-happy) "New Mail!"]
                    [(n . <= . 50) "New Mail!"]
                    [(n . <= . 200) "New Mail"]
                    [else "New Mail!@#$%"]))
            (update-min-width))
          (inherit min-width)
          (define/private (update-min-width)
            (let-values ([(w h d s) (send (get-dc) get-text-extent message font)])
              (min-width (inexact->exact (ceiling w)))))
          (super-new (style '(transparent)))
          (update-min-width)
          (inherit stretchable-width)
	  (horiz-margin 2)
	  (vert-margin 2)
          (stretchable-width #f)))
      
      (define-values (show-new-mail-msg hide-new-mail-msg disconnected-msg enqueued-msg)
	(let* ([font (make-object font% (send normal-control-font get-point-size) 'system 'normal 'bold)])
	  (let ([spacer (make-object message% "  " disable-button-panel)]
                [m (make-object new-mail-message% font disable-button-panel)]
		[d (new message% [label "Disconnected"] [parent disable-button-panel] [font font])]
                [e-msg (new message% [label "Mail Enqueued"] [parent disable-button-panel] [font font])])
	    (send m show #f)
            (send e-msg show #f)
	    (values (lambda () 
                      (send m set-message (length mailbox))
                      (send m show #t))
                    (lambda () (send m show #f))
                    d
                    e-msg))))
      
      (thread
       (lambda ()
         (let loop ()
           (unless (and (object? main-frame)
                        (send main-frame is-shown?)
                        (procedure? enqueued-messages?))
             (sleep 1/2)
             (loop)))
         (let loop ()
           (when (send main-frame is-shown?)
             (send enqueued-msg show (enqueued-messages?))
             (sleep 1/2)
             (loop)))))

      ;; Optional GC icon (lots of work for this little thing!)
      (when (get-pref 'sirmail:show-gc-icon)
	(let* ([gif (make-object bitmap% (build-path (collection-path "icons") "recycle.png"))]
	       [w (send gif get-width)]
	       [h (send gif get-height)]
               [scale 1]
	       [recycle-bm (make-object bitmap% (quotient w scale) (quotient h scale))]
	       [dc (make-object bitmap-dc% recycle-bm)])
	  (send dc set-scale (/ 1 scale) (/ 1 scale))
	  (send dc draw-bitmap gif 0 0)
	  (send dc set-bitmap #f)
	  (let* ([w (send recycle-bm get-width)]
		 [h (send recycle-bm get-height)]
		 [canvas (instantiate canvas% (button-panel)
				      [stretchable-width #f]
				      [stretchable-height #f]
				      [style '(border)])]
		 [empty-bm (make-object bitmap% w h)]
		 [dc (make-object bitmap-dc% empty-bm)])
	    (send canvas min-client-width w)
	    (send canvas min-client-height h)
	    (send dc clear)
	    (send dc set-bitmap #f)
	    (register-collecting-blit canvas 
				      0 0 w h
				      recycle-bm empty-bm
				      0 0 0 0))))

      (define cancel-button
	(make-object button% "Stop" button-panel
		     (lambda (b e) (cancel-button-todo))))
      (define cancel-button-todo void)
      (send cancel-button enable #f)

      ;; -------------------- Status Line --------------------
      
      (define last-status "")
      (define status-sema (make-semaphore 1))
      (define (status . args)
        (semaphore-wait status-sema)
	(let ([s (apply format args)])
          (unless (equal? s last-status)
            (set! last-status s)
            (update-status-text)))
        (semaphore-post status-sema))
      
      ;; update-status-text : -> void
      ;; =any thread=
      (define (update-status-text)
        (let ([mem-str
               (if (and vsz rss)
                   (format "(mz: ~a vsz: ~a rss: ~a vocab: ~a)"
                           (format-number (quotient (current-memory-use) 1024)) 
                           vsz 
                           rss
                           (word-count))
                   (format "(mz: ~a vocab: ~a)"
                           (format-number (quotient (current-memory-use) 1024))
                           (word-count)))])
	  (send main-frame set-status-text 
		(if (equal? last-status "")
		    mem-str
		    (string-append last-status " " mem-str)))))
      (thread
       (lambda ()
         (let loop ()
	   (semaphore-wait status-sema)
           (when (object? main-frame)
             (update-status-text))
           (semaphore-post status-sema)
	   (sleep 5)
           (unless done?
             (loop)))))
      
      (define vsz #f)
      (define rss #f)
      (define (start-vsz/rss-thread)
        (thread
         (lambda ()
           (define (get-numbers)
             (with-handlers ([exn:fail? (lambda (x) #f)])
               (let ([re:nums #rx"[^ \t]*[ \t]*[^ \t]*[ \t]*[^ \t]*[ \t]*[^ \t]*[ \t]*([0-9]*)[ \t]*([0-9]*)[ \t]*"])
                 (let ([m (regexp-match re:nums (get-lines))])
                   (and m
                        (map string->number (cdr m)))))))
           (define command "ps wwaux | grep SirMail | grep -v grep")
           
           (define (get-lines)
             (let ([p (open-output-string)])
               (parameterize ([current-output-port p]
                              [current-input-port (open-input-string "")])
                 (system command))
               (get-output-string p)))
           
           (let loop ()
             (let ([v (get-numbers)])
               (when (and v (send main-frame is-shown?))
                 (set! vsz (format-number (car v)))
                 (set! rss (format-number (cadr v)))
                 (sleep 10)
                 (loop)))))))

      ;; copied from framerok/private/frame.sss -- be sure to propogate fixes....
      ;; or establish single point of control.
      (define (format-number n)
	(if n
	    (let loop ([n n])
	      (cond
	       [(<= n 1000) (number->string n)]
	       [else
		(string-append 
		 (loop (quotient n 1000))
		 ","
		 (pad-to-3 (modulo n 1000)))]))
	    "???"))
      
      (define (pad-to-3 n)
        (cond
          [(<= n 9) (format "00~a" n)]
          [(<= n 99) (format "0~a" n)]
          [else (number->string n)]))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  GUI: Sorting                                           ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

      (send sorting-list min-height 5)
      (define sorting-text (send (send sorting-list new-item) get-editor))
      (define sorting-text-from (make-object text%))
      (send sorting-text-from insert "From")
      (define sorting-text-subject (make-object text%))
      (send sorting-text-subject insert "Subject")
      (define sorting-text-uid (make-object text%))
      (send sorting-text-uid insert "UID")
      (define (add-sorting-es text width)
        (let ([es (instantiate editor-snip% ()
                    (with-border? #f)
                    (editor text)
                    (top-margin 1)
                    (top-inset 1)
                    (bottom-margin 1)
                    (bottom-inset 1))])
          (send sorting-text insert es)
          (send es set-flags (remove 'handles-events (send es get-flags)))
          es))
      (send sorting-list set-line-count 1)
      (define sorting-from-snip (add-sorting-es sorting-text-from FROM-WIDTH))
      (send sorting-text insert (make-object vertical-line-snip%))
      (define sorting-subject-snip (add-sorting-es sorting-text-subject SUBJECT-WIDTH))
      (send sorting-text insert (make-object vertical-line-snip%))
      (define sorting-uid-snip (add-sorting-es sorting-text-uid UID-WIDTH))
      
      (when (AUTO-FILE-TABLE)
	(make-object separator-menu-item% msg-menu)
	(make-object menu-item% "Auto File" msg-menu
          (lambda (i e)
            (auto-file))))
      
      (define (redisplay-current)
	(when current-selected
	  (send header-list on-double-select current-selected)))
      
      (define (sort-by-date) 
        (sort-by-fields (list (list "date" date-cmp)))
        (reset-sorting-text-styles))
      (define (sort-by-sender) 
        (sort-by from<?)
        (reset-sorting-text-styles)
        (identify-sorted sorting-text-from))
      (define (sort-by-subject) 
        (sort-by subject<?)
        (reset-sorting-text-styles)
        (identify-sorted sorting-text-subject))
      (define (sort-by-size) 
        (sort-by size<?)
        (reset-sorting-text-styles)
        (identify-sorted sorting-text-subject))
      (define (sort-by-order-received) 
        (sort-by-uid)
        (reset-sorting-text-styles)
        (identify-sorted sorting-text-uid))

      (define prev-field-list "")
      (define prev-field-regexp "")
      (define (sort-by-header-field) 
	(letrec ([d (new dialog% 
			 [parent main-frame]
			 [label "Header Search"]
			 [stretchable-width #f]
			 [stretchable-height #f])]
		 [text-pane (new vertical-pane%
				 [parent d]
				 [alignment '(left center)]
				 [stretchable-height #f])]
		 [t1 (new message%
			  [parent text-pane]
			  [label "Sorts messages with matching fields before non-matching;"])]
		 [t2 (new message%
			  [parent text-pane]
			  [label "use a comma to separate multiple field names"])]
		 [text-change-callback
		  (lambda (txt e)
		    (check-enable-ok)
		    (when (and (eq? 'text-field-enter
				    (send e get-event-type))
			       (send ok is-enabled?))
		      (do-ok)))]
		 [field-text (new text-field%
				   [parent d]
				   [label "Header Field(s):"]
				   [callback text-change-callback]
				   [init-value prev-field-list])]
		 [regexp-text (new text-field%
				   [parent d]
				   [label "Value Regexp:"]
				   [callback text-change-callback]
				   [init-value prev-field-regexp])]
		 [buttons-panel (new horizontal-panel%
				     [parent d]
				     [stretchable-height #f]
				     [alignment '(right center)])]
		 [ok (new button%
			  [parent buttons-panel]
			  [label "Ok"]
			  [style '(border)]
			  [callback (lambda (b e)(do-ok))])]
		 [cancel (new button%
			      [parent buttons-panel]
			      [label "Cancel"]
			      [callback (lambda (b e) 
					  (send d show #f))])]
		 [find-field-list #f]
		 [find-fields #f]
		 [find-regexp #f]
		 [ok? #f]
		 [check-enable-ok
		  (lambda ()
		    (set! find-field-list (send field-text get-value))
		    (set! find-fields (regexp-split #rx" *, *" find-field-list))
		    (set! find-regexp (send regexp-text get-value))
		    (send ok enable
			  (and (andmap (lambda (find-field)
					 (and (positive? (string-length find-field))
					      (regexp-match #rx"^[a-zA-Z0-9-]+$" find-field)))
				       find-fields)
			       (with-handlers ([void (lambda (x) #f)]) (regexp find-regexp)))))]
		 [do-ok (lambda ()
			  (set! ok? #t) 
			  (send d show #f))])
	  (send d show #t)
	  (when ok?
	    (set! prev-field-list find-field-list)
	    (set! prev-field-regexp find-regexp)
	    (sort-by (field<? find-fields (regexp find-regexp) (make-hash-table 'equal)))
	    (reset-sorting-text-styles))))
      
      (define no-sort-style-delta (make-object style-delta% 'change-normal))
      (define sort-style-delta (make-object style-delta% 'change-bold))
      (send sort-style-delta set-delta-foreground "blue")
      (define tracking-style-delta (make-object style-delta%))
      (send tracking-style-delta set-delta-background "Gray")
      (define not-tracking-style-delta (make-object style-delta%))
      (send not-tracking-style-delta set-delta-background "White")
      (define (reset-sorting-text-styles)
        (send sorting-text-from change-style no-sort-style-delta 0 (send sorting-text-from last-position))
        (send sorting-text-uid change-style no-sort-style-delta 0 (send sorting-text-uid last-position))
        (send sorting-text-subject change-style no-sort-style-delta 0 (send sorting-text-subject last-position)))
      (define (identify-sorted text)
        (send text change-style sort-style-delta 0 (send text last-position)))
      (define (reset-sorting-tracking)
        (send sorting-text-from change-style not-tracking-style-delta 0 (send sorting-text-from last-position))
        (send sorting-text-uid change-style not-tracking-style-delta 0 (send sorting-text-uid last-position))
        (send sorting-text-subject change-style not-tracking-style-delta 0 (send sorting-text-subject last-position)))
      (define (set-sorting-tracking which)
	(let ([text (case which
		      [(from) sorting-text-from]
		      [(subject) sorting-text-subject]
		      [(uid) sorting-text-uid])])
	  (send text change-style tracking-style-delta 0 (send text last-position))))
      
      (reset-sorting-text-styles)
      (identify-sorted sorting-text-uid)
      
      (define re:date
	(regexp
	 "([0-9]*)[ 	]+([A-Za-z]+)[ 	]+([0-9]+)[ 	]+([0-9][0-9]):([0-9][0-9]):([0-9][0-9])"))
      
      ;; using the tz seems to require a date->seconds -- too expensive.
      (define (date-cmp aid bid a b)
	(define (month->number mon)
	  (string-lowercase! mon)
	  (case (string->symbol mon)
	    [(jan) 1]
	    [(feb) 2]
	    [(mar) 3]
	    [(apr) 4]
	    [(may) 5]
	    [(jun) 6]
	    [(jul) 7]
	    [(aug) 8]
	    [(sep) 9]
	    [(oct) 10]
	    [(nov) 11]
	    [(dec) 12]))
        
	(define (pairwise-cmp l1 l2)
	  (cond
            [(and (null? l1) (null? l2)) 'same]
            [(or (null? l1) (null? l2)) (error 'pairwise-cmp "internal error; date lists mismatched")]
            [(= (car l1) (car l2)) (pairwise-cmp (cdr l1) (cdr l2))]
            [else (< (car l1) (car l2))]))
        
	(define (get-date a)
	  (let* ([m (regexp-match re:date a)])
	    (if m
		(let* ([datel (cdr m)]
		       [day (string->number (first datel))]
		       [month (month->number (second datel))]
		       [year (string->number (third datel))]
		       [hours (string->number (fourth datel))]
		       [minutes (string->number (fifth datel))]
		       [seconds (string->number (sixth datel))])
		  (list year month day
			hours minutes seconds))
		(list 0 0 0
		      0 0 0))))
        
	(pairwise-cmp
	 (get-date a)
	 (get-date b)))
            
      (define re:quote "[\"<>]")
      ;; from<? : message message -> boolean
      ;; compares messages by from lines, defaults to uid if froms are equal.
      (define (from<? a b)
        (string-cmp/default-uid (get-address a)
                                (get-address b)
                                a
                                b))

      ;; get-address : message -> string
      (define (get-address msg)
        (let ([frm (message-from msg)])
          (if frm
	      (hash-table-get
	       address-memo-table
	       frm
	       (lambda ()
		 (let ([res
			(with-handlers ([exn:fail? (lambda (x) "")])
			  (regexp-replace* re:quote 
					   (car (extract-addresses
						 frm
						 'address))
					   ""))])
		   (hash-table-put! address-memo-table frm res)
		   res)))
              "")))

      ;; get-address : message -> string
      (define address-memo-table (make-hash-table 'equal))
      
      (define ((field<? field-names rx ht) a b)
	(let ([a? (match-field a field-names rx ht)]
	      [b? (match-field b field-names rx ht)])
	  (cond
	   [(and a? (not b?)) #t]
	   [(and b? (not a?)) #f]
	   [else (< (message-uid a) (message-uid b))])))
      
      (define (match-field msg field-names rx ht)
	(hash-table-get
	 ht
	 msg
	 (lambda ()
	   (let ([header (get-header (message-uid msg))])
	     (let ([flds (map (lambda (field-name)
				(extract-field field-name header))
			      field-names)])
	       (let ([res (ormap (lambda (fld)
				   (and fld
					(regexp-match rx fld)
					#t))
				 flds)])
		 (hash-table-put! ht msg res)
		 res))))))
      
      (define re:re (regexp "^[rR][eE]: *(.*)"))
      ;; subject<? : message message -> boolean
      ;; compares messages by subject lines, defaults to uid if subjects are equal.
      (define (subject<? a b)
	(let ([simplify (lambda (msg)
			  (let ([s (message-subject msg)])
                            (if s
                                (let ([m (regexp-match re:re s)])
                                  (if m
                                      (cadr m)
                                      s))
                                "")))])
          (string-cmp/default-uid (simplify a) (simplify b) a b)))

      (define (size<? a b)
        (let ([sa (or (message-size a) 0)]
              [sb (or (message-size b) 0)])
          (if (= sa sb)
              (< (message-uid a) (message-uid b))
              (< sa sb))))
      
      ;; string-cmp : string string message message -> boolean
      (define (string-cmp/default-uid str-a str-b a b)
        (if (string-locale-ci=? str-a str-b)
            (< (message-uid a) (message-uid b))
            (string-locale-ci<? str-a str-b)))
      
      (define (sort-by compare)
        (as-background
         enable-main-frame
         (lambda (break-bad break-ok)
           (status "Sorting...")
           (send header-list sort
                 (lambda (a b)
                   (let* ([aid (send a user-data)]
                          [bid (send b user-data)]
                          [ma (find-message aid)]
                          [mb (find-message bid)])
                     (compare ma mb))))
           (status ""))
         close-frame))
      
      (define (sort-by-uid)
        (as-background
         enable-main-frame
         (lambda (break-bad break-ok)
           (status "Sorting...")
           (send header-list sort
                 (lambda (a b)
                   (let ([aid (send a user-data)]
                         [bid (send b user-data)])
                     (< aid bid))))
           (status ""))
         close-frame))
      
      (define (sort-by-fields fields)
	(let* ([ht (make-hash-table)]
	       [get-header/cached 
		(lambda (uid first-field)
		  (hash-table-get
		   ht
		   uid
		   (lambda ()
		     (let* ([h (get-header uid)]
			    [p (cons h
				     (and first-field
					  (extract-field 
					   first-field 
					   h)))])
		       (hash-table-put! ht uid p)
		       p))))])
	  (as-background
	   enable-main-frame
	   (lambda (break-bad break-ok)
	     (status "Sorting...")
	     (send header-list sort
		   (lambda (a b)
		     (let ([aid (send a user-data)]
			   [bid (send b user-data)])
		       (let ([ah+f (get-header/cached aid (and (pair? fields)
							       (caar fields)))]
			     [bh+f (get-header/cached bid (and (pair? fields)
							       (caar fields)))])
			 (let loop ([fields fields][first? #t])
			   (if (null? fields)
			       (< aid bid)
			       (let ([c ((cadar fields)
					 aid bid
					 (if first?
					     (cdr ah+f)
					     (parse-encoded
					      (extract-field (caar fields) (car ah+f))))
					 (if first?
					     (cdr bh+f)
					     (parse-encoded
					      (extract-field (caar fields) (car bh+f)))))])
				 (if (eq? c 'same)
				     (loop (cdr fields) #f)
				     c))))))))
	     (status ""))
	   close-frame)))

      (when (SORT)
	(case (SORT)
	  [(date) (sort-by-date)]
	  [(subject) (sort-by-subject)]
	  [(from) (sort-by-sender)]
	  [(id) (void)])) ;; which is (sort-by-order-received)

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  GUI: Finish Setup                                      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (send header-list focus)
      
      (send (send header-list get-editor) begin-edit-sequence)
      (for-each add-message mailbox)      
      (send (send header-list get-editor) end-edit-sequence)

      (send main-frame create-status-line)
      
      (send main-frame show #t)
      (when (eq? (system-type) 'macosx)
        (start-vsz/rss-thread))
      (set! got-started? #t)
      
      (unless (null? mailbox)
	(let ([last (car (last-pair (send header-list get-items)))])
	  (send last select #t)
	  (queue-callback (lambda () (send last scroll-to)))))

      (frame:reorder-menus main-frame)
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Queued Message Sends                                   ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; queue-directory : string
      ;; the directory where queue'd files are stored (created at this point)
      (define queue-directory 
        (let ([dir (build-path (find-system-path 'pref-dir) 
                               (if (eq? 'unix (system-type)) 
                                   ".sirmail-queue"
                                   "SirMail Queue"))])
          (unless (directory-exists? dir)
            (make-directory* dir))
          dir))
      
      ;; enqueued-messages? : -> bool
      ;; returns true if there are messages to send
      (define (enqueued-messages?)
	(not (= 0 (length (directory-list queue-directory)))))

      ;; send-queued-messsages : -> void
      ;; sends the files queued in `queue-directory'
      (define (send-queued-messages)
        (for-each send-queued-message (directory-list queue-directory)))
      
      ;; send-queued-message : string -> void
      ;; sends the email message in `filename' by opening a window and sending it a message
      (define (send-queued-message filename)
        (start-new-window
	 (lambda ()
           (let ([full-filename (build-path queue-directory filename)])
             (send (new-mailer full-filename "" "" "" "" "" null (length mailbox))
                   send-message)
             (delete-file full-filename)))))
            
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Message Parsing                                        ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define get-viewable-headers
	(lambda (h)
	  ((if mime-mode? parse-encoded values)
	   (if show-full-headers?
	       h
	       (let loop ([l (reverse (MESSAGE-FIELDS-TO-SHOW))]
			  [small-h empty-header])
		 (if (null? l)
		     small-h
		     (let ([v (extract-field (car l) h)])
		       (if v
			   (loop (cdr l) (insert-field
					  (car l)
					  v
					  small-h))
			   (loop (cdr l) small-h)))))))))

      (define (parse-and-insert-body header body text-obj insert sep-width img-mode?)
	(define (insert-separator)
	  (insert (format "\n~a\n" (make-string sep-width #\-))
		  (lambda (t s e) 
		    (send t change-style green-delta (add1 s) (sub1 e)))))
    
	(if mime-mode?
	    (let mime-loop ([msg (with-handlers ([exn:fail? (lambda (x)
							      (mime:make-message
							       #f
							       (mime:make-entity
								'text
								'plain 
								'charset
								'encoding
								(mime:make-disposition
								 'error 
								 'filename 'creation
								 'modification 'read
								 'size 'params)
								'params 'id
								'description 'other 'fields
								null 
								(lambda (o)
								  (fprintf o "MIME error: ~a"
									   (if (exn? x)
									       (exn-message x)
									       x))))
							       #f))])
				   (mime:mime-analyze (bytes-append (string->bytes/latin-1 
								     header 
								     (char->integer #\?))
								    body)))]
			    [skip-headers? #t])
	      (let* ([ent (mime:message-entity msg)]
                     [slurp-stream (lambda (ent o)
                                     (with-handlers ([exn:fail? (lambda (x)
								  (fprintf o 
									   "\n[decode error: ~a]\n"
									   (if (exn? x)
									       (exn-message x)
									       x)))])
                                       ((mime:entity-body ent) o)))]
		     [slurp (lambda (ent)
                              (let ([o (open-output-bytes)])
                                (slurp-stream ent o)
                                (get-output-bytes o)))]
		     [generic (lambda (ent)
				(let ([fn (parse-encoded
                                           (or (let ([disp (mime:entity-disposition ent)])
                                                 (and (not (equal? "" (mime:disposition-filename disp)))
                                                      (mime:disposition-filename disp)))
                                               (let ([l (mime:entity-params ent)])
                                                 (let ([a (assoc "name" l)])
                                                   (and a (cdr a))))))]
				      [sz (mime:disposition-size (mime:entity-disposition ent))]
				      [content #f])
				  (let ([to-file
					 (lambda (fn)
					   (as-background
					    enable-main-frame
					    (lambda (break-bad break-ok)
					      (break-ok)
					      (let ([v (slurp ent)])
						(break-bad)
						(unless content
						  (set! content v)))
					      (break-ok)
					      (with-output-to-file fn
						(lambda ()
						  (write-bytes content))
						'truncate/replace))
					    close-frame))])
				    (insert-separator)
				    (insert (format "[~a/~a~a~a]" 
						    (mime:entity-type ent)
						    (mime:entity-subtype ent)
						    (if fn
							(format " \"~a\"" fn)
							"")
						    (if sz
							(format " ~a bytes" sz)
							""))
					    (lambda (t s e)
					      (send t set-clickback s e
						    (lambda (a b c)
						      (let ([fn (put-file "Save Attachement As"
									  main-frame
									  #f
									  fn)])
							(when fn
							  (to-file fn))))
						    #f #f)
					      (send t change-style url-delta s e)))
				    (when (eq? (system-type) 'macosx)
                                      (when fn
                                        (let ([and-open
                                               (lambda (dir)
                                                 (let ([safer-fn (normalize-path (build-path (find-system-path 'home-dir)
                                                                                             dir
                                                                                             (regexp-replace* #rx"[/\"|:<>\\]" fn "-")))])
                                                   (insert " " set-standard-style)
                                                   (insert (format "[~~/~a & open]" dir)
                                                           (lambda (t s e)
                                                             (send t set-clickback s e
                                                                   (lambda (a b c)
                                                                     (to-file safer-fn)
                                                                     (parameterize ([current-input-port (open-input-string "")])
                                                                       (system* "/usr/bin/open" (path->string safer-fn))))
                                                                   #f #f)
                                                             (send t change-style url-delta s e)))))])
                                          (and-open "Desktop")
                                          (and-open "Temp")))))
                                  (insert "\n" set-standard-style)
				  (lambda ()
				    (unless content
				      (set! content (slurp ent)))
				    content)))])
		(case (mime:entity-type ent)
		  [(text) (let ([disp (mime:disposition-type (mime:entity-disposition ent))])
                            (cond
                              [(or (eq? disp 'error)
				   (and (eq? disp 'inline) (not no-mime-inline?)))
                               (cond
                                 [(and html-mode?
                                       (eq? 'html (mime:entity-subtype ent)))
                                  ;; If no text-obj supplied, make a temporary one for rendering:
                                  (let ([target (or text-obj (make-object display-text%))])
                                    (as-background 
                                     enable-main-frame
                                     (lambda (break-bad break-ok) 
                                       (break-ok)
                                       (with-handlers ([void no-status-handler])
                                         (status "Rendering HTML...")
                                         (let-values ([(in out) (make-pipe)])
                                           (slurp-stream ent out)
                                           (close-output-port out)
					   (render-html-to-text in target img-mode? #f))
                                         (status "")))
                                     close-frame)
                                    (unless text-obj
                                      ;; Copy text in target to `insert':
                                      (insert (send target get-text) void)))]
                                 [else
				  (let-values ([(bytes->string done)
						(cond
						 [(and mime-mode?
						       (string? (mime:entity-charset ent))
						       (string-ci=? "UTF-8" (mime:entity-charset ent)))
						  (values bytes->string/utf-8 void)]
						 [(and mime-mode?
						       (string? (mime:entity-charset ent))
						       (bytes-open-converter (generalize-encoding
									      (mime:entity-charset ent))
									     "UTF-8"))
						  => (lambda (c)
						       (values
							(lambda (s alt)
							  (let loop ([l null][start 0])
							    (let-values ([(r got status) (bytes-convert c s start)])
							      (case status
								[(complete)
								 (bytes->string/utf-8 (apply bytes-append (reverse (cons r l))) alt)]
								[(aborts)
								 (loop (list* #"?" r l) (+ start got))]
								[(error)
								 (loop (list* #"?" r l) (+ start got 1))]))))
							(lambda ()
							  (bytes-close-converter c))))]
						 [else (values bytes->string/latin-1 void)])])
				    (dynamic-wind
					void
					(lambda ()
					  (insert (bytes->string (crlf->lf/preserve-last (slurp ent)) #\?)
						  (lambda (t s e)
						    (when (SHOW-URLS) (hilite-urls t s e))
						    ;;(handle-formatting e) ; too slow
						    (if (eq? disp 'error)
							(send t change-style red-delta s e)))))
					done))])]
                              [else
                               (generic ent)]))]
                  [(image) 
                   (let ([get (generic ent)])
		     (let ([tmp-file (make-temporary-file "sirmail-mime-image-~a")])
		       (call-with-output-file tmp-file
			 (lambda (port)
			   (write-bytes (get) port))
			 'truncate)
		       (unless no-mime-inline?
			 (let ([bitmap (make-object bitmap% tmp-file)])
			   (when (send bitmap ok?)
			     (insert (make-object image-snip% bitmap) void)
			     (insert "\n" void))
			   (delete-file tmp-file)))))]
		  [(message)
		   (insert-separator)
		   (unless (or skip-headers?
			       (null? (mime:message-fields msg)))
		     (insert (string-crlf->lf
			      (get-viewable-headers
			       (let loop ([l (mime:message-fields msg)])
				 (if (null? l)
				     crlf
				     (string-append (car l)
						    crlf
						    (loop (cdr l)))))))
			     void))
		   (map (lambda (x) (mime-loop x #f)) (mime:entity-parts ent))]
		  [(multipart)
		   (cond
		    [(and (eq? 'alternative (mime:entity-subtype ent))
			  (= 2 (length (mime:entity-parts ent)))
			  (andmap (lambda (m)
				    (eq? 'text (mime:entity-type (mime:message-entity m))))
				  (mime:entity-parts ent))
			  (let ([l (map (lambda (m)
					  (mime:entity-subtype (mime:message-entity m)))
					(mime:entity-parts ent))])
			    (and (member l '((plain html)
					     (html plain)
					     (plain enriched)
					     (enriched plain)))
				 l)))
		     => (lambda (l)
			  (let ([pos (if (eq? (car l) 'plain)
					 (if prefer-text? 0 1)
					 (if prefer-text? 1 0))])
			    (mime-loop (list-ref (mime:entity-parts ent) pos) #f)))]
		    [else
		     (map (lambda (x) (mime-loop x #f)) (mime:entity-parts ent))])]
		  [else (generic ent)])))
	    ;; Non-mime mode:
	    (insert (bytes->string/latin-1 (crlf->lf body)) void)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Biff                                                   ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define biff%
	(class timer%
	  (inherit stop)
          (define/override (notify)
            (when can-poll?
              (unless (send disconnected-msg is-shown?)
                (with-handlers ([void
                                 (lambda (x)
                                   (stop)
                                   (force-disconnect/status)
                                   (status "Error connecting: ~s"
                                           (if (exn? x)
                                               (exn-message x)
                                               x)))])
		  (let ([old-new-messages? new-messages?])
		    (as-background
		     enable-main-frame
		     (lambda (break-bad break-ok) 
		       (check-for-new break-bad break-ok))
		     close-frame)
		    (when (and new-messages?
			       (not (eq? old-new-messages? new-messages?)))
		      (bell)))))))
          (super-instantiate ())))
      
      (define biff
	(if (BIFF-DELAY)
	    (make-object biff%)
	    #f))
      
      (define (start-biff)
	(when biff
	  (send biff start (* 1000 (BIFF-DELAY)))))
      
      (start-biff)
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Mail Sending                                           ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Mail-sending window is implemented in sendr.ss. This is
      ;;  the set-up for opening such a window.
      
      (define my-address 
	(with-handlers ([void (lambda (x) "<bad address>")])
	  (car (extract-addresses (MAIL-FROM) 'address))))
      
      (define my-username-@
	(let ([m (regexp-match "^([^@]*)@" my-address)])
	  (if m
	      (cadr m)
	      (string-append my-address "@"))))
      
      (define (not-me? name-addr-full)
	(let ([addr (cadr name-addr-full)])
	  (cond
            [(string-ci=? addr my-address) #f]
            [(and (SELF-ADDRESSES) (member addr (SELF-ADDRESSES))) #f]
            [(and (> (string-length addr) (string-length  my-username-@))
                  (string-ci=? my-username-@ (substring addr 0 (string-length  my-username-@))))
             (eq? (message-box
                   "Identity?"
                   (format "Are you ~a?" (caddr name-addr-full))
                   main-frame
                   '(yes-no))
                  'no)]
            [else #t])))
      
      (define (do-reply follow-up? quote-msg?)
	(define selected (send header-list get-selected))
	(unless selected
	  (bell))
	(when selected
	  (unless (eq? selected current-selected)
	    (send header-list on-double-select selected))
	  (unless (eq? selected current-selected)
	    (bell))
	  (when (eq? selected current-selected)
	    (let* ([uid (send selected user-data)]
		   [h (get-header uid)]
		   [rendered-body (let ([e (send message get-editor)]
					[start (string-length 
						(string-crlf->lf 
						 (get-viewable-headers h)))])
				    (send e get-text start 'eof #t #t))])
	      (start-new-mailer
	       #f
	       (parse-encoded
		(or (extract-field "Reply-To" h) 
		    (extract-field "From" h)
		    ""))
	       (if follow-up?
		   (let ([to (parse-encoded (extract-field "To" h))]
			 [cc (parse-encoded (extract-field "CC" h))])
		     (if (or to cc)
			 (let ([to (map
				    caddr
				    (filter
				     not-me?
				     (append
				      (if to
					  (extract-addresses to 'all)
					  null)
				      (if cc
					  (extract-addresses cc 'all)
					  null))))])
			   (if (null? to)
			       ""
			       (assemble-address-field to)))
			 ""))
		   "")
	       (let ([s (parse-encoded (extract-field "Subject" h))])
		 (cond
		  [(not s) ""]
		  [(regexp-match #rx"^[Rr][Ee][(]([0-9]+)[)]:(.*)$" s)
		   ;; Other mailer is counting replies. We'll count, too.
		   => (lambda (m)
			(format "~a(~a):~a"
				(substring s 0 2)
				(add1 (string->number (caddr m)))
				(cadddr m)))]
		  [(regexp-match "^[Rr][Ee]:" s) s]
		  [(regexp-match "^[Aa][Nn][Tt][Ww][Oo][Rr][Tt]:" s) s]
		  [else (string-append "Re: " s)]))
	       (let ([id (extract-field "Message-Id" h)]
		     [refs (extract-field "References" h)])
		 (format "~a~a"
			 (if id
			     (format "In-Reply-To: ~a\r\n" id)
			     "")
			 (if (or refs id)
			     (format "References: ~a\r\n"
				     (cond
				      [(and refs id)
				       (format "~a\r\n\t\t~a" refs id)]
				      [else (or refs id)]))
			     "")))
	       (if quote-in-reply?
                   (let ([date (parse-encoded (extract-field "Date" h))]
                         [name
                          (with-handlers ([exn:fail? (lambda (x) #f)])
                            (let ([from (parse-encoded (extract-field "From" h))])
                              (car (extract-addresses from 'name))))])
                     (string-append
		      (cond
		       [(and date name)
			(format "At ~a, ~a wrote:\r" date name)]
		       [name
			(format "Quoting ~a:\r" name)]
		       [else
			(format "Quoting <unknown>:\r")])
                      "> "
                      (regexp-replace #rx"(?:\n> )*$"
				      (regexp-replace* #rx"\n" rendered-body "\n> ")
				      "")))
		   "")
	       null)))))

      (define (do-forward)
	(define selected (send header-list get-selected))
	(unless selected
	  (bell))
	(when selected
	  (let* ([uid (send selected user-data)]
		 [h (get-header uid)]
		 [body (get-body uid void void)])
	    (start-new-mailer
	     #f "" ""
	     (let ([s (extract-field "Subject" h)])
	       (if (and s (not (regexp-match "^[Ff][Ww][Dd]:" s)))
		   (string-append "Fwd: " s)
		   (or s "Fwd")))
	     "" ""
	     (list
	      (make-enclosure
	       "Forwarded Message"
	       (insert-field
		"Content-Type" "message/rfc822"
		(insert-field 
		 "Content-Transfer-Encoding" "8bit"
		 (insert-field
		  "Content-Disposition" "inline"
		  empty-header)))
	       (lambda ()
		 (split-crlf
		  (bytes-append (string->bytes/latin-1 h (char->integer #\?))
				body)))))))))
      
      (define (start-new-mailer file to cc subject other-headers body enclosures)
	(start-new-window
	 (lambda ()
	   (new-mailer file to cc subject other-headers body enclosures (length mailbox)))))
      
      (define (start-new-mailer/send-message file to cc subject other-headers body enclosures)
	(start-new-window
	 (lambda ()
           (send (new-mailer file to cc subject other-headers body enclosures (length mailbox))
                 send-message))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Misc Formatting                                        ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; handle-formatting : text -> void
      (define (handle-formatting e)
        (let loop ([line (send e last-line)])
          (unless (zero? line)
            (for-each
             (lambda (regexp/action)
               (handle-single-line/formatting
                (car regexp/action) 
                (cadr regexp/action)
                e
                line))
             regexps/actions)
            (loop (- line 1)))))
      
      (define (handle-single-line/formatting regexp action e line)
        (let ([start (send e line-start-position line)]
              [end (send e line-end-position line)])
          (let loop ([string (send e get-text start end #f)]
                     [line-offset 0])
            (cond
              [(regexp-match-positions regexp string)
               =>
               (lambda (m)
                 (let ([before (cadr m)]
                       [during (caddr m)]
                       [after (cadddr m)])
                   (action e
                           (+ line-offset start (car during))
                           (+ line-offset start (cdr during)))
                   (loop (substring string (car before) (cdr before))
                         (+ (car before) line-offset))
                   (loop (substring string (car after) (cdr after))
                         (+ (car after) line-offset))))]
              [else (void)]))))

      ;; emoticon-path (may not exist)
      (define emoticon-path
        (build-path (collection-path "sirmail") "emoticon"))

      ;; emoticon : string string -> (listof (list regexp (text number number -> void)))
      (define (emoticon img . icons)
        (let ([snip (make-object image-snip% (build-path emoticon-path img))])
          (map
           (lambda (icon)
             (list (regexp (string-append "(.*)(" (quote-regexp-chars icon) ")(.*)"))
                   (lambda (e start end)
                     (send e insert (send snip copy) start end))))
           icons)))
      
      (define (quote-regexp-chars str)
        (apply
         string
         (let loop ([chars (string->list str)])
           (cond
             [(null? chars) null]
             [else (let ([char (car chars)])
                     (if (memq char regexp-special-chars)
                         (list* #\\ char (loop (cdr chars)))
                         (cons char (loop (cdr chars)))))]))))
      
      (define regexp-special-chars (string->list "()*+?[].^\\|"))
      
      ;; all regexps must have three parenthesized sub-expressions
      ;; the first is unmatched text before the regexp, the second
      ;; is the matched tetx and the third is unmatched text after the regexp.
      (define regexps/actions
        (list*
         (list (regexp "(.*)( \\*([^\\*]*)\\* )(.*)")
               (lambda (e start end) (send e change-style bold-style-delta start end)))
         (list (regexp "(.*) _([^_]*)_ (.*)")
               (lambda (e start end) (send e change-style italic-style-delta start end)))
         (if (directory-exists? emoticon-path)
             (append
              (emoticon "bigsmile.gif" ":D" ":-D")
              (emoticon "cry.gif" ":')" ":'-)")
              (emoticon "happy.gif" ":)" ":-)" ":>" ":->" "<-:" "<:" "(-:" "(:")
              (emoticon "kiss.gif" "*:" ":*")
              (emoticon "sad.gif" ":(" ":-(" ":<" ":-<" ">-:" ">:" "):" ")-:")
              (emoticon "tongue.gif" ":P" ":-P")
              (emoticon "wink.gif" ";)" ";-)" ";>" ";->"))
             null)))

    
      (define bold-style-delta (make-object style-delta% 'change-bold))
      (define italic-style-delta (make-object style-delta% 'change-italic))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;; Mailbox memory graph (from messages in this mailbox)
      ;;

      (define (show-memory-graph)
        ;; grab the current value of the mailbox
        (let ([mailbox mailbox]
              [mbox-eventspace (current-eventspace)])
          
          (status "Collecting memory graph record...")
          ;; ht : [symbol -o> (listof (cons seconds bytes))]
          (parameterize ([current-eventspace (make-eventspace)])
            (queue-callback
             (lambda ()
               (let ([ht (make-hash-table)])
                 (let loop ([mailbox mailbox])
                   (cond
                     [(empty? mailbox) (void)]
                     [else 
                      (let* ([message (car mailbox)]
                             [uid (message-uid message)]
                             [header (get-header uid)]
                             [key 
                              (string->symbol
                               (format "~a" (extract-field "X-Mailer" header)))]
                             [uptime-str (extract-field "X-Uptime" header)])
                        (when uptime-str
                          (let ([uptime (parse-uptime uptime-str)])
                            (when uptime
                              (hash-table-put! 
                               ht
                               key
                               (cons
                                uptime
                                (hash-table-get ht key (lambda () '()))))))))
                      (loop (cdr mailbox))]))
                 
                 (let ([info 
                        (sort 
                         (hash-table-map ht (lambda (x y) (list (symbol->string x) y)))
                         (lambda (x y) (string<=? (car x) (car y))))])
                   (parameterize ([current-eventspace mbox-eventspace])
                     (queue-callback
                      (lambda ()
                        (status "Showing graph"))))
                   (make-memory-graph-window info))))))))

      ;; eventspace: graph eventspace
      (define (parse-uptime str)
        (let* ([sep-bytes (regexp-match #rx"([0-9,]*) bytes" str)]
               [bytes (and sep-bytes
                           (string->number
                            (regexp-replace* #rx"," (cadr sep-bytes) "")))]
               [seconds
                (cond
                  [(regexp-match day-hour-regexp str)
                   =>
                   (combine (* 24 60 60) (* 60 60))]
                  [(regexp-match hour-minute-regexp str)
                   =>
                   (combine (* 60 60) 60)]
                  [(regexp-match minute-second-regexp str)
                   =>
                   (combine 60 1)]
                  [else #f])])
          (if (and bytes seconds)
              (cons seconds bytes)
              #f)))

      ;; eventspace: graph eventspace
      (define (combine m1 m2)
        (lambda (match)
          (let ([first (cadr match)]
                [second (caddr match)])
            (+ (* (string->number first) m1)
               (* (string->number second) m2)))))
      
      ;; info : (listof (list string (listof (cons number number)))) -> void
      ;; eventspace: new graph eventspace
      (define (make-memory-graph-window info)
        (define frame (new frame:basic% 
                           (label "Memory Histogram")
                           (width 500)
                           (height 600)))
        (define canvas (new canvas%
                            (paint-callback
                             (lambda (c dc)
                               (draw-graph dc text)))
                            (parent (send frame get-area-container))))
        (define text (new text%))
        (define editor-canvas (new editor-canvas% 
                                   (parent (send frame get-area-container))
                                   (editor text)
                                   (stretchable-height #f)
                                   (line-count 6)))
        
        (define colors '("Green"
                         "DarkOliveGreen"
                         "ForestGreen"
                         "MediumTurquoise"
                         "SteelBlue"
                         "Teal"
                         "CadetBlue"
                         "Indigo"
                         "Purple"
                         "Fuchsia"
                         "Black"
                         "DarkRed"
                         "HotPink"
                         "OrangeRed"
                         "SaddleBrown"))
        
        (define original-colors colors)

        (define (draw-graph dc text)
          (let ([max-x 0]
                [max-y 0]
                [left-scale 0])
            (for-each
             (lambda (key-pairs)
               (for-each
                (lambda (pair)
                  (set! max-x (max (car pair) max-x))
                  (set! max-y (max (cdr pair) max-y)))
                (cadr key-pairs)))
             info)

            (let-values ([(cw ch) (send canvas get-client-size)])
              (let* ([text-height (let-values ([(w h _1 _2) (send dc get-text-extent "9")])
                                    h)]
                     [draw-y-label
                      (lambda (frac)
                        (let ([str (format "~a" (quotient (* frac max-y) (* 1024 1024)))]
                              [y (max (* ch (- 1 frac)) text-height)])
                          (let-values ([(w h _1 _2) (send dc get-text-extent str)])
                            (set! left-scale (max left-scale w))
                            (send dc draw-line 0 (+ y (floor (/ h 2))) cw (+ y (floor (/ h 2))))
                            (send dc draw-text str 0 y))))])
                (draw-y-label 1)
                (draw-y-label 3/4)
                (draw-y-label 1/2)
                (draw-y-label 1/4)
                (draw-y-label 0)
                (send dc draw-line left-scale 0 left-scale ch)))
              
            (set! colors original-colors)
            (for-each
             (lambda (key-pairs)
               (let ([key (car key-pairs)]
                     [pairs (cadr key-pairs)])
                 (set! colors (cdr colors))
                 (send dc set-pen (send the-pen-list find-or-create-pen (car colors) 1 'solid))
                 (send dc set-brush (send the-brush-list find-or-create-brush (car colors) 'solid))
                 (for-each
                  (lambda (pair)
                    (plot-pair dc (car pair) (cdr pair) left-scale max-x max-y))
                  pairs)))
             info)))
        
        (define (plot-pair dc x y left-scale max-x max-y)
          (let-values ([(cw ch) (send canvas get-client-size)])
            (let* ([w (- cw left-scale)]
                   [h ch]
                   [dc-x (+ left-scale (* x (/ w max-x)))]
                   [dc-y (- ch (* y (/ h max-y)))])
              (send dc draw-rectangle dc-x dc-y 3 3))))
        
        (send text begin-edit-sequence)
        (for-each (lambda (key-pairs)
                    (let ([key (car key-pairs)]
                          [pairs (cadr key-pairs)])
                      (set! colors (cdr colors))
                      (let ([before (send text last-position)])
                        (send text insert (format "~a msgs ~a ~a" 
                                                  (length pairs)
                                                  key
                                                  (car colors)))
                        (let ([after (send text last-position)])
                          (send text insert "\n")
                          (let ([sd (make-object style-delta%)])
                            (send sd set-delta-foreground (car colors))
                            (send text change-style sd before after))))))
                  info)
        (send text end-edit-sequence)
        
        ; (set-cdr! (last-pair colors) colors) ;; FIXME: need a cyclic list
        (send frame show #t))
      
      (define (make-and-regexp first second)
        (regexp (format "([0-9]+) ~as? and ([0-9]+) ~as?" first second)))
      (define day-hour-regexp (make-and-regexp "day" "hour"))
      (define hour-minute-regexp (make-and-regexp "hour" "minute"))
      (define minute-second-regexp (make-and-regexp "minute" "second"))
           
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Hiliting URLS                                          ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; hilite-urls : text -> void
      ;; highligts all of the urls (strings beginning with `http:', `https:' or `ftp:')
      ;; in the buffer to be able to click on them.
      (define (hilite-urls e start end)
        (define (hilite-urls/prefix prefix)
          (let loop ([pos start])
            (when (< pos end)
              (let ([start-pos (send e find-string prefix 'forward pos 'eof #t #f)])
                (when start-pos
                  (let ([eou-pos (let loop ([eou-pos start-pos])
                                   (cond
                                     [(= eou-pos (send e last-position)) eou-pos]
                                     [(char-whitespace? (send e get-character eou-pos))
				      ;; Back up past ., ,, >, ", and ):
				      (let loop ([eou-pos eou-pos])
					(if (memq (send e get-character (sub1 eou-pos))
						  '(#\" #\. #\, #\> #\)))
					    (loop (sub1 eou-pos))
					    eou-pos))]
                                     [else (loop (+ eou-pos 1))]))])
                    (send e change-style url-delta start-pos eou-pos)
                    (send e set-clickback start-pos eou-pos 
                          (lambda (e start-pos eou-pos)
                            (send-url (send e get-text start-pos eou-pos))))
                    (loop eou-pos)))))))
        (hilite-urls/prefix "http:")
        (hilite-urls/prefix "https:")
        (hilite-urls/prefix "ftp:"))))
