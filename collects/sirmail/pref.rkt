(module pref mzscheme

  (require mzlib/class
	   framework
	   mred
	   mzlib/list
	   mzlib/string
	   mzlib/etc
	   net/head)

  ;; IMPORTANT! All preferences operations outside this
  ;; file should go through the following exports.
  ;; DO NOT use preferences:... elsewhere.
  (provide get-pref put-pref
	   show-pref-dialog
	   add-preferences-menu-items)

  (define (string-or-false? x) (or (not x) (string? x)))
  (define (ip-string? x) (and (string? x)
			      (positive? (string-length x))))
  (define (abs-path-or-false? x) 
    (or (not x)
	(and (path? x) (absolute-path? x))))

  (define (un/marshall-path pref)
    (preferences:set-un/marshall pref
				 (lambda (x) 
                                   (if (path? x)
                                       (path->bytes x)
                                       x))
				 (lambda (x) 
                                   (cond
                                     [(bytes? x) (bytes->path x) ]
                                     [(not x) x]
                                     [else 'badvalue]))))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Preference Definitions                                 ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  (preferences:set-default 'sirmail:mail-from "SirMail User <sirmail@racket-lang.org>" string?)
  (preferences:set-default 'sirmail:username "username" string?)
  (preferences:set-default 'sirmail:password #f string-or-false?)
  (preferences:set-default 'sirmail:default-to-domain "racket-lang.org" ip-string?)

  (preferences:set-default 'sirmail:imap-server "imap.racket-lang.org" ip-string?)
  (preferences:set-default 'sirmail:use-ssl? #f boolean?)
  (preferences:set-default 'sirmail:server-certificate #f abs-path-or-false?)
  (preferences:set-default 'sirmail:smtp-server "sendmail.racket-lang.org" ip-string?)

  (preferences:set-default 'sirmail:local-directory 
			   (build-path (find-system-path 'home-dir)
				       "SirMail")
			   (lambda (x)
			     (and (path? x)
				  (absolute-path? x))))
  (un/marshall-path 'sirmail:local-directory)
  (preferences:set-default 'sirmail:sent-directory 
			   (build-path (find-system-path 'home-dir)
				       "SentMail")
			   (lambda (x)
			     (or (not x)
				 (and (path? x)
				      (absolute-path? x)))))
  (un/marshall-path 'sirmail:sent-directory)
  (preferences:set-default 'sirmail:root-mailbox-folder #f string-or-false?)

  (preferences:set-default 'sirmail:archive-mailbox-folder #f string-or-false?)
  
  (preferences:set-default 'sirmail:initial-sort 'id
			   (lambda (x) (memq x '(id date subject from))))
  (preferences:set-default 'sirmail:biff-delay
			   60
			   (lambda (x)
			     (or (not x)
				 (and (number? x)
				      (exact? x)
				      (integer? x)
				      (positive? x)))))
  (preferences:set-default 'sirmail:warn-download-size 32000
			   (lambda (x) (or (not x) (and (number? x) (real? x)))))
  (preferences:set-default 'sirmail:external-composer 'xemacs
			   (lambda (x) (memq x '(xemacs gnu-emacs))))
  (preferences:set-default 'sirmail:use-extenal-composer? #f boolean?)
  (preferences:set-default 'sirmail:show-urls? #t boolean?)
  (preferences:set-default 'sirmail:show-gc-icon #f boolean?)
  (preferences:set-default 'sirmail:always-happy #f boolean?)
  (preferences:set-default 'sirmail:wrap-lines #f boolean?)
  (preferences:set-default 'sirmail:prefer-text #t boolean?)

  (preferences:set-default 'sirmail:aliases-file
                           (build-path (find-system-path 'home-dir) ".sirmail.aliases")
			   abs-path-or-false?)
  (un/marshall-path 'sirmail:aliases-file)
  (preferences:set-default 'sirmail:auto-file-table-file (build-path (find-system-path 'home-dir) ".sirmail.auto-file")
			   abs-path-or-false?)
  (un/marshall-path 'sirmail:auto-file-table-file)

  (preferences:set-default 'sirmail:self-addresses null
			   (lambda (x) (and (list? x) (andmap string? x))))
  (preferences:set-default 'sirmail:fields-to-show '("From" "To" "CC" "Subject" "Date" "X-Mailer" "X-Uptime")
			   (lambda (x) (and (list? x) (andmap string? x))))
  (preferences:set-default 'sirmail:bcc #f
                           (λ (x) (or (not x) (string? x))))

  (let ([fw 560]
	[fh 600])
    (let-values ([(display-width display-height) (get-display-size)])
      (preferences:set-default 'sirmail:frame-width
			       (min display-height fh)
			       (lambda (x) (and (number? x) (<= 0 x 32768))))
      (preferences:set-default 'sirmail:frame-height 
			       (min display-width fw)
			       (lambda (x) (and (number? x) (<= 0 x 32768))))))

  (define (xywh-okay? n)
    (and (number? n)
         (<= 0 n 10000)))
  (preferences:set-default 'sirmail:folder-window-w 200 xywh-okay?)
  (preferences:set-default 'sirmail:folder-window-h 400 xywh-okay?)
  (preferences:set-default 'sirmail:folder-window-x 0 xywh-okay?)
  (preferences:set-default 'sirmail:folder-window-y 0 xywh-okay?)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Preference Manager                                     ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define prefs-eventspace (make-eventspace))

  (define (in-preferences-eventspace thunk)
    (let ([val #f]
	  [s (make-semaphore)])
      (parameterize ([current-eventspace prefs-eventspace])
	(queue-callback
	 (lambda ()
	   (with-handlers ([void (lambda (x)
				   ;; Assume all raised values are exns
				   (set! val x))])
	     (set! val (thunk)))
	   (semaphore-post s))))
      (semaphore-wait s)
      (if (exn? val)
	  (raise val)
	  val)))

  (define (get-pref id)
    (in-preferences-eventspace (lambda ()
				 (preferences:get id))))

  (define (put-pref id val)
    (in-preferences-eventspace (lambda ()
				 (preferences:set id val))))

  (define (add-preferences-menu-items edit-menu)
    (make-object separator-menu-item% edit-menu)
    (make-object menu-item% "Preferences" edit-menu
		 (lambda (x y) (in-preferences-eventspace preferences:show-dialog))))
  
  (define (show-pref-dialog)
    (in-preferences-eventspace 
     (lambda ()
       (preferences:show-dialog)
       (yield 'wait))))


  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Preference Dialog                                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define needs-check null)

  
  (define (set-hilite e on?)
    (send e change-style 
	  (send (make-object style-delta%) set-delta-background (if on? "yellow" "white"))
	  0 (send e last-position)))

  ;; make-text-field : string panel number symbol boolean
  ;;                   ((union #f string) (union #f top-level-window<%>) string -> boolean)
  ;;                   (any -> string)
  ;;                   (string -> any)
  ;;                   [ (union #f string) ]
  ;;                -> void
  ;; sets up a text field for a preference
  ;; The 3rd-to-last argument checks the validity of the field content.If
  ;;  a string is provided, then a top-level-window<%> is also provded, and
  ;;  the checking function should tell the user why the field-value string
  ;;  is bad if it is bad.
  ;; the last two arguments convert between the string representation (as shown in the text field)
  ;; and the preferences's actual Scheme value.
  (define make-text-field
    (opt-lambda (label panel width-num pref optional? check-value val->str str->val [post-label #f])
      (define p0 (and (or optional? post-label)
		      (instantiate horizontal-panel% (panel) [stretchable-height #f])))
      (define e (and optional?
		     (make-object check-box% label p0
				  (lambda (c e)
				    (let ([on? (send c get-value)])
				      (send t enable on?)
				      (if on?
					  (t-cb t e)
					  (begin
					    ;; remove all need-check registrations, if any:
					    (let loop ()
					      (let ([a (assq t needs-check)])
						(when a
						  (set! needs-check (remq a needs-check))
						  (loop))))
					    (preferences:set pref #f))))))))
      (define t-cb (lambda (t e)
		     (let* ([s (send t get-value)])
		       (if (check-value #f #f s)
			   (preferences:set pref (str->val s))
			   (begin
			     (set! needs-check (cons (list t label check-value) needs-check))
			     (set-hilite (send t get-editor) #t))))))
      (define t (make-object text-field% 
			     (if optional? #f label) 
			     (or p0 panel)
			     t-cb
			     (make-string width-num #\X)))
      (when post-label
	(send t stretchable-width #f)
	(make-object message% post-label p0))

      (send t set-value (let ([v (preferences:get pref)])
			  (if v
			      (val->str v)
			      "")))
      (when optional?
	(send e set-value (preferences:get pref)))
      (when e
	(send t enable (send e get-value)))
      (preferences:add-callback pref (lambda (name val)
				       (set-hilite (send t get-editor) #f)
				       (when e
					 (send e set-value val)
					 (send t enable val))
				       (when val
					 (let ([sval (val->str val)])
					   (unless (equal? sval (send t get-value))
					     (send t set-value sval))))))

      (or p0 t)))

  (define (check-unsaved-pref?)
    (and (andmap (lambda (a)
		   ((caddr a) (cadr a) (send (car a) get-top-level-window) (send (car a) get-value)))
		 needs-check)
	 (begin
	   (set! needs-check null)
	   #t)))
	     
  (define make-file/directory-button
    (lambda (dir? button-label parent pref enabler)
      (define p0 (and enabler
		      (instantiate horizontal-panel% (parent) [stretchable-height #f])))
      (define e (and enabler
		     (make-object check-box% enabler p0
				  (lambda (c e)
				    (let ([on? (send c get-value)])
				      (send p enable on?)
				      (preferences:set
				       pref
				       (and on?
					    (string->path (send field get-value)))))))))
      (define p (instantiate horizontal-panel% ((or p0 parent))
			     [stretchable-height #f]))
      (define (set-it v)
	(preferences:set pref v))
      (define field (make-object text-field% button-label p
				 ;; For now, just counteract edits:
				 (lambda (t e)
				   (send field set-value (path->string (preferences:get pref))))
                      (path->string
                       (or (preferences:get pref)
                           (current-directory)))))
      (when e
	(send e set-value (preferences:get pref))
	(send p enable (send e get-value)))
      (preferences:add-callback pref (lambda (name val)
				       (when e
					 (send e set-value val)
					 (send p enable val))
				       (when val
					 (send field set-value (path->string val)))))
      (make-object button% "Set..." p (lambda (b e)
					(let ([v ((if dir? get-directory get-file)
						  (or enabler button-label))])
					  (when v
					    (set-it v)))))
      p0))

  (define make-boolean
    (opt-lambda (label p pref [extra-action void])
      (define c
	(make-object check-box% label p (lambda (c e)
					  (let ([v (send c get-value)])
					    (extra-action v)
					    (preferences:set pref v)))))
      (send c set-value (preferences:get pref))
      (preferences:add-callback pref (lambda (name val)
				       (send c set-value val)))))

  (define (is-host-address? s)
    (regexp-match "^([-a-zA-Z0-9]+[.])*[-a-zA-Z0-9]+$" s))

  (define (is-host-address+port? s)
    (or (is-host-address? s)
	(let ([m (regexp-match "^(.*):([0-9]+)$" s)])
	  (and m
	       (<= 1 (string->number (caddr m)) 65535)
	       (is-host-address? (cadr m))))))

  (define (is-host-address+port+user? s)
    (or (is-host-address+port? s)
	(let ([m (regexp-match "^(?:[-+a-zA-Z0-9_.]+)@(.*)$" s)])
	  (and m
	       (is-host-address+port? (cadr m))))))

  (define (is-host-address+port+user+type? s)
    (or (is-host-address+port+user? s)
	(let ([m (regexp-match "^(?:ssl|tcp):(.*)$" s)])
	  (and m
	       (is-host-address+port+user? (cadr m))))))

  (define (is-host-address+port+user+type-list? s)
    (let ([l (regexp-split ", *" s)])
      (andmap is-host-address+port+user+type? l)))

  (define (check-address ok? who tl s port-ok? multi?)
    (or (ok? s)
	(begin
	  (when who
	    (message-box
	     "Preference Error"
	     (format (string-append
		      "The ~a value must be a~a host IP address~a~a~a.\n"
		      "An IP address is an string containing a combination of "
		      "period (.), dash (-), A-Z, a-Z, and 0-9. "
		      "Also the period cannot appear at the very beginning or end.\n"
		      "~a"
		      "You provided\n\n  ~a\n\nwhich is not legal.")
		     who 
		     (if multi? " comma-separated list of" "")
		     (if multi? " es" "")
		     (if (and multi? port-ok?) " each" "")
		     (if port-ok? " with an optional port number" "")
		     (if port-ok? 
			 (string-append
			  "An optional port number is specified by adding a "
			  "colon (:) followed by a number between 1 and 65535.\n")
			 "")
		     s)
	     tl
	     '(ok stop)))
	  #f)))
			
  (define (check-host-address who tl s)
    (check-address is-host-address? who tl s #f #f))
  (define (check-host-address/port who tl s)
    (check-address is-host-address+port? who tl s #t #f))
  (define (check-host-address/port/user/type/multi who tl s)
    (check-address is-host-address+port+user+type-list? who tl s #t #t))

  ;; check-biff-delay : (union #f string) (union #f parent) string -> boolean
  ;; checks to see if the string in the biff delay field makes
  ;; sense as an exact integer between 1 and 3600
  (define (check-biff-delay who tl s)
    (let ([n (string->number s)])
      (or (and (number? n)
	       (integer? n)
	       (exact? n)
	       (<= 1 n 3600))
	  (begin
	    (when who
	      (message-box
	       "Preference Error"
	       (format (string-append
			"The biff delay must be an exact integer between 1 and 3600.~n"
			"You provided:~n"
			"  ~a")
		       s)
	       tl
	       '(ok stop)))
	    #f))))

  
  ;; check-message-size : (union #f string) (union #f parent) string -> boolean
  ;; checks to see if the string in the download-max-size field makes
  ;; sense as an exact positive integer
  (define (check-message-size who tl s)
    (let ([n (string->number s)])
      (or (and (number? n)
	       (integer? n)
	       (exact? n)
	       (positive? n))
	  (begin
	    (when who
	      (message-box
	       "Preference Error"
	       (format (string-append
			"The message size must be an exact, positive integer.~n"
			"You provided:~n"
			"  ~a")
		       s)
	       tl
	       '(ok stop)))
	    #f))))
  
  (define (check-user-address who tl s)
    (with-handlers ([exn:fail? 
		     (lambda (x)
		       (when who
			 (message-box
			  "Preference Error"
			  (format "The ~a value you provided is not a legal mail address: ~a"
				  who s)
			  tl
			  '(ok stop)))
		       #f)])
      (unless (= 1 (length (extract-addresses s 'all)))
	(error "multiple addresses"))
      #t))

  (define (check-simple-user-address who tl s)
    (and (check-user-address who tl s)
	 (car (extract-addresses s 'address))))

  (define (check-id who tl s) #t)
      
  (define (make-text-list label parent pref check-item)
    (let ([p (make-object group-box-panel% label parent)])
      (define l (make-object list-box% #f (or (preferences:get pref) null) p
			     (lambda (l e)
			       (send delete enable (pair? (send l get-selections))))
			     '(multiple)))
      (define hp (instantiate horizontal-panel% (p) 
			      [stretchable-height #f]
			      [alignment '(center center)]))
      (define add (make-object button% "Add" hp (lambda (b e)
						  (let loop ([init ""])
						    (let ([v (get-text-from-user (format "Add to ~a" label)
										 (format "Add to ~a" label)
										 (send parent get-top-level-window)
										 init)])
						      (when v
							(let ([revised (check-item (format "item for ~a" label)
										   (send b get-top-level-window) v)])
							  (if revised
							      (begin
								(send l append (if (string? revised) revised v))
								(set-prefs))
							      (loop v)))))))))
      (define delete (make-object button% "Delete" hp (lambda (b e)
							(let ([d (send l get-selections)])
							  (for-each (lambda (i)
								      (send l delete i))
								    (sort d >))
							  (set-prefs)))))
      (define (set-prefs)
	(send delete enable (pair? (send l get-selections)))
	(preferences:set
	 pref
	 (let ([n (send l get-number)])
	   (let loop ([i 0])
	     (if (= i n)
		 null
		 (cons (send l get-string i)
		       (loop (add1 i))))))))
      (send delete enable #f)
      (preferences:add-callback pref (lambda (name val)
				       (send l clear)
				       (for-each (lambda (i)
						   (send l append i))
						 val)
				       (send delete enable (pair? (send l get-selections)))))))

  (define (make-addresses-preferences-panel parent)
    (let ([p (instantiate vertical-panel% (parent))])
      
      (make-text-field "Mail From" p 20 'sirmail:mail-from #f check-user-address (lambda (x) x) (lambda (x) x))
      (make-text-field "SMTP Server" p 20 'sirmail:smtp-server #f check-host-address/port/user/type/multi 
		       (lambda (x) x) (lambda (x) x))

      (make-file/directory-button #t #f p
				  'sirmail:sent-directory
				  "Save Sent Files")


      (make-text-field "Default \"To\" domain" p 20 'sirmail:default-to-domain #f check-host-address (lambda (x) x) (lambda (x) x))
      (make-text-field "BCC line" p 20 'sirmail:bcc #t void (lambda (x) x) (lambda (x) x))
      (make-file/directory-button #f #f p
				  'sirmail:aliases-file
				  "Aliases File")

      (make-text-list "Self Addresses" p 'sirmail:self-addresses check-simple-user-address)

      (make-boolean "Enable compose-with-Emacs" p 'sirmail:use-extenal-composer?)

      p))

  (define (make-mbox-preferences-panel parent)
    (let ([p (instantiate vertical-panel% (parent)
               (alignment '(left center)))])
      
      (make-text-field "Username" p 10 'sirmail:username #f check-id (lambda (x) x) (lambda (x) x))
      (let ([sp (instantiate group-box-panel% ("IMAP Server" p)
			     [alignment '(left center)])]
	    [cert #f])
	(make-text-field "Server" sp 20 'sirmail:imap-server #f check-host-address/port (lambda (x) x) (lambda (x) x))
	(make-boolean "Encrypt connection using SSL" sp 'sirmail:use-ssl?
		      (lambda (on?) (send cert enable on?)))
	(set! cert (make-file/directory-button #f #f sp
					       'sirmail:server-certificate
					       "Verify SSL with certificates"))
        (make-text-field "Archive folder" sp 20 'sirmail:archive-mailbox-folder #t void (lambda (x) x) (lambda (x) x))
	(make-text-field "Folder list root" sp 20 'sirmail:root-mailbox-folder #t void (lambda (x) x) (lambda (x) x))

	(send cert enable (preferences:get 'sirmail:use-ssl?)))

      (make-file/directory-button #t "Local directory" p
				  'sirmail:local-directory
				  #f)

      (make-text-field "Check mail every" p 5 'sirmail:biff-delay #t check-biff-delay number->string string->number
		       "seconds")

      (make-text-field "Verify download of messages larger than" p 10
		       'sirmail:warn-download-size #t 
		       check-message-size number->string string->number
		       "bytes")

      (make-file/directory-button #f #f p
				  'sirmail:auto-file-table-file
				  "Auto-file table file")
      
      (make-boolean "Show GC icon" p 'sirmail:show-gc-icon)
      (make-boolean "Always happy to get mail" p 'sirmail:always-happy)
      
      (make-text-list "Shown Header Fields" p 'sirmail:fields-to-show void)

      p))

  (in-preferences-eventspace
   (lambda ()
     (preferences:add-panel "Reading" make-mbox-preferences-panel)
     (preferences:add-panel "Sending" make-addresses-preferences-panel)
     (preferences:add-editor-checkbox-panel)
     (preferences:add-can-close-dialog-callback check-unsaved-pref?))))

