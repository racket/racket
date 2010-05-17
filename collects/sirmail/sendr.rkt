
;; This module implements the mail-composing window. The `new-mailer'
;;  function creates a compose-window instance.

(module sendr scheme/base
  (require scheme/tcp
           scheme/unit
           scheme/class
           scheme/string
	   mred/mred-sig
           framework)

  (require scheme/file
	   mzlib/process
	   openssl/mzssl)

  (require "sirmails.ss"
	   "pref.ss"
           "spell.ss")

  (require net/imap-sig
	   net/smtp-sig
	   net/head-sig
	   net/base64-sig
	   net/qp-sig)

  (require mrlib/hierlist/hierlist-sig)

  (define smtp-passwords (make-hash))

  (provide send@)
  (define-unit send@
      (import sirmail:exit^
	      sirmail:utils^
	      sirmail:options^
	      sirmail:read^
	      (prefix env: sirmail:environment^)
	      mred^
	      imap^
	      smtp^
	      head^
	      base64^
	      qp^
	      hierlist^)
      (export sirmail:send^)

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Constants                                             ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (show-error x main-frame)
	(show-error-message-box x main-frame))
        
      (define FRAME-WIDTH 560)
      (define FRAME-HEIGHT 600)
      (let-values ([(display-width display-height) (get-display-size)])
	(set! FRAME-HEIGHT (min display-height FRAME-HEIGHT))
	(set! FRAME-WIDTH (min display-width FRAME-WIDTH)))

      (define FORWARD-LIST-HEIGHT 50)

      (define return-bitmap
	(with-handlers ([void (lambda () #f)])
	  (let ([bm (make-object bitmap% 
				 (build-path 
				  (collection-path "icons")
				  "return.xbm"))])
	    (and (send bm ok?) bm))))

      (define send-icon (make-object bitmap% (build-path (collection-path "sirmail")
							 "stamp.bmp")))
      (define send-icon-mask (make-object bitmap% (build-path (collection-path "sirmail")
							      "stamp-mask.xbm")))
      (unless (and (send send-icon ok?)
		   (send send-icon-mask ok?))
	(set! send-icon #f))

      (define SEPARATOR (make-string 75 #\=))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Address Parsing                                       ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      ;; Returns a list of <full>-<address> pairs
      (define (resolve-alias addr)
	(cond
	 [(assoc addr (ALIASES))
	  => (lambda (m)
	       (let ([resolve
		      (lambda (n)
			(let ([l (sm-extract-addresses n)])
			  (unless (> (length l) 0)
			    (error 'resolve-alias "alias is not an address: ~a" n))
			  l))])
		 (if (list? (cadr m))
		     (apply append (map resolve (cadr m)))
		     (resolve (cadr m)))))]
	 [(DEFAULT-DOMAIN) (let ([addr (format "~a@~a" addr (DEFAULT-DOMAIN))])
			     (list (cons addr addr)))]
	 [else (list (cons addr addr))]))

      ;; Returns a list of <full>-<address> pairs
      (define (sm-extract-addresses s)
	(let ([addrs (extract-addresses s 'all)])
	  (apply
	   append
	   (map
	    (lambda (a)
	      (let ([name (car a)]
		    [address (cadr a)]
		    [full (caddr a)])
		(if (and (string=? address full)
			 (not (regexp-match "@" full)))
		    (resolve-alias full)
		    (list (cons full address)))))
	    addrs))))
      
      (define (remove-fields l h)
	(if (null? l)
	    h
	    (remove-fields (cdr l) (remove-field (car l) h))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Enclosures                                            ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define-struct enclosure (name            ; identifies enclosure in the GUI
				subheader       ; header for enclosure
				data-thunk)     ; gets enclosure data as bytes (already encoded)
        #:mutable)

      ;; Create a message with enclosures.
      ;;  `header' is a message header created with the head.ss library
      ;;  `body-lines' is a list of strings and byte strings
      ;;  `enclosures' is a list of `enclosure' structs
      (define (enclose header body-lines enclosures)
        (define qp-body-lines?
          (ormap (lambda (l)
                   (or ((string-length l) . > . 1000)
                       (regexp-match? #rx"[^\0-\177]" l)))
                 body-lines))
        (define (encode-body-lines)
          (if qp-body-lines?
              (map
               bytes->string/utf-8
               (regexp-split #rx"\r\n"
                             (qp-encode (string->bytes/utf-8 
                                         (string-join body-lines "\r\n")))))
              body-lines))
        (define (add-body-encoding-headers header)
          (insert-field
           "Content-Type"
            "text/plain; charset=UTF-8"
            (insert-field
             "Content-Transfer-Encoding"
             (if qp-body-lines? "quoted-printable" "7bit")
             header)))
	(if (null? enclosures)
	    (values (insert-field
                     "MIME-Version"
                     "1.0"
                     (add-body-encoding-headers
                      header))
		    (encode-body-lines))
	    (let* ([enclosure-datas
                    (map (lambda (e) ((enclosure-data-thunk e))) enclosures)]
                   [boundary
                    ;; Generate something that isn't there:
                    (let loop ()
                      (let* ([b (format "---~a~a~a-----" (random 10000) (random 10000) (random 10000))]
                             [m (regexp b)])
                        (if (or (ormap (lambda (bl)
					 (regexp-match-positions m bl))
				       body-lines)
                                (ormap
                                 (lambda (enc data)
                                   (or (regexp-match-positions m (enclosure-subheader enc))
                                       (ormap (lambda (bl)
						(regexp-match-positions m bl))
					      data)))
                                 enclosures enclosure-datas))
                            (loop)
                            b)))])
	      (let ([mime-header (insert-field
				  "MIME-Version"
				  "1.0"
				  (insert-field
				   "Content-Type"
				   (data-lines->data
				    (list
				     "multipart/mixed;"
				     (format "boundary=~s"
					     boundary)))
				   empty-header))])
		(values (append-headers header mime-header)
			(append
			 (list
			  "This is a multi-part message in MIME format."
			  (format "--~a" boundary))
			 (header->lines
                          (add-body-encoding-headers
                           empty-header))
                         (encode-body-lines)
                         (apply
                          append
                          (map
                           (lambda (enc data)
                             (cons
                              (format "--~a" boundary)
                              (append
                               (header->lines
                                (enclosure-subheader enc))
                               data)))
                           enclosures enclosure-datas))
                         (list
                          (format "--~a--" boundary))))))))
      
      (define (get-enclosure-type-and-encoding filename mailer-frame auto?)
        (let ([types '("application/postscript"
                       "text/plain"
                       "text/plain; charset=UTF-8"
                       "text/html"
                       "image/jpeg"
                       "image/gif"
                       "image/png"
                       "application/octet-stream")]
              [encodings '("7bit"
                           "quoted-printable"
                           "base64")]
              [d (instantiate dialog% ("Enclosure" mailer-frame)
                   [alignment '(left center)])])
          (make-object message% (string-append 
                                 "File: "
				 (let ([filename (path->string filename)])
				   (let ([l (string-length filename)])
				     (if (l . < . 58)
					 filename
					 (string-append
					  (substring filename 0 5)
					  "..."
					  (substring filename (- l 50) l))))))
		       d)
          (let ([type-list (make-object choice% "Type:" types d void)]
                [encoding-list (make-object choice% "Encoding:" encodings d void)]
		[inline-check (make-object check-box% "Inline in recipient's view" d void)]
                [button-panel (instantiate horizontal-pane% (d)
                                [alignment '(right center)]
                                [stretchable-height #f])]
                [ok? auto?])
            (let-values ([(ok cancel) (gui-utils:ok/cancel-buttons
                                       button-panel
                                       (lambda (b e) 
                                         (set! ok? #t)
                                         (send d show #f))
                                       (lambda (b e)
                                         (send d show #f)))])
              (let ([default (lambda (t e inline?)
                               (letrec ([findpos (lambda (l s)
                                                   (if (string=? (car l) s)
                                                       0
                                                       (add1 (findpos (cdr l) s))))])
				 (send type-list set-selection (findpos types t))
				 (send encoding-list set-selection (findpos encodings e))
                                 (send inline-check set-value inline?)))]
                    [suffix (let ([m (regexp-match #rx"[.](.*)$" (path->string filename))])
                              (and m (cadr m)))])
                (case (if suffix (string->symbol (string-locale-downcase suffix)) '???)
                  [(txt ss scm) (default "text/plain" "quoted-printable" #f)]
                  [(htm html) (default "text/html" "quoted-printable" #f)]
                  [(ps) (default "application/postscript" "base64" #f)]
                  [(jpeg jpg) (default "image/jpeg" "base64" #t)]
                  [(png) (default "image/png" "base64" #t)]
                  [(gif) (default "image/gif" "base64" #t)]
                  [else (default "application/octet-stream" "base64" #f)]))
	      (unless auto?
		(send d show #t))
              (if ok?
                  (values (list-ref types (send type-list get-selection))
                          (list-ref encodings (send encoding-list get-selection))
			  (send inline-check get-value))
                  (values #f #f #f))))))
			
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Composer Instance                                     ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      ;; new-mailer : ... -> frame[with send-message method]
      (define (new-mailer file to cc subject other-headers body enclosures message-count)
        (define f% (class frame:basic%
                     (inherit get-menu-bar set-icon get-eventspace accept-drop-files)
                     [define/public (send-message)
                       (send-msg)]
                     (define/augment (can-close?)
                       (and (send (get-menu-bar) is-enabled?)
                            (or (not (send message-editor is-modified?))
                                (eq? 'yes
                                     (confirm-box
                                      "Warning"
                                      "The message is not saved or sent. Close anyway?"
                                      this)))
                            (inner #t can-close?)))
                     (define/augment (on-close)
                       (send message-editor on-close)
                       (inner (void) on-close)
                       (exit-sirmail "mailer close"))
		     (define/override (on-drop-file f)
		       (add-enclosure-file f #t))
                     (super-instantiate ())
		     (accept-drop-files #t)
                     (when send-icon
                       (set-icon send-icon send-icon-mask))))
        (define mailer-frame (make-object f% "Send Mail" #f FRAME-WIDTH FRAME-HEIGHT))
        
        (define mb (send mailer-frame get-menu-bar))
        (define file-menu (make-object menu% "File" mb))
        (define edit-menu (make-object menu% "Edit" mb))
        (define composer-menu (and (USE-EXTERNAL-COMPOSER?)
                                   (make-object menu% "Composer" mb)))
        (define button-pane (make-object horizontal-pane% (send mailer-frame get-area-container)))
        (define title-message (make-object message% "Compose message" button-pane)) 
        (define button-pane-spacer (make-object vertical-pane%  button-pane))
        (define cancel-button
          (make-object button% "Stop" button-pane
            (lambda (b e) (cancel-button-todo))))
        (define cancel-button-todo void)
        
        (define external-composer-button
          (and (USE-EXTERNAL-COMPOSER?)
               (make-object
                   button%
                 "External Composer"
                 button-pane
                 (lambda (button control-event)
                   (let ([t (make-temporary-file "sirmail~a")])
                     
                     (send message-editor save-file t 'text #t)
                     
                     ; To get rid of the Standard Output window: Set
                     ; the current output & error ports to something
                     ; else. Or use `process' instead.  Warning: be sure
                     ; to test in error circumstances (eg, when the
                     ; external program can't be found).
                     
                     (system
                      (case external-composer
                        [(xemacs)
                         (string-append "gnuclient +5 " t)]
                        [(gnu-emacs)
                         (string-append "emacsclient +5 " t)]))
                     
                     (send message-editor load-file t 'guess #t)
                     
                     (with-handlers
                         ([exn:fail:filesystem?
                           (lambda (exn)
                             (message-box "Error Deleting Temporary File"
                                          (string-append
                                           "Attempted to delete the "
                                           "temporary file "
                                           "`" t "'"
                                           "but couldn't find it.")
                                          #f
                                          '(ok)))])
                       (delete-file t)))))))
        
        (define c (new canvas:color% 
                       [parent (send mailer-frame get-area-container)]
                       [style '(auto-hscroll)]))
        (define message-editor-super%
          (color:text-mixin 
           (editor:backup-autosave-mixin
            (text:foreground-color-mixin
             text:standard-style-list%))))
        (define message-editor (make-object (class message-editor-super%
                                              (inherit reset-region)
                                              
                                              (define immutable-start 0)
                                              (define immutable-end 0)
                                              
                                              (define/override (set-modified mod?)
                                                (send mailer-frame modified mod?)
                                                (super set-modified mod?))
                                              (define/public (set-no-change-region start end)
                                                (set! immutable-start start)
                                                (set! immutable-end end)
                                                (reset-region end 'end))
                                              
                                              (define/augment (can-insert? start len)
                                                (and (or (<= start immutable-start)
                                                         (>= start immutable-end))
                                                     (inner #t can-insert? start len)))
                                              (define/augment (after-insert start len)
                                                (when (<= start immutable-start)
						  (set! immutable-start (+ immutable-start len))
						  (set! immutable-end (+ immutable-end len))
						  (reset-region immutable-end 'end))
                                                (inner (void) after-insert start len))
                                              
                                              (define/augment (can-delete? start len)
                                                (and (or (<= (+ start len) immutable-start)
                                                         (>= start immutable-end))
                                                     (inner #t can-delete? start len)))
                                              (define/augment (after-delete start len)
                                                (when (<= start immutable-start)
						  (set! immutable-start (- immutable-start len))
						  (set! immutable-end (- immutable-end len))
						  (reset-region immutable-end 'end))
                                                (inner (void) after-delete start len))
                                              
                                              (super-new))))
        (define enclosure-list (make-object hierarchical-list% (send mailer-frame get-area-container)))
        
        (define plain-cursor (make-object cursor% 'arrow))
        (define arrow+watch-cursor (make-object cursor% 'arrow+watch))
        
        (define (enable on? refocus cancel-proc)
          (let ([w (send mailer-frame get-focus-window)])
            (set! cancel-button-todo cancel-proc)
            (send mb enable on?)
            (send c enable on?)
            (send cancel-button enable (not on?))
            (let* ([cursor (if on? plain-cursor arrow+watch-cursor)])
              (send mailer-frame set-cursor cursor)
              (send (send c get-editor) set-cursor (if on? #f cursor) #t))
            (when (and on? refocus)
              (send refocus focus))
            w))
        
        (define (send-msg)
          (define-values (smtp-ssl? smtp-auth-user smtp-server-to-use smtp-port-to-use)
            (parse-server-name+user+type (SMTP-SERVER) 25))
	  (define smtp-auth-passwd (and smtp-auth-user
					(or (hash-ref smtp-passwords (cons smtp-auth-user smtp-server-to-use)
                                                      (lambda () #f))
					    (let ([p (get-pw-from-user smtp-auth-user mailer-frame)])
					      (unless p (raise-user-error 'send "send canceled"))
					      p))))
          (send-message
           (send message-editor get-text)
	   smtp-ssl?
           smtp-server-to-use
           smtp-port-to-use
	   smtp-auth-user smtp-auth-passwd
           (map (lambda (i) (send i user-data)) 
                (send enclosure-list get-items))
           enable
           (lambda () (send mailer-frame set-status-text "Sending mail..."))
           (lambda () (send mailer-frame set-status-text "Building enclosures..."))
           (lambda () (send mailer-frame set-status-text ""))
           (lambda ()
             (send mailer-frame on-close)
             (send mailer-frame show #f))
           (lambda ()
             (let loop ()
               (when (eq? (message-box "Save?" "Save message before killing?" #f '(yes-no caution))
                          'yes)
                 (let ([f (put-file)])
                   (if f
                       (send message-editor save-file f 'text)
                       (loop))))))
           message-count)
	  (when smtp-auth-passwd
	    (hash-set! smtp-passwords (cons smtp-auth-user smtp-server-to-use) 
                       smtp-auth-passwd)))
        
        ;; enq-msg : -> void
        ;; enqueues a message for a later send
        (define (enq-msg)
          (let ([filename (get-fresh-queue-filename)])
            (send message-editor save-file filename 'text))
          
          (when (send mailer-frame can-close?)
            (send mailer-frame on-close)
            (send mailer-frame show #f)))
        
        ;; get-fresh-queue-filename : -> string
        (define (get-fresh-queue-filename)
          (build-path queue-directory 
                      (format "enq~a" (+ 1 (length (directory-list queue-directory))))))

	(define (add-enclosure-file file auto?)
	  (let-values ([(type encoding inline?) (get-enclosure-type-and-encoding file mailer-frame auto?)])
	    (when (and type encoding)
	      (let ([i (send enclosure-list new-item)]
		    [enc (make-enclosure
			  (path->string file)
			  (let ([fn (clean-filename
				     (with-handlers ([void (lambda (x) "unknown")])
				       (let-values ([(base name dir?) (split-path file)])
					 (path->string name))))])
			    (insert-field
			     "Content-Type" 
			     (data-lines->data
			      (list
			       (string-append type ";")
			       (format "name=~s" fn)))
			     (insert-field
			      "Content-Transfer-Encoding" encoding
			      (insert-field
			       "Content-Disposition"
			       (data-lines->data
				(list
				 (format "~a; " (if inline? 'inline 'attachment))
				 (format "filename=~s" fn)))
			       empty-header))))
			  (lambda ()
			    (let ([content (with-input-from-file file
					     (lambda ()
					       (read-bytes (file-size file))))])
			      (case (string->symbol encoding)
				[(base64) (split-crlf (base64-encode content))]
				[(quoted-printable) (split-crlf (qp-encode (lf->crlf content)))]
				[(7bit) (split-lf (crlf->lf content))]))))])
		(send (send i get-editor) insert (enclosure-name enc))
		(send i user-data enc)
		(let ([p (send mailer-frame get-area-container)])
		  (unless (memq enclosure-list (send p get-children))
		    (send p add-child enclosure-list)))))))
        
        (define external-composer (get-pref 'sirmail:external-composer))
        
        (frame:reorder-menus mailer-frame)
        (send button-pane stretchable-height #f)
        (send cancel-button enable #f)
        
        (send enclosure-list stretchable-height #f)
        (send enclosure-list min-height FORWARD-LIST-HEIGHT)
        (when (null? enclosures)
          (send (send mailer-frame get-area-container) delete-child enclosure-list))
        (for-each
         (lambda (enc)
           (let ([i (send enclosure-list new-item)])
             (send (send i get-editor) insert (enclosure-name enc))
             (send i user-data enc)))
         enclosures)
        
        (when (USE-EXTERNAL-COMPOSER?)
          (letrec ([switch (lambda (item e)
                             (if (send item is-checked?)
                                 (begin
                                   ;; Disable others:
                                   (send xemacs check (eq? xemacs item))
                                   (send gnu-emacs check (eq? gnu-emacs item))
                                   ;; Update flags
                                   (set! external-composer
                                         (cond
                                           [(send xemacs is-checked?)
                                            'xemacs]
                                           [(send gnu-emacs is-checked?)
                                            'gnu-emacs]))
                                   (put-pref 'sirmail:external-composer external-composer))
                                 ;; Turn it back on
                                 (send item check #t)))]
                   [xemacs (make-object checkable-menu-item% "XEmacs" composer-menu switch)]
                   [gnu-emacs (make-object checkable-menu-item% "GNU Emacs" composer-menu switch)])
            (send
             (case external-composer
               [(xemacs) xemacs]
               [(gnu-emacs) gnu-emacs])
             check #t)))
        
        (make-object menu-item% "Save" file-menu 
          (lambda (i ev) (send message-editor save-file #f 'text)))
        (make-object menu-item% "Send" file-menu (lambda (i ev) (send-msg)))
        (make-object menu-item% "Enqueue message" file-menu (lambda (i ev) (enq-msg)))
        (make-object separator-menu-item% file-menu)
        (make-object menu-item% "Add Enclosure..." file-menu
          (lambda (i env)
            (let ([file (get-file "Get Enclosure" mailer-frame)])
              (when file
		(add-enclosure-file file #f)))))

        (make-object separator-menu-item% file-menu)
        (make-object (class menu% 
                       (inherit get-items)
                       (define/override (on-demand)
                         (for-each (lambda (i) (send i delete)) (get-items))
                         (let ([server (SMTP-SERVER)]
                               [servers (SMTP-SERVERS)])
                           (for-each
                            (lambda (s)
                              (let ([i (make-object checkable-menu-item% s this
                                         (lambda (i e)
                                           (for-each (lambda (i) (send i check #f)) (get-items))
                                           (set-SMTP-SERVER! s)
                                           (send i check #t)))])
                                (when (string=? s server)
                                  (send i check #t))))
                            servers)))
                       (super-make-object "SMTP Server" file-menu)))
        (make-object separator-menu-item% file-menu)
        (make-object menu-item% "Close" file-menu
          (lambda (i e) 
            (when (send mailer-frame can-close?)
              (send mailer-frame on-close)
              (send mailer-frame show #f)))
          (if (eq? (system-type) 'windows) #f #\W))
        (append-editor-operation-menu-items edit-menu #t)
        ;; Strip menu key bindings
        (for-each
         (lambda (i)
           (when (is-a? i selectable-menu-item<%>)
             (send i set-shortcut #f)))
         (send edit-menu get-items))
        
        (make-object separator-menu-item% edit-menu)
        (send (instantiate menu-item% ("Delete Enclosure" edit-menu)
                [callback (lambda (i e)
                            (let ([i (send enclosure-list get-selected)])
                              (send enclosure-list delete-item i)))]
                [demand-callback (lambda (m)
                                   (send m enable (send enclosure-list get-selected)))])
              enable #f)
	(instantiate menu-item% ("Clone Message" edit-menu)
		     [callback (lambda (i e)
				 (let ([p (open-input-text-editor message-editor)]
				       [s (make-semaphore)])
				   (env:start-new-window
				    (lambda ()
				      (new-mailer p "" "" "" "" "" 
						  (map (lambda (i) (send i user-data)) 
						       (send enclosure-list get-items))
						  message-count)
				      (semaphore-post s)))
				   (semaphore-wait s)))]
		     [shortcut #\=])
        
        (add-preferences-menu-items edit-menu)
        
        (let ([km (send message-editor get-keymap)])
          (send km add-function "reflow-paragraph"
                (lambda (e ev) (reflow-paragraph 
                                e
                                (add1 (send e find-string SEPARATOR
                                            'forward 0 'eof #f)))))
          (send km map-function ":m:q" "reflow-paragraph")
          (send km map-function ":a:q" "reflow-paragraph")
          (special-option-key #t)
          
          (add-text-keymap-functions km)
          (keymap:setup-global km)
          
          (send km add-function "send-message"
                (lambda (w e) (send-msg)))
          (send km map-function ":m:return" "send-message")
          (send km map-function ":a:return" "send-message"))
        
        (make-fixed-width c message-editor #t return-bitmap)
        (send message-editor set-paste-text-only #t)
        (send message-editor set-max-undo-history 'forever)
        (send c set-editor message-editor)
        
        (activate-spelling message-editor)
        
        (send message-editor begin-edit-sequence)
        (if file
            ;; Resume/clone a composition...
	    (begin
	      (if (port? file)
		  (send message-editor insert-port file)
		  (send message-editor load-file file))
	      (let ([pos (send message-editor
			       find-string (string-append "\n" SEPARATOR "\n")
			       'forward 0)])
		(when pos
		  (send message-editor set-no-change-region 
			pos
			(+ pos 2 (string-length SEPARATOR))))))
	    ;; Build message skeleton
            (begin
              (send message-editor insert "To: ")
              (send message-editor insert (string-crlf->lf to))
              (send message-editor insert #\newline)
              (unless (string=? cc "")
                (send message-editor insert "CC: ")
                (send message-editor insert (string-crlf->lf cc))
                (send message-editor insert #\newline))
              (send message-editor insert "Subject: ")
              (send message-editor insert (string-crlf->lf subject))
              (send message-editor insert #\newline)
              (let ([bcc-header (get-pref 'sirmail:bcc)])
                (when bcc-header
                  (send message-editor insert "bcc: ")
                  (send message-editor insert bcc-header)
                  (send message-editor insert #\newline)))
              (send message-editor insert (string-crlf->lf other-headers))
              (send message-editor insert "X-Mailer: SirMail under GRacket ")
              (send message-editor insert (version))
              (send message-editor insert " (")
              (send message-editor insert (path->string (system-library-subpath)))
              (send message-editor insert ")")
              (let ([start-no-change (send message-editor last-position)])
                (send message-editor insert #\newline)
                (send message-editor insert SEPARATOR)
                (send message-editor insert #\newline)
                (send message-editor set-no-change-region 
                      start-no-change
                      (send message-editor last-position)))
              (let ([message-start (send message-editor last-position)])
                (send message-editor insert body)
                (if (string=? to "")
                    (send message-editor set-position (send message-editor paragraph-end-position 0))
                    (send message-editor set-position message-start)))
              (send message-editor change-style
                    (send (send message-editor get-style-list)
                          find-named-style
                          (editor:get-default-color-style-name))
                    0 
                    (send message-editor last-position))))

	(send message-editor clear-undos)
        (send message-editor set-modified #f)
        (send message-editor scroll-to-position 0)
        (send message-editor end-edit-sequence)
        
        (send c focus)
        
        (send mailer-frame create-status-line)
        
        (send mailer-frame show #t)
        
        (uncaught-exception-handler
         (lambda (x)
           (show-error x mailer-frame)
           ((error-escape-handler))))
        
        mailer-frame)

      
      ;; clean-filename : string -> string
      ;; builds a filename from a name by sripping out bad chars.
      (define (clean-filename name)
        (regexp-replace* "[ /:\\\"'`?*%<>$|\u0100-\U10FFFF]" name "_"))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Message Send                                          ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define (send-message message-str
			    ssl?
                            smtp-server
                            smtp-port auth-user auth-pw
                            enclosures
                            enable 
                            status-message-starting
                            status-message-enclosures
                            status-message-clear
                            status-done
                            save-before-killing
                            message-count)
        (let ([re (regexp (format "~a\n" SEPARATOR))])
          (let ([m (regexp-match-positions re message-str)])
            (if m
                (let ([header (string-append
                               (string-lf->crlf (substring message-str 0 (caar m)))
                               (build-uptime-field message-count) 
                               "\r\n"
                               empty-header)]
                      [body-lines (regexp-split 
				   #rx"\n" 
				   (substring message-str (cdar m) (string-length message-str)))])
		  (validate-header (regexp-replace* #rx"[^\x0-\xFF]" header "_"))
                  (let* ([to* (sm-extract-addresses (extract-field "To" header))]
                         [to (map encode-for-header (map car to*))]
                         [cc* (sm-extract-addresses (extract-field "CC" header))]
                         [cc (map encode-for-header (map car cc*))]
                         [bcc* (sm-extract-addresses (extract-field "BCC" header))]
                         [bcc (map encode-for-header (map car bcc*))]
                         [from (let ([l (extract-addresses (MAIL-FROM) 'full)])
                                 (unless (= 1 (length l))
                                   (error 'send "bad mail-from configuration: ~a" (MAIL-FROM)))
                                 (car l))]
                         [simple-from (let ([l (extract-addresses (MAIL-FROM) 'address)])
                                        (unless (= 1 (length l))
                                          (error 'send "bad mail-from configuration: ~a" (MAIL-FROM)))
                                        (car l))]
                         [subject (encode-for-header (extract-field "Subject" header))]
                         [prop-header (remove-fields '("To" "CC" "BCC" "Subject") header)]
                         [std-header (standard-message-header from to cc bcc subject)]
                         [new-header (append-headers std-header prop-header)]
                         [tos (map cdr (append to* cc* bcc*))])

                    (validate-header new-header)

                    (as-background
                     enable
                     (lambda (break-bad break-ok)
                       (if (null? enclosures)
                           (status-message-starting)
                           (status-message-enclosures))
                       (with-handlers ([void (lambda (x)
                                               (status-message-clear)
                                               (raise x))])
                         (break-ok)
                         (let-values ([(new-header body-lines) (enclose new-header body-lines enclosures)])
                           (break-bad)
                           (unless (null? enclosures)
                             (status-message-starting))
                           (when (SAVE-SENT)
                             (let* ([chop (lambda (s)
                                            (let ([l (string-length s)])
                                              (clean-filename (substring s 0 (min l 10)))))]
                                    [to (if (null? tos) "noone" (chop (car tos)))]
                                    [subj (if subject (chop subject) "nosubj")])
                               (let loop ([n 1])
                                 (let ([fn (build-path (SAVE-SENT) (format "~a_~a_~a" to subj n))])
                                   (if (file-exists? fn)
                                       (loop (add1 n))
                                       (with-output-to-file fn
                                         (lambda ()
                                           (display (string-crlf->lf header))
					   (map (lambda (body-line)
						  (display body-line)
						  (newline))
						body-lines))))))))
                           (break-ok)
                           (smtp-sending-end-of-message break-bad)
                           (smtp-send-message smtp-server
                                              simple-from
                                              tos
                                              new-header
                                              body-lines
                                              #:port-no smtp-port
					      #:tcp-connect (if ssl? ssl-connect tcp-connect)
					      #:auth-user auth-user
					      #:auth-passwd auth-pw))))
                       save-before-killing))
                  (status-done))
                (message-box
                 "Error"
                 (format "Lost \"~a\" separator" SEPARATOR))))))
      
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;  Meta-Q Reflowing                                      ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define reflow-wordbreak-map
	(make-object editor-wordbreak-map%))
      (send reflow-wordbreak-map set-map #\- '(line))
      (define (reflow-paragraph edit start-min)
	(let ([wbm (send edit get-wordbreak-map)])
	  (dynamic-wind
	      (lambda ()
		(send edit set-wordbreak-map reflow-wordbreak-map)
		(send edit begin-edit-sequence))
	      (lambda ()
		(let* ([p (max start-min (send edit get-start-position))]
                       [min-line (send edit position-paragraph start-min)]
                       [end-line (send edit position-paragraph p)])
                  ;; Find start and end lines that form a paragraph:
                  (let* ([start-l
                          (let loop ([start-l end-line])
                            (if (or (<= start-l min-line)
                                    (= (send edit paragraph-start-position start-l)
                                       (add1 (send edit paragraph-start-position (sub1 start-l)))))
                                start-l
                                (loop (sub1 start-l))))]
                         [end-l
                          (let loop ([end-l start-l])
                            (if (or (= end-l (send edit last-paragraph))
                                    (= (send edit paragraph-end-position end-l)
                                       (sub1 (send edit paragraph-end-position (add1 end-l)))))
                                end-l
                                (loop (add1 end-l))))])
                    ;; Remember start and end positions, and determine the paragraph prefix:
                    (let ([orig-start (send edit paragraph-start-position start-l)]
                          [end (send edit paragraph-end-position end-l)]
                          [second-line-prefix
                           (if (= start-l end-l)
                               ""
                               (let ([p (send edit paragraph-start-position (add1 start-l))])
                                 (let loop ([pe p])
                                   (case (send edit get-character pe)
                                     [(#\space #\tab #\>) (loop (add1 pe))]
                                     [else (send edit get-text p pe)]))))])
                      ;; Adjust starting position by skipping spaces on the first line:
                      (let ([start
                             (let ([start-end (send edit paragraph-end-position start-l)])
                               (let loop ([start orig-start])
                                     (cond
                                      [(= start-end start) orig-start]
                                      [(memq (send edit get-character start) '(#\space #\tab))
                                       (loop (add1 start))]
                                      [else start])))])
                        ;; Remove all line breaks, double spaces, tabs, and prefixes,
                        ;;  producing a revised start and end position:
                        (let-values ([(start end)
                                      (let loop ([start start]
                                                 [end end]
                                                 ;; l is the list of patterns to delete:
                                                 [l (list (string-append (string #\newline)
                                                                         second-line-prefix)
                                                          (string #\newline)
                                                          (string #\tab)
                                                          (string #\space #\space))])
                                        ;; Look for the first thing in l:
                                        (let ([p (send edit find-string (car l)
                                                       'forward start end)])
                                          (if (or p (pair? (cdr l)))
                                              (if p
                                                  ;; Found an instance; replace it with a single space,
                                                  ;;  and look again
                                                  (let ([len (string-length (car l))])
                                                    (send edit insert " " p (+ p len))
                                                    (loop start (- end len -1) l))
                                                  ;; Didn't find an instance; start looking for the
                                                  ;;  next thing in our list
                                                  (loop start end (cdr l)))
                                              ;; Nothing else to find, so we're done removing things
                                              (values start end))))])
                          ;; At this point the paragraph should be on a single line.
                          ;; Insert good line breaks to wrap the paragraph:
                          (let ([line-break (string-append (string #\newline)
                                                           second-line-prefix)]
                                [slp-len (string-length second-line-prefix)])
                            (let loop ([start start]
                                       [len (- start orig-start)]
                                       ;; Actually, remove ending space before we start:
                                       [end (if (or (= end start)
                                                    (not (char=?
                                                          #\space
                                                          (send edit get-character 
                                                                (sub1 end)))))
                                                end
                                                (begin
                                                  (send edit delete (sub1 end) end)
                                                  (sub1 end)))])
                              (unless (>= start end)
                                ;; Find end of the current word:
                                (let ([ebox (box start)])
                                  (send edit find-wordbreak #f ebox 'line)
                                  (let* ([p (unbox ebox)]
                                         [wlen (- p start)])
                                    (cond
                                     ;; If it's the first word on the line, or if it fits,
                                     ;; no line break
                                     [(or (zero? len) (< (+ len wlen) 72))
                                      (loop p (+ len wlen) end)]
                                     ;; If the next thing is a space, then replace the space with a
                                     ;; newline and prefix
                                     [(char=? #\space (send edit get-character start))
                                      (send edit insert line-break start (add1 start))
                                      (loop (+ p slp-len) (+ wlen -1 slp-len) 
                                            (+ slp-len end))]
                                     ;; Otherwise, insert a newline and prefix
                                     [else
                                      (send edit insert line-break start)
                                      (loop (+ p 1 slp-len) (+ wlen slp-len)
                                            (+ end 1 slp-len))]))))))))))))
	      (lambda () 
		(send edit end-edit-sequence)
		(send edit set-wordbreak-map wbm)))
	  #t)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;  Uptime                                                ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (build-uptime-field msg-count)
    (string-append "X-Uptime: "
		   (how-long-ago (- (current-seconds) invoked-time))
		   ", using "
		   (how-much-memory)
		   " bytes"
                   (if (number? msg-count) (format " (s: ~A)" msg-count) "")))

  (define invoked-time (current-seconds))
  
  (define (how-long-ago diff)
    (let-values ([(seconds minutes hours days) (apply values (how-long-ago-list diff))])
		(cond
		 [days
		  (string-append
		   (build-ele days "day")
		   " and "
		   (build-ele hours "hour"))]
		 [hours
		  (string-append
		   (build-ele hours "hour")
		   " and "
		   (build-ele minutes "minute"))]
		 [minutes
		  (string-append
		   (build-ele minutes "minute")
		   " and "
		   (build-ele seconds "second"))]
		 [else
		  (build-ele seconds "second")])))
  
  (define (build-ele count name)
    (cond
     [(or (not count) (zero? count))
      (format "0 ~as" name)]
     [(= count 1)
      (format "1 ~a" name)]
     [else
      (format "~a ~as" count name)]))
  
  (define (how-long-ago-list diff)
    (let loop ([divs '(60 60 24)]
               [diff diff])
      (cond
       [(null? divs) 
	(if (zero? diff)
	    (list #f)
	    (list diff))]
       [else (let ([div (car divs)])
	       (if (<= diff 0)
		   (cons #f (loop (cdr divs) 0))
		   (cons (modulo diff div)
			 (loop (cdr divs)
			       (quotient diff div)))))])))
  
  (define (how-much-memory)
    (let loop ([n (current-memory-use)])
      (cond
       [(< n 1000) (format "~a" n)]
       [else (format "~a,~a"
		     (loop (quotient n 1000))
		     (pad-3 (modulo n 1000)))])))

  (define (pad-3 n)
    (cond
     [(< n 10) (format "00~a" n)]
     [(< n 100) (format "0~a" n)]
     [else (format "~a" n)])))
