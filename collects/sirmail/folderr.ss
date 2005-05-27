
(module folderr mzscheme
  (require (lib "unitsig.ss")
           (lib "class.ss")
           (lib "framework.ss" "framework")
           (lib "mred-sig.ss" "mred"))
  
  (require (lib "list.ss")
           (lib "etc.ss"))
  
  (require "sirmails.ss"
           "pref.ss")
  
  (require (lib "imap-sig.ss" "net"))
  
  (require (lib "hierlist-sig.ss" "hierlist"))

  (require (lib "openssl.ss" "openssl"))
  
  (provide folder@)
  (define folder@
    (unit/sig ()
      (import sirmail:environment^
              (shutdown-folders-window)
              sirmail:options^
              mred^
              net:imap^
              hierlist^)
  
      (define (show-error x frame)
	(message-box "Error" 
		     (if (exn? x)
			 (exn-message x)
			 (format "Strange exception: ~s" x))
		     frame
		     '(ok stop)))
        
      (define mailbox-cache-file (build-path (LOCAL-DIR) "folder-window-mailboxes"))
      
      (define (imap-open-connection)
        (let ([passwd
               (or (get-PASSWORD)
                   (let ([p (get-text-from-user "Password" 
                                                (format "Password for ~a:" (USERNAME))
                                                frame
                                                ""
                                                '(password))])
                     (unless p (error 'connect "connection cancelled"))
                     p))])
          (let-values ([(server port-no)
                        (parse-server-name (IMAP-SERVER) (if (get-pref 'sirmail:use-ssl?) 993 143))])
            (begin0
	      (if (get-pref 'sirmail:use-ssl?)
		  (let-values ([(in out) (ssl-connect server port-no)])
		    (imap-connect* in out (USERNAME) passwd mailbox-name))
		  (parameterize ([imap-port-number port-no])
		    (imap-connect server (USERNAME) 
		                  passwd
                                  mailbox-name)))
              (unless (get-PASSWORD)
                (set-PASSWORD passwd))))))

      (define imap-mailbox-name-mixin
        (lambda (list%)
          (class list%
            (field
             [full-mailbox-name 'unknown-full-mailbox-name]
             [is-selectable? #f])
            [define/public set-full-mailbox-name
              (lambda (fm)
                (set! full-mailbox-name fm))]
            [define/public get-full-mailbox-name
             (lambda ()
               full-mailbox-name)]
            [define/public set-selectable
              (lambda (x) (set! is-selectable? x))]
            [define/public selectable?
              (lambda () is-selectable?)]
            (super-instantiate ()))))
      
      (define imap-mailbox-list-mixin
        (lambda (list%)
          (class list%
            (field
             [mailbox-name 'unknown-mailbox-name])
            [define/public get-mailbox-name
              (lambda ()
                mailbox-name)]
            [define/public set-mailbox-name
              (lambda (m)
                (set! mailbox-name m))]
            (super-instantiate ()))))
      
      ;; mailbox-folder = (make-deep-folder (union #f bytes)
      ;;                                    (union #f string)
      ;;                                    bool
      ;;                                    nested-mailbox-folder)
      ;; nested-mailbox-folder = 
      ;; (union (make-flat-folder bytes (union #f string) bool)
      ;;        (make-deep-folder bytes (union #f string) bool (listof mailbox-folder)))
      (define-struct folder (name short-name selectable?))
      (define-struct (deep-folder folder) (children))
      (define-struct (flat-folder folder) ())

      ;; refresh-mailboxes : -> void
      (define (refresh-mailboxes)
        (let ([mailboxes (fetch-mailboxes)])
          (when mailboxes
            (write-mailbox-folder mailboxes)
            (update-gui mailboxes))))

      ;; write-mailbox-folder : mailbox-folder -> void
      (define (write-mailbox-folder mbf)
        (let ([raw-datum
               (let loop ([mbf mbf])
                 (cond
                   [(flat-folder? mbf) (list (folder-name mbf)
                                             (folder-short-name mbf))]
                   [(deep-folder? mbf)
                    (list (folder-name mbf)
                          (folder-short-name mbf)
                          (folder-selectable? mbf)
                          (map loop (deep-folder-children mbf)))]
                   [else (error 'write-mailbox-folder "unknown mailbox folder: ~e"
                                mbf)]))])
          (call-with-output-file mailbox-cache-file
            (lambda (port)
              (write raw-datum port))
            'truncate 'text)))
      
      ;; read-mailbox-folder : -> mailbox-folder
      (define (read-mailbox-folder)
        (let* ([root-box (ROOT-MAILBOX-FOR-LIST)]
               [default
                (make-deep-folder (and root-box (string->bytes/utf-8 root-box))
                                  root-box
                                  #f ;; arbitrary
                                  null)])
          (if (file-exists? mailbox-cache-file)
              (let/ec k
                (let ([raw-datum (call-with-input-file mailbox-cache-file read 'text)])
                  (let loop ([rd raw-datum])
                    (cond
                      [(and (= 2 (length rd))
                            (or (not (car rd)) (bytes? (car rd)))
                            (or (not (car rd)) (string? (cadr rd))))
                       (make-flat-folder (car rd) (cadr rd) #t)]
                      [(and (= 3 (length rd))
                            (or (not (car rd)) (bytes? (car rd)))
                            (or (not (car rd)) (string? (cadr rd)))
                            (list? (caddr rd)))
                       (make-deep-folder (car rd)
                                         (cadr rd)
                                         #f
                                         (map loop (caddr rd)))]
                      [(and (= 4 (length rd))
                            (or (not (car rd)) (bytes? (car rd)))
                            (or (not (cadr rd)) (string? (cadr rd)))
                            (boolean? (caddr rd))
                            (list? (cadddr rd)))
                       (make-deep-folder (car rd)
                                         (cadr rd)
                                         (caddr rd)
                                         (map loop (cadddr rd)))]
                      [else (k default)]))))
              default)))

      
      ;; fetch-mailboxes : -> (union #f mailbox-folder)
      ;; gets the current mailbox list from the server
      (define (fetch-mailboxes)
        (with-custodian-killing-stop-button
         "Updating folder list..."
         (lambda ()
           (let-values ([(imap msg-count recent-count) (imap-open-connection)]
                        [(root-box) (ROOT-MAILBOX-FOR-LIST)])
             (make-deep-folder
              (and root-box (string->bytes/utf-8 root-box))
              root-box
              #f ;; arbitrary
              (let loop ([mailbox-name (and root-box (string->bytes/utf-8 root-box))])
                (let ([mailbox-name-length (if mailbox-name
					       (bytes-length mailbox-name)
					       0)]
                      [get-child-mailbox-name (lambda (item) (second item))]
                      [child-mailboxes (imap-list-child-mailboxes imap mailbox-name)])
                  (map (lambda (item)
                         (let* ([child-mailbox-name (get-child-mailbox-name item)]
                                [child-mailbox-flags (first item)]
                                [symbols (map imap-flag->symbol child-mailbox-flags)]
                                [flat-mailbox? (or (member 'noinferiors symbols)
						   (member 'hasnochildren symbols))]
                                [selectable? (not (member 'noselect symbols))]
                                [child-name-length (bytes-length child-mailbox-name)]
                                [strip-prefix?
                                 (and (> child-name-length mailbox-name-length)
				      mailbox-name
                                      (bytes=?
                                       (subbytes child-mailbox-name 0 mailbox-name-length)
                                       mailbox-name))]
                                [short-name
                                 (bytes->string/utf-8
                                  (if strip-prefix?
                                      (subbytes child-mailbox-name
                                                ;; strip separator (thus add1)
                                                (add1 mailbox-name-length)
                                                child-name-length)
                                      child-mailbox-name))])
                           (if flat-mailbox?
                               (make-flat-folder child-mailbox-name short-name #t)
                               (make-deep-folder 
                                child-mailbox-name
                                short-name
                                selectable?
                                (loop child-mailbox-name)))))
                       (quicksort
                        child-mailboxes
                        (lambda (x y)
                          (string<=? (bytes->string/utf-8 (get-child-mailbox-name x))
                                     (bytes->string/utf-8 (get-child-mailbox-name y)))))))))))))
         
      (define imap-mailbox-mixin
        (compose 
         imap-mailbox-list-mixin
         imap-mailbox-name-mixin))
      
      (define imap-top-list% 
        (class (imap-mailbox-list-mixin hierarchical-list%) 
          (field
           [selected-mailbox #f])
          [define/public get-selected-mailbox
            (lambda ()
              selected-mailbox)]
          (define/override on-select
            (lambda (i)
              (send frame set-status-text "")
              (set! selected-mailbox (and i 
                                          (send i selectable?)
                                          (send i get-full-mailbox-name)))
              (super on-select i)))
          (define/override on-double-select
            (lambda (i)
              (when (and i (send i selectable?))
                (let ([mail-box (send i get-full-mailbox-name)])
                  (send frame set-status-text (format "Opening ~a" mail-box))
                  (setup-mailboxes-file mail-box)
                  (open-mailbox (bytes->string/utf-8 mail-box))))
              (super on-double-select i)))
          (super-instantiate ())))
      
      (define (update-gui orig-mbf)
        (define (add-child hl mbf)
          (let* ([deep? (deep-folder? mbf)]
                 [new-item (if deep?
                               (send hl new-list imap-mailbox-mixin)
                               (send hl new-item imap-mailbox-name-mixin))]
                 [text (send new-item get-editor)])
            (send new-item set-full-mailbox-name (or (folder-name mbf) #""))
            (send new-item set-selectable (folder-selectable? mbf))
            (when deep?
              (send new-item set-mailbox-name (or (folder-name mbf) #"")))
            (send text insert (or (folder-short-name mbf) ""))
            new-item))
        (send (send top-list get-editor) begin-edit-sequence)
        (for-each (lambda (x) (send top-list delete-item x))
                  (send top-list get-items))
	(for-each (lambda (mbf)
		    (let loop ([hl top-list]
			       [mbf mbf])
		      (let ([new-item (add-child hl mbf)])
			(when (deep-folder? mbf)
			  (for-each (lambda (child) (loop new-item child))
				    (deep-folder-children mbf))))))
		  (cons (make-flat-folder (string->bytes/utf-8 mailbox-name) mailbox-name #t)
			(deep-folder-children orig-mbf)))
        (send (send top-list get-editor) end-edit-sequence))
      
      (define folders-frame%
        (class frame:basic%
          (define/override (on-size w h)
            (put-pref 'sirmail:folder-window-w w)
            (put-pref 'sirmail:folder-window-h h))
          (define/override (on-move x y)
            (put-pref 'sirmail:folder-window-x x)
            (put-pref 'sirmail:folder-window-y y))
          (define/augment (on-close)
            (inner (void) on-close)
            (shutdown-folders-window))
          (define/override (on-message msg)
            (let ([s (and (list? msg)
                          (number? (car msg))
                          (number? (cadr msg))
                          (let ([gx (car msg)]
                                [gy (cadr msg)])
                            (let-values ([(x y) (send top-list screen->client gx gy)])
                              (let ([lxb (box 0)]
                                    [lyb (box 0)])
                                (let loop ([ed (send top-list get-editor)])
                                  (set-box! lxb x)
                                  (set-box! lyb y)
                                  (send ed global-to-local lxb lyb)
                                  (let* ([on-it-b (box #f)]
                                         [pos (send ed find-position (unbox lxb) (unbox lyb) #f on-it-b)])
                                    (and (unbox on-it-b)
                                         (let ([snip (send ed find-snip pos 'after-or-none)])
                                           (cond
                                             [(is-a? snip hierarchical-item-snip%)
                                              (let ([item (send snip get-item)])
                                                (send item get-full-mailbox-name))]
                                             [(is-a? snip hierarchical-list-snip%)
                                              (let ([ed (send snip get-content-buffer)])
                                                (or (loop ed)
                                                    (let ([i (send snip get-item)])
                                                      (and (send i selectable?)
                                                           (send i get-full-mailbox-name)))))]
                                             [else #f])))))))))])
              (send frame set-status-text (if s 
                                              (format "Dragging to ~a" s)
                                              ""))
              s))
          (define/public (get-mailbox-name)
            (send top-list get-selected-mailbox))
          (super-instantiate ())))
      
      (define icon (make-object bitmap% (build-path (collection-path "sirmail")
                                                    "folder.bmp")))
      (define icon-mask (make-object bitmap% (build-path (collection-path "sirmail")
                                                         "folder-mask.xbm")))
      (define frame (make-object folders-frame% "Folders" #f
                      (get-pref 'sirmail:folder-window-w)
                      (get-pref 'sirmail:folder-window-h)
                      (max 0 (get-pref 'sirmail:folder-window-x))
                      (max 0 (get-pref 'sirmail:folder-window-y))))
      (define top-panel (instantiate horizontal-panel% ((send frame get-area-container))
                          [alignment '(right center)]
                          [stretchable-height #f]))
      
      (define re:setup-mailboxes (regexp "^([^/]*)/(.*)$"))
      (define (setup-mailboxes-file bytes-mailbox-name)
        (define mailbox-name (bytes->string/utf-8 bytes-mailbox-name))
        (define mailboxes-file (build-path (LOCAL-DIR) "mailboxes"))
        (define mailboxes
          (with-handlers ([exn:fail? (lambda (x) '(("Inbox" #"inbox")))])
            (with-input-from-file mailboxes-file
              read)))
        
        (define mailbox-loc (assoc mailbox-name mailboxes))
        
        (unless mailbox-loc
          
          (let ([fns (let loop ([str mailbox-name])
                       (cond
                         [(regexp-match re:setup-mailboxes str)
                          =>
                          (lambda (m)
                            (cons (cadr m)
                                  (loop (caddr m))))]
                         [else
                          (if (string=? str "")
                              null
                              (list str))]))])
            
            (unless (null? fns)
              (let ([mailbox-dir
                     (let loop ([fns (if (string=? (car fns) "")
                                         (cdr fns)
                                         fns)]
                                [local-dir 'same]
                                [fs-dir (LOCAL-DIR)])
                       (cond
                         [(null? fns) local-dir]
                         [else (let ([new-fs-dir (build-path fs-dir (car fns))])
                                 (unless (directory-exists? new-fs-dir)
                                   (make-directory new-fs-dir))
                                 (loop (cdr fns)
                                       (build-path local-dir (car fns))
                                       new-fs-dir))]))])
                
                (with-output-to-file (build-path (LOCAL-DIR) "mailboxes")
                  (lambda () (write
                              (append mailboxes
                                      (list (list mailbox-name
						  (path->bytes mailbox-dir))))))
                  'truncate))))))
      
      
      (define refresh-mailbox-button
        (instantiate button% ()
          (label "Update Folder List")
          (parent top-panel)
          (callback (lambda (x y)
                      (refresh-mailboxes)))))
      
      (define stop-thread #f)
      (define stop-button
        (instantiate button% ()
          (label "Stop")
          (parent top-panel)
          (callback (lambda (x y)
                      (when stop-thread
                        (break-thread stop-thread))))))
      
      (send stop-button enable #f)
      
      (define (with-custodian-killing-stop-button what thunk)
        (let ([c (make-custodian)]
              [result #f])
          (dynamic-wind
           (lambda ()
             (send frame set-status-text what)
             (send (send frame get-menu-bar) enable #f)
             (send top-list enable #f)
             (send refresh-mailbox-button enable #f)
             (send stop-button enable #t))
           (lambda ()
             (parameterize ([current-custodian c])
               (set! stop-thread (thread (lambda ()
                                           (with-handlers ([values (lambda (x)
                                                                     (set! result x))])
                                             (set! result (thunk))))))
               (yield stop-thread)))
           (lambda ()
             (send frame set-status-text "")
             (custodian-shutdown-all c)
             (send (send frame get-menu-bar) enable #t)
             (send top-list enable #t)
             (send refresh-mailbox-button enable #t)
             (send stop-button enable #f)))
          (if (exn? result)
              (raise result)
              result)))
             
      
      (define top-list (make-object imap-top-list% (send frame get-area-container)))
      
      (when (and (send icon ok?) (send icon-mask ok?))
        (send frame set-icon icon icon-mask 'both))
      
      (define file-menu (make-object menu% "&File" (send frame get-menu-bar)))
      (make-object menu-item% "&Add Folder..." file-menu
        (lambda (i e)
          (let ([t (get-text-from-user "New Folder" "New folder name:" frame)])
            (when t
              (when (with-custodian-killing-stop-button
                     (format "Creating ~a" t)
                     (lambda ()
                       (let-values ([(imap x y) (imap-open-connection)])
                         (imap-create-mailbox imap t))
                       #t))
                (refresh-mailboxes))))))
      (make-object separator-menu-item% file-menu)
      (make-object menu-item% "Close" file-menu
		   (lambda (i e)
		     (send frame close)))
      
      (frame:reorder-menus frame)
      
      (send frame show #t)
      (send frame min-width 350)
      (send frame min-height 450)
      (send frame create-status-line)
      (send top-list set-mailbox-name (ROOT-MAILBOX-FOR-LIST))
      (update-gui (read-mailbox-folder))

      (initial-exception-handler
         (lambda (x)
           (show-error x frame)
           ((error-escape-handler))))
      (current-exception-handler
       (initial-exception-handler))
      
      frame)))
