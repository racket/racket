#lang racket/base

(require racket/class racket/unit racket/file racket/gui/base net/sendurl
         mrlib/switchable-button mrlib/bitmap-label drracket/tool framework
         "info.rkt" "client.rkt" "this-collection.rkt")

(provide tool@)

(define uninstalled? #f)

(define server:port (#%info-lookup 'server:port (lambda () #f)))
(define-values (server port-no)
  (if server:port
    (let ([m (regexp-match #rx"^([^:]+):([0-9]+)$" server:port)])
      (unless m
        (error 'handin-client
               "Bad configuration ~s, expecting \"server:port\""
               server:port))
      (values (cadr m) (string->number (caddr m))))
    (values #f #f)))

(define handin-name   (#%info-lookup 'name))
(define web-menu-name (#%info-lookup 'web-menu-name (lambda () #f)))
(define web-address   (#%info-lookup 'web-address   (lambda () #f)))

(define password-keep-minutes
  (#%info-lookup 'password-keep-minutes (lambda () #f)))

(define handin-dialog-name (string-append handin-name " Handin"))
(define button-label/h     (string-append handin-name " Handin"))
(define button-label/r     (string-append handin-name " Retrieve"))
(define manage-dialog-name (string-append handin-name " Handin Account"))

(define updater?
  (#%info-lookup 'enable-auto-update (lambda () #f)))
(define multifile?
  (#%info-lookup 'enable-multifile-handin (lambda () #f)))

(define preference-key (make-my-key 'submit:username))

(preferences:set-default preference-key "" string?)
(define (remembered-user)
  (preferences:get preference-key))
(define (remember-user user)
  (preferences:set preference-key user))

(define remembered-assignment (make-parameter #f))

(define (connect) (handin-connect server port-no))

;; parameter-like procedure that keeps the password cached for a few minutes
(define cached-password
  (let ([passwd #f]
        [timer #f])
    (define protect
      (let ([s (make-semaphore 1)])
        (lambda (thunk)
          (dynamic-wind (lambda () (semaphore-wait s))
                        thunk
                        (lambda () (semaphore-post s))))))
    (case-lambda
      [() passwd]
      [(new)
       (protect (lambda ()
                  (when (and password-keep-minutes
                             (not (equal? 0 password-keep-minutes))
                             (not (equal? passwd new)))
                    (when timer (kill-thread timer))
                    (set! passwd new)
                    (set! timer (thread
                                 (lambda ()
                                   (sleep (* 60 password-keep-minutes))
                                   (protect (lambda ()
                                              (set! passwd #f)
                                              (set! timer #f)))))))))])))
;; a password entry box that uses the one cached above
(define cached-passwd%
  (class text-field%
    (define cached (cached-password))
    ;; use this instead of a cached password -- to avoid copy/pastes
    ;; of a password, and to hide its length
    (define fake-value "CACHED PASSWD")
    (define/override (get-value)
      (or cached (super get-value)))
    (define/override (on-focus on?)
      (if on?
        ;; got focus -- clear out bogus contents, if any
        (when cached (send this set-value "") (set! cached #f))
        ;; lost focus -- remember a new password, or restore it
        (let ([p (super get-value)])
          (cond [(and p (not (string=? "" p)))
                 ;; don't behave as if we have a cache: don't clear
                 ;; the value now, or if we get the focus later
                 (set! cached #f)
                 (cached-password p)]
                [(cached-password)
                 => (lambda (p)
                      (set! cached p)
                      (send this set-value fake-value))]))))
    (super-new [init-value (if cached fake-value "")]
               [style '(single password)])))

(provide handin-frame%)
(define handin-frame%
  (class dialog%
    (inherit show is-shown? center)
    (super-new [label handin-dialog-name])

    (init-field content on-retrieve)
    (define mode
      (cond [(and content on-retrieve) #f]
            [content     'submit]
            [on-retrieve 'retrieve]
            [else (error 'handin-frame "bad initial values")]))

    (define status
      (new message%
           [label (format "Making secure connection to ~a..." server)]
           [parent this]
           [stretchable-width #t]))
    (define username
      (new text-field%
           [label "Username:"]
           [init-value (remembered-user)]
           [parent this]
           [callback (lambda (t e) (activate-ok))]
           [stretchable-width #t]))
    (define passwd
      (new cached-passwd%
           [label "Password:"]
           [parent this]
           [callback (lambda (t e) (activate-ok))]
           [stretchable-width #t]))
    (define assignment
      (new choice%
           [label "Submit to:"]
           [choices null]
           [parent this]
           [callback (lambda (c e)
                       (remembered-assignment
                        (send assignment get-string
                              (send assignment get-selection))))]
           [stretchable-width #t]))

    (define button-panel
      (new horizontal-pane%
           [parent this]
           [stretchable-height #f]))

    (make-object vertical-pane% button-panel) ; spacer

    (define retrieve?
      (new check-box%
           [label "Retrieve"]
           [parent button-panel]
           [callback (lambda _
                       (define r? (send retrieve? get-value))
                       (send ok set-label
                             (if r? button-label/r button-label/h)))]
           [value (eq? 'retrieve mode)]
           [style (if mode '(deleted) '())]))

    (define (submit-file)
      (define final-message "Handin successful.")
      (submit-assignment
       connection
       (send username get-value)
       (send passwd get-value)
       (send assignment get-string (send assignment get-selection))
       content
       ;; on-commit
       (lambda ()
         (semaphore-wait commit-lock)
         (send status set-label "Committing...")
         (set! committing? #t)
         (semaphore-post commit-lock))
       ;; message/message-final/message-box handlers
       (lambda (msg) (send status set-label msg))
       (lambda (msg) (set! final-message msg))
       (lambda (msg styles) (message-box "Handin" msg this styles)))
      (queue-callback
       (lambda ()
         (when abort-commit-dialog (send abort-commit-dialog show #f))
         (send status set-label final-message)
         (set! committing? #f)
         (done-interface))))
    (define (retrieve-file)
      (let ([buf (retrieve-assignment
                  connection
                  (send username get-value)
                  (send passwd get-value)
                  (send assignment get-string (send assignment get-selection)))])
        (queue-callback
         (lambda ()
           (done-interface)
           (do-cancel-button)
           (on-retrieve buf)))))

    (define ok
      (new button%
           [label (case mode
                    [(submit)   button-label/h]
                    [(retrieve) button-label/r]
                    [else (string-append "  " button-label/h "  ")])] ; can change
           [parent button-panel]
           [style '(border)]
           [callback
            (lambda (b e)
              (disable-interface)
              (send status set-label "Handing in...")
              (parameterize ([current-custodian comm-cust])
                (thread
                 (lambda ()
                   (remember-user (send username get-value))
                   (with-handlers ([void (lambda (exn)
                                           (report-error "Handin failed." exn))])
                     (if (send retrieve? get-value)
                       (retrieve-file)
                       (submit-file)))))))]))

    (define ok-can-enable? #f)
    (define (activate-ok)
      (send ok enable (and ok-can-enable?
                           (not (string=? "" (send username get-value)))
                           (not (string=? "" (send passwd get-value))))))

    (define cancel
      (new button%
           [label "Cancel"]
           [parent button-panel]
           [callback (lambda (b e) (do-cancel-button))]))
    (define (do-cancel-button)
      (let ([go? (begin
                   (semaphore-wait commit-lock)
                   (if committing?
                     (begin
                       (semaphore-post commit-lock)
                       (send abort-commit-dialog show #t)
                       continue-abort?)
                     #t))])
        (when go?
          (custodian-shutdown-all comm-cust)
          (show #f))))

    (define continue-abort? #f)
    (define abort-commit-dialog
      (let ([d (make-object dialog% "Commit in Progress")])
        (make-object message% "The commit action is in progress." d)
        (make-object message% "Cancelling now may or may not work." d)
        (make-object message% "Cancel anyway?" d)
        (let ([b (new horizontal-panel%
                      [parent d]
                      [stretchable-height #f]
                      [alignment '(center center)])])
          (make-object button% "Continue Commit" d
                       (lambda (b e) (send d show #f)))
          (make-object button% "Try to Cancel" d
                       (lambda (b e)
                         (set! continue-abort? #t) (send d show #f))))))

    (define interface-widgets
      (list ok username passwd assignment retrieve?))
    (define (disable-interface)
      (for ([x (in-list interface-widgets)]) (send x enable #f)))
    (define (enable-interface)
      (for ([x (in-list interface-widgets)]) (send x enable #t) ))
    (define (done-interface)
      (send cancel set-label "Close")
      (send cancel focus))

    (define (report-error tag exn)
      (queue-callback
       (lambda ()
         (let* ([msg (if (exn? exn)
                       (let ([s (exn-message exn)])
                         (if (string? s) s (format "~.s" s)))
                       (format "~.s" exn))]
                [retry? (regexp-match #rx"bad username or password for" msg)])
           (custodian-shutdown-all comm-cust)
           (set! committing? #f)
           (disable-interface)
           (send status set-label tag)
           (when (is-shown?)
             (message-box "Server Error" msg this)
             (if retry?
               (begin (init-comm) (semaphore-post go-sema) (enable-interface))
               (done-interface)))))))

    (define go-sema #f)
    (define commit-lock #f)
    (define committing? #f)

    (define connection #f)

    (define comm-cust #f)
    (define (init-comm)
      (set! go-sema (make-semaphore 1))
      (set! commit-lock (make-semaphore 1))
      (set! comm-cust (make-custodian))
      (parameterize ([current-custodian comm-cust])
        (thread
         (lambda ()
           (let/ec break
             (with-handlers ([void
                              (lambda (exn)
                                (report-error "Connection failed." exn)
                                (break))])
               (semaphore-wait go-sema)
               (let* ([h (connect)]
                      [l (retrieve-active-assignments h)]
                      [n (cond [(member (remembered-assignment) l)
                                => (lambda (r) (- (length l) (length r)))]
                               [else 0])])
                 (when (null? l)
                   (handin-disconnect h)
                   (error 'handin "there are no active assignments"))
                 (set! connection h)
                 (for ([assign (in-list l)]) (send assignment append assign))
                 (send assignment enable #t)
                 (send assignment set-selection n)
                 (set! ok-can-enable? #t)
                 (activate-ok)
                 (send status set-label
                       (format "Connected securely for ~a." handin-name)))))))))

    (define/augment (on-close)
      (inner (void) on-close)
      (do-cancel-button))

    (init-comm)

    (send (cond [(string=? "" (send username get-value)) username]
                [(string=? "" (send passwd get-value)) passwd]
                [else ok])
          focus)
    (send ok enable #f) ; disable after focus possibly sent to it
    (send assignment enable #f)

    (center)
    (show #t)))

(provide manage-handin-dialog%)
(define manage-handin-dialog%
  (class dialog% (init [parent #f])
    (inherit show is-shown? center)
    (super-new [label manage-dialog-name]
               [alignment '(left center)]
               [parent parent])
    (define user-fields (get-user-fields parent))

    ;; === utilities ===
    (define (mk-txt label parent activate-ok)
      (new text-field%
           [label label]
           [parent parent]
           [callback (lambda (t e) (activate-ok))]
           [stretchable-width #t]))
    (define (mk-passwd label parent activate-ok)
      (new text-field%
           [label label]
           [parent parent]
           [callback (lambda (t e) (activate-ok))]
           [style '(single password)]
           [stretchable-width #t]))
    (define (non-empty? . ts)
      (andmap (lambda (t) (not (string=? "" (send t get-value)))) ts))
    (define (same-value t1 t2)
      (string=? (send t1 get-value) (send t2 get-value)))
    (define (report-error tag exn)
      (queue-callback
       (lambda ()
         (custodian-shutdown-all comm-cust)
         (send status set-label tag)
         (when (is-shown?)
           (message-box
            "Server Error"
            (if (exn? exn)
              (let ([s (exn-message exn)]) (if (string? s) s (format "~.s" s)))
              (format "~.s" exn))
            this)
           (set! comm-cust (make-custodian))))))
    (define comm-cust (make-custodian))
    (define/augment (on-close)
      (inner (void) on-close)
      (custodian-shutdown-all comm-cust))
    ;; Too-long fields can't damage the server, but they might result in
    ;;  confusing errors due to safety cut-offs on the server side.
    (define (check-length field size name k)
      (when ((string-length (send field get-value)) . > . size)
        (message-box "Error"
                     (format "The ~a must be no longer than ~a characters."
                             name size))
        (k (void))))
    (define (do-change/add new? username)
      (let/ec break
        (check-length username 50 "Username" break)
        (let* ([pw1 (if new? new-passwd  add-passwd)]
               [pw2 (if new? new-passwd2 add-passwd2)]
               [l1 (regexp-replace #rx" *:$" (send pw1 get-label) "")]
               [l2 (regexp-replace #rx" *:$" (send pw2 get-label) "")])
          (check-length pw1 50 l1 break)
          ;; not really needed, but leave just in case
          (unless (string=? (send pw1 get-value) (send pw2 get-value))
            (message-box
             "Password Error"
             (format "The \"~a\" and \"~a\" passwords are not the same." l1 l2))
            (break (void))))
        (for ([t (in-list (if new? add-user-fields change-user-fields))]
              [f (in-list (or user-fields '()))])
          (check-length t 100 f break))
        (send tabs enable #f)
        (parameterize ([current-custodian comm-cust])
          (thread
           (lambda ()
             (with-handlers ([void (lambda (exn)
                                     (send tabs enable #t)
                                     (report-error
                                      (format "~a failed."
                                              (if new? "Creation" "Update"))
                                      exn))])
               (remember-user (send username get-value))
               (send status set-label "Making secure connection...")
               (let ([h (connect)])
                 (define (run proc . fields)
                   (apply proc h
                          (let loop ([x fields])
                            (if (list? x) (map loop x) (send x get-value)))))
                 (send status set-label
                       (if new? "Creating user..." "Updating server..."))
                 (if new?
                   (run submit-addition username add-passwd add-user-fields)
                   (run submit-info-change username old-passwd new-passwd
                        change-user-fields)))
               (send status set-label "Success.")
               (send cancel set-label "Close")))))))
    (define (do-retrieve username)
      (send tabs enable #f)
      (parameterize ([current-custodian comm-cust])
        (thread
         (lambda ()
           (with-handlers ([void (lambda (exn)
                                   (send tabs enable #t)
                                   (report-error "Retrieve failed." exn))])
             (remember-user (send username get-value))
             (send status set-label "Making secure connection...")
             (let ([h (connect)])
               (define (run proc . fields)
                 (apply proc h
                        (let loop ([x fields])
                          (if (list? x) (map loop x) (send x get-value)))))
               (send status set-label "Retrieving information...")
               (let ([vals (run retrieve-user-info username old-passwd)])
                 (send status set-label "Success, you can now edit fields.")
                 (send tabs enable #t)
                 (for ([f change-user-fields]
                       [val vals])
                   (send f set-value val))
                 (activate-change))))))))

    ;; === toplevel gadgets ===
    (define status
      (new message%
           [label (if user-fields
                    (format "Manage ~a handin account at ~a."
                            handin-name server)
                    "No connection to server!")]
           [parent this]
           [stretchable-width #t]))
    (define (set-active-tab n)
      (send new-user-box show #f)
      (send old-user-box show #f)
      (send un/install-box show #f)
      (send (if user-fields
              (case n
                [(0) new-user-box]
                [(1) old-user-box]
                [(2) un/install-box]
                [else (error "internal error")])
              un/install-box)
            show #t))
    (define tabs
      (let* ([names (list (if multifile? "Un/Install" "Uninstall"))]
             [names (if user-fields
                      `("New User" "Change Info" ,@names) names)]
             [callback (lambda _ (set-active-tab (send tabs get-selection)))])
        (new tab-panel% [parent this] [choices names] [callback callback])))
    (define single (new panel:single% [parent tabs]))
    (define button-panel
      (new horizontal-pane% [parent this] [stretchable-height #f]))
    (make-object vertical-pane% button-panel) ; spacer
    (define cancel
      (new button%
           [label "Cancel"] [parent button-panel]
           [callback (lambda (b e)
                       (custodian-shutdown-all comm-cust)
                       (show #f))]))

    ;; === change existing info tab ===
    (define (activate-change)
      (define an-extra-non-empty? (ormap non-empty? change-user-fields))
      (send retrieve-old-info-button enable
            (non-empty? old-username old-passwd))
      (send change-button enable
            (and (same-value new-passwd new-passwd2)
                 (non-empty? old-username old-passwd)
                 (or (non-empty? new-passwd) an-extra-non-empty?)))
      (send change-button set-label
            (if an-extra-non-empty? "Change Info" "Set Password")))
    (define old-user-box (new vertical-panel%
                              [parent single] [alignment '(center center)]))
    (define old-username (mk-txt "Username:" old-user-box activate-change))
    (send old-username set-value (remembered-user))
    (define old-passwd
      (new cached-passwd%
           [label "Old Password:"]
           [parent old-user-box]
           [callback (lambda (t e) (activate-change))]
           [stretchable-width #t]))
    (define change-user-fields
      (map (lambda (f)
             (mk-txt (string-append f ":") old-user-box activate-change))
           (or user-fields '())))
    (define new-passwd
      (mk-passwd "New Password:" old-user-box activate-change))
    (define new-passwd2
      (mk-passwd "New Password again:" old-user-box activate-change))
    (define-values (retrieve-old-info-button change-button)
      (let ([p (new horizontal-pane%
                    [parent old-user-box]
                    [stretchable-height #f]
                    [alignment '(center center)])])
        (make-object vertical-pane% p)
        (values
         (begin0 (new button%
                      [label "Get Current Info"] [parent p]
                      [callback (lambda (b e) (do-retrieve old-username))])
           (make-object vertical-pane% p))
         (begin0 (new button%
                      [label "Set Password"] [parent p] [style '(border)]
                      [callback (lambda (b e)
                                  (do-change/add #f old-username))])
           (make-object vertical-pane% p)))))

    ;; === register new user tab ===
    (define (activate-new)
      (send new-button enable
            (and (apply non-empty? new-username add-passwd add-passwd2
                        add-user-fields)
                 (same-value add-passwd add-passwd2))))
    (define new-user-box (new vertical-panel%
                              [parent single] [alignment '(center center)]))
    (define new-username (mk-txt "Username:" new-user-box activate-new))
    (send new-username set-value (remembered-user))
    (define add-user-fields
      (map (lambda (f)
             (mk-txt (string-append f ":") new-user-box activate-new))
           (or user-fields '())))
    (define add-passwd
      (mk-passwd "Password:" new-user-box activate-new))
    (define add-passwd2
      (mk-passwd "Password again:" new-user-box activate-new))
    (define new-button (new button%
                            [label "Add User"] [parent new-user-box]
                            [callback (lambda (b e)
                                        (do-change/add #t new-username))]
                            [style '(border)]))

    ;; === uninstall client, install standalone client ===
    (define un/install-box
      (new vertical-panel% [parent single] [alignment '(center center)]))
    (define uninstall-button
      (new button%
           [label (format "Uninstall ~a Handin" handin-name)]
           [parent un/install-box]
           [callback
            (lambda (b e)
              (with-handlers ([void (lambda (exn)
                                      (report-error "Uninstall failed." exn))])
                (delete-directory/files (in-this-collection))
                (set! uninstalled? #t)
                (send uninstall-button enable #f)
                (message-box
                 "Uninstall"
                 (format "The ~a tool has been uninstalled. ~a~a"
                         handin-name
                         "The Handin button and associated menu items will"
                         " not appear after you restart DrRacket.")
                 this)
                (send this show #f)))]))
    (send uninstall-button enable (not uninstalled?))
    (define install-standalone-button
      (and multifile?
           (new button%
                [label (format "Install Standalone ~a Handin" handin-name)]
                [parent un/install-box]
                [callback
                 (lambda (b e)
                   (define (launcher sym)
                     (dynamic-require `launcher sym))
                   (let* ([exe (let-values
                                   ([(dir name dir?)
                                     (split-path
                                      ((launcher 'mred-program-launcher-path)
                                       (format "~a Handin" handin-name)))])
                                 (path->string name))]
                          [dir (get-directory
                                (format "Choose a directory to create the ~s~a"
                                        exe " executable in")
                                #f)])
                     (when (and dir (directory-exists? dir))
                       (parameterize ([current-directory dir])
                         (when (or (not (file-exists? exe))
                                   (eq? 'ok
                                        (message-box
                                         "File Exists"
                                         (format
                                          "The ~s executable already exists, ~a"
                                          exe "it will be overwritten")
                                         this '(ok-cancel caution))))
                           ((launcher 'make-mred-launcher)
                            (list "-le-" (format "~a/handin-multi"
                                                 this-collection-name)
                                  "(multifile-handin)")
                            (build-path dir exe))
                           (message-box "Standalone Executable"
                                        (format "~s created" exe)
                                        this)
                           (send this show #f))))))])))

    ;; === initialize the whole thing ===
    (activate-new)
    (activate-change)
    (center)
    (queue-callback
     (lambda ()
       (define n (cond [(not user-fields) 0]
                       [(equal? "" (remembered-user)) 0]
                       [else 1]))
       (set-active-tab n)
       (send tabs set-selection n)))
    (show #t)))

;; A simple dialog during connection, with an option to cancel (used
;; by `get-user-fields' below, since its value is needed to
;; construct the above dialog).
(define connection-dialog%
  (class dialog% (init receiver [parent #f])
    (inherit show is-shown? center)
    (super-new [label manage-dialog-name]
               [alignment '(right center)]
               [parent parent])
    (define status
      (new message% [label "Connecting to server..."]
                    [parent this]
                    [stretchable-width #t]))
    (define comm-cust (make-custodian))
    (define/augment (on-close)
      (inner (void) on-close)
      (custodian-shutdown-all comm-cust))
    (define button
      (new button% [label "Cancel"] [parent this]
                   [callback (lambda (b e)
                               (custodian-shutdown-all comm-cust)
                               (show #f))]
                   [style '(border)]))
    (send button focus)
    (parameterize ([current-custodian comm-cust])
      (thread
       (lambda ()
         (unless (with-handlers ([void (lambda (_) #f)])
                   (receiver (connect)) #t)
           (begin (send status set-label "Connection failure!")
                  ;; (send button enable #f)
                  (sleep 5)))
         (queue-callback (lambda () (show #f))))))
    (center)
    (show #t)))

(define cached-user-fields #f)
(define (get-user-fields parent)
  (unless cached-user-fields
    (new connection-dialog%
         [receiver (lambda (h)
                     (set! cached-user-fields (retrieve-user-fields h)))]
         [parent parent]))
  cached-user-fields)

(define (scale-by-half file)
  (let* ([bm (make-object bitmap% file 'unknown/mask)]
         [w (send bm get-width)]
         [h (send bm get-height)]
         [bm2 (make-object bitmap% (quotient w 2) (quotient h 2))]
         [mbm2 (and (send bm get-loaded-mask)
                    (make-object bitmap% (quotient w 2) (quotient h 2)))]
         [mdc (make-object bitmap-dc% bm2)])
    (send mdc draw-bitmap-section-smooth bm 
          0 0 (quotient w 2) (quotient h 2)
          0 0 w h)
    (send mdc set-bitmap #f)
    (when mbm2
      (send mdc set-bitmap mbm2)
      (send mdc draw-bitmap-section-smooth (send bm get-loaded-mask)
            0 0 (quotient w 2) (quotient h 2)
            0 0 w h)
      (send mdc set-bitmap #f)
      (send bm2 set-loaded-mask mbm2))
    bm2))

(define handin-icon (scale-by-half (in-this-collection "icon.png")))

(define (editors->string editors)
  (let* ([base (make-object editor-stream-out-bytes-base%)]
         [stream (make-object editor-stream-out% base)])
    (write-editor-version stream base)
    (write-editor-global-header stream)
    (for ([ed (in-list editors)]) (send ed write-to-file stream))
    (write-editor-global-footer stream)
    (send base get-bytes)))

(define (string->editor! str defs)
  (let* ([base (make-object editor-stream-in-bytes-base% str)]
         [stream (make-object editor-stream-in% base)])
    (read-editor-version stream base #t)
    (read-editor-global-header stream)
    (send* defs (begin-edit-sequence #f)
                (erase) (read-from-file stream)
                (end-edit-sequence))
    (read-editor-global-footer stream)))

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define phase1 void)
    (define phase2
      (if updater?
        (dynamic-require `(lib "updater.rkt" ,this-collection-name) 'bg-update)
        void))

    (define tool-button-label (bitmap-label-maker button-label/h handin-icon))

    (define (make-new-unit-frame% super%)
      (class super%
        (inherit get-button-panel
                 get-definitions-text
                 get-interactions-text)
        (super-instantiate ())

        (define/override (file-menu:between-open-and-revert file-menu)
          ;; super adds a separator, add this and another sep after that
          (super file-menu:between-open-and-revert file-menu)
          (new menu-item%
               [label (format "Manage ~a Handin Account..." handin-name)]
               [parent file-menu]
               [callback (lambda (m e)
                           (new manage-handin-dialog% [parent this]))])
          (when multifile?
            (new menu-item%
                 [label (format "Submit multiple ~a Files..." handin-name)]
                 [parent file-menu]
                 [callback (lambda (m e)
                             ((dynamic-require
                               `(lib "handin-multi.rkt" ,this-collection-name)
                               'multifile-handin)))]))
          (when updater?
            (new menu-item%
                 [label (format "Update ~a plugin..." handin-name)]
                 [parent file-menu]
                 [callback
                  (lambda (m e)
                    ((dynamic-require `(lib "updater.rkt" ,this-collection-name)
                                      'update)
                     #f #t))])) ; no parent
          (new separator-menu-item% [parent file-menu]))

        (define/override (help-menu:after-about menu)
          (when web-menu-name
            (new menu-item%
                 [label web-menu-name]
                 [parent menu]
                 [callback (lambda (item evt) (send-url web-address))]))
          (super help-menu:after-about menu))

        (define client-panel
          (new panel:vertical-discrete-sizes% (parent (get-button-panel))))

        (define client-button
          (new switchable-button%
               [label button-label/h]
               [bitmap handin-icon]
               [parent client-panel]
               [callback
                (lambda (button)
                  (let ([content (editors->string
                                  (list (get-definitions-text)
                                        (get-interactions-text)))])
                    (new handin-frame%
                         [parent this]
                         [content content]
                         [on-retrieve
                          (lambda (buf)
                            (string->editor!
                             buf
                             (send (drracket:unit:open-drscheme-window)
                                   get-editor)))])))]))

        (inherit register-toolbar-button)
        (register-toolbar-button client-button #:number -1000)

        (send (get-button-panel) change-children
              (lambda (l) (cons client-panel (remq client-panel l))))))

    (when (and server port-no)
      (drracket:get/extend:extend-unit-frame make-new-unit-frame% #f))))
