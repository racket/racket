
(module bug-report mzscheme
  (require (lib "string-constant.ss" "string-constants")
           (lib "head.ss" "net")
           (lib "smtp.ss" "net")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "class.ss")
           (lib "etc.ss")
           (lib "url.ss" "net")
           (lib "uri-codec.ss" "net")
           (lib "htmltext.ss" "browser")
           "private/buginfo.ss"
           "private/manuals.ss")
  
  (provide help-desk:report-bug)
  
  (define bug-report-recipient "bugs")
  (define bug-email-server "bugs.plt-scheme.org")
  (define bug-email-server-port 1025)
  (define bug-www-server "bugs.plt-scheme.org")
  (define bug-www-server-port 80)
  (define bug-report-email-address 
    (string-append bug-report-recipient "@plt-scheme.org"))
  
  ;; this one should be defined by help desk.
  (define frame-mixin
    (namespace-variable-value 'help-desk:frame-mixin #f (lambda () (lambda (x) x))))
  
  (preferences:set-default 'drscheme:email "" string?)
  (preferences:set-default 'drscheme:full-name "" string?)

  (define (remove-extra-blanks %)
    (class %
      (define/override (edit-menu:between-find-and-preferences menu) (void))
      (super-instantiate ())))

  (define bug-frame%
    (class (frame-mixin (remove-extra-blanks (frame:standard-menus-mixin frame:basic%)))
      (init title)

      (define/override (file-menu:between-print-and-close menu) (void))
      
      (field (ok-to-close? #f))
      (public set-ok-to-close)
      (define (set-ok-to-close ok?) (set! ok-to-close? #t))
      (define/augment (can-close?)
        (or ok-to-close?
            (ask-yes-or-no (string-constant cancel-bug-report?)
                           (string-constant are-you-sure-cancel-bug-report?)
                           this)))
      
      (super-make-object title)))
  
  
  (define (help-desk:report-bug)
    (define bug-frame (instantiate bug-frame% () (title (string-constant bug-report-form))))
    (define single (new panel:single% (parent (send bug-frame get-area-container))))
    (define outermost-panel (make-object vertical-panel% single))
    
    (define response-panel (new vertical-panel% (parent single)))
    (define response-text (new (html-text-mixin text%) (auto-wrap #t)))
    (define response-ec (new editor-canvas% (parent response-panel) (editor response-text)))
    (define response-button-panel (new horizontal-panel%
                                       (stretchable-height #f)
                                       (parent response-panel)
                                       (alignment '(right center))))
    (define cancel-kill-thread #f)
    (define response-reset (new button%
                                (parent response-button-panel)
                                (enabled #f)
                                (label (string-constant wizard-back))
                                (callback
                                 (lambda (x y)
                                   (switch-to-compose-view)))))
    (define response-abort (new button%
                                 (parent response-button-panel)
                                 (enabled #f)
                                 (callback
                                  (lambda (x y)
                                    (kill-thread cancel-kill-thread)
                                    (switch-to-compose-view)))
                                 (label (string-constant abort))))
    (define response-close (new button%
                                (parent response-button-panel)
                                (enabled #f)
                                (callback (lambda (x y) (cleanup-frame)))
                                (label (string-constant close))))
    
    (define top-panel (make-object vertical-panel% outermost-panel))
    
    (define (switch-to-respose-view) (send single active-child response-panel))
    (define (switch-to-compose-view) 
      (send response-text erase)
      (send single active-child outermost-panel))
    
    (define lps null)
    
    ; build/label : ((union string (list-of string))
    ;                (area-container<%> -> item<%>) 
    ;                boolean
    ;                area-container<%>
    ;             -> item<%>)
    ; constructs and arranges the gui objects for the bug report form
    ; effect: updates lps with the new label panel, for future alignment
    (define build/label
      (opt-lambda (text make-item top? [stretch? #f] [top-panel top-panel] [vertical? #f])
        (let*-values ([(hp) (make-object (if vertical?
                                             vertical-panel%
                                             horizontal-panel%)
                              top-panel)]
                      [(lp) (make-object vertical-panel% hp)]
                      [(ip) (make-object vertical-panel% hp)]
                      [(label/s) (if (string? text)
                                     (make-object message% text lp)
                                     (map (lambda (s)
                                            (make-object message% s lp))
                                          text))]
                      [(item) (make-item ip)])
          (set! lps (cons lp lps))
          (unless stretch? 
            (send hp stretchable-height #f)
            (send lp stretchable-height #f)
            (send ip stretchable-height #f))
          (send lp stretchable-width #f)
          (send lp stretchable-height #f)
          (send lp set-alignment (if vertical? 'left 'right) (if top? 'top 'center))
          (send ip set-alignment 'left 'top)
          item)))
    
    (define (align-labels)
      (let ([width (apply max (map (lambda (x) (send (car (send x get-children)) min-width))
                                   lps))])
        (for-each (lambda (x) (send x min-width width)) lps))) 
    
    (define name
      (build/label 
       (string-constant bug-report-field-name)
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel
              (lambda (text event)
                (preferences:set 'drscheme:full-name (send text get-value)))
              (preferences:get 'drscheme:full-name)))))
       #f))
    
    (define email  
      (build/label
       (string-constant bug-report-field-email)
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel
              (lambda (text event)
                (preferences:set 'drscheme:email (send text get-value)))
              (preferences:get 'drscheme:email)))))
       #f))
    
    (define summary
      (build/label
       (string-constant bug-report-field-summary) 
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void))))
       #f))
    
    (define severity
      (build/label 
       (string-constant bug-report-field-severity) 
       (lambda (panel)
         (make-object choice% 
           #f
           (list "critical" "serious" "non-critical")
           panel
           void))
       #f))
    
    (define bug-classes '(("software bug" "sw-bug")
                          ("documentation bug" "doc-bug")
                          ("change request" "change-request")
                          ("support" "support")))
    
    (define bug-class
      (build/label
       (string-constant bug-report-field-class)
       (lambda (panel)
         (make-object choice%
           #f
           (map car bug-classes)
           panel
           void))
       #f))
    
    (define (translate-class class)
      (cadr (assoc class bug-classes)))
    
    (define (make-big-text label . args)
      (let ([canvas 
             (apply
              build/label 
              label 
              (lambda (panel)
                (let* ([text (new (editor:standard-style-list-mixin
				   (editor:keymap-mixin
				    text:basic%)))]
                       [canvas (make-object canvas:basic% panel text)])
                  (send text set-paste-text-only #t)
                  (send text set-styles-fixed #t)
                  canvas))
              #t
              args)])
        (send canvas min-width 500)
        (send canvas min-height 130)
        (send canvas get-editor)
        (send canvas allow-tab-exit #t)
        canvas))
    
    (define description (make-big-text (string-constant bug-report-field-description) #t))
    (define reproduce (make-big-text (list (string-constant bug-report-field-reproduce1)
                                           (string-constant bug-report-field-reproduce2))
                                     #t))
    
    (define synthesized-dialog (make-object dialog% (string-constant bug-report-synthesized-information)))
    (define synthesized-panel (make-object vertical-panel% synthesized-dialog))
    (define synthesized-button-panel (make-object horizontal-panel% synthesized-dialog))
    (define synthesized-ok-button (make-object button% (string-constant ok) synthesized-button-panel
					       (lambda (x y)
						 (send synthesized-dialog show #f))))
    (define synthesized-info-shown? #t)
    (define (show-synthesized-info)
      (send synthesized-dialog show #t))
    
    (define version
      (build/label
       (string-constant bug-report-field-version)
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel
       #f))
    (define environment
      (build/label
       (string-constant bug-report-field-environment)
       (lambda (panel)
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel
       #f))
    
    (define human-language
      (build/label 
       (string-constant bug-report-field-human-language)
       (lambda (panel)            
         (keymap:call/text-keymap-initializer
          (lambda ()
            (make-object text-field% #f panel void ""))))
       #f
       #f
       synthesized-panel))
    
    (define docs-installed
      (make-big-text
       (string-constant bug-report-field-docs-installed)
       #t
       synthesized-panel))

    (define collections
      (make-big-text 
       (string-constant bug-report-field-collections)
       #t
       synthesized-panel))
    
    (define extras
      (map (lambda (bri)
             (let ([label (bri-label bri)])
               (cons
                label
                (build/label
                 label
                 (lambda (panel)
                   (let ([field 
                          (keymap:call/text-keymap-initializer
                           (lambda ()
                             (make-object text-field% #f panel void "")))])
                     (send field set-value (bri-value bri))
                     field))
                 #f
                 #f
                 synthesized-panel))))
           (get-bug-report-infos)))

    (define button-panel (make-object horizontal-panel% outermost-panel))
    (define synthesized-button (make-object button%
                                 (string-constant bug-report-show-synthesized-info)
                                 button-panel (lambda x (show-synthesized-info))))
    (define ok-button (make-object button% (string-constant bug-report-submit) button-panel (lambda x (ok))))
    (define cancel-button (make-object button% (string-constant cancel) button-panel (lambda x (cancel))))
    (define grow-box-spacer-pane (make-object grow-box-spacer-pane% button-panel))
    
    (define (get-query)
      (list (cons 'help-desk "true")
            (cons 'replyto (preferences:get 'drscheme:email))
            (cons 'originator (preferences:get 'drscheme:full-name))
            (cons 'subject (send summary get-value))
            (cons 'severity (send severity get-string-selection))
            (cons 'class (translate-class (send bug-class get-string-selection)))
            (cons 'release (send version get-value))
            (cons 'description (apply string-append (map (lambda (x) (string-append x "\n")) 
                                                         (get-strings description))))
            (cons 'how-to-repeat (apply string-append (map (lambda (x) (string-append x "\n")) 
                                                           (get-strings reproduce))))
            (cons 'platform (get-environment))))
    
    (define (get-environment)
      (string-append (send environment get-value)
                     "\n"
                     "Docs Installed:\n" 
                     (format "~a" (send (send docs-installed get-editor) get-text))
                     "\n"
                     "Collections:\n"
                     (format "~a" (send (send collections get-editor) get-text))
                     "\n"
                     (format "Human Language: ~a\n" (send human-language get-value))
                     (apply 
                      string-append
                      (map (lambda (extra)
                             (format "~a: ~a\n"
                                     (car extra)
                                     (send (cdr extra) get-value)))
                           extras))))
        
    ;; smtp-send-bug-report : -> void
    (define (smtp-send-bug-report)
      (smtp-send-message
       bug-email-server
       (preferences:get 'drscheme:email)
       (list bug-report-recipient)
       (insert-field
        "X-Mailer"
        (format "Help Desk ~a (bug report form)" (version:version))
        (insert-field     
         "Subject" 
         (send summary get-value)
         (insert-field
          "To"
          bug-report-email-address
          (insert-field
           "From"
           (format "~a <~a>" 
                   (preferences:get 'drscheme:full-name)
                   (preferences:get 'drscheme:email))
           empty-header))))
       `(">Category:       all"
         ,(format ">Synopsis:       ~a" (send summary get-value))
         ">Confidential:   no"
         ,(format ">Severity:       ~a" (send severity get-string-selection))
         ,(format ">Priority:       medium")
         ,(format ">Class:          ~a" (translate-class (send bug-class get-string-selection)))
         ">Submitter-Id:   unknown"
         ,(format ">Originator:     ~a" (preferences:get 'drscheme:full-name))
         ">Organization:"
         "titan"
         ,(format ">Release:        ~a" (send version get-value))
         ">Environment:"
         ,(format "~a" (send environment get-value))
         "Docs Installed:" 
         ,(format "~a" (send (send docs-installed get-editor) get-text))
         "Collections: "
         ,(format "~a" (send (send collections get-editor) get-text))
         " "
         ,(format "Human Language: ~a" (send human-language get-value))
         " "
         ,@(map (lambda (extra)
                  (format "~a: ~a"
                          (car extra)
                          (send (cdr extra) get-value)))
                extras)
         ">Fix: "
         ">Description:"
         ,@(get-strings description)
         ">How-To-Repeat:"
         ,@(get-strings reproduce))
       bug-email-server-port))
    
    ; send-bug-report : (-> void)
    ;; initiates sending the bug report and switches the GUI's mode
    (define (send-bug-report)
      (letrec ([query (get-query)]
               [url (make-url "http"
                              #f
                              bug-www-server
                              bug-www-server-port
                              (list "cgi-bin" "bug-report")
                              '() ;query
                              #f)]
               [post-data 
                (parameterize ([current-alist-separator-mode 'amp])
                  (string->bytes/utf-8 (alist->form-urlencoded query)))]
               [http-thread
                (parameterize ([current-custodian (make-custodian)])
                  (thread
                   (lambda ()
                     (with-handlers ([(lambda (x) (exn:break? x))
                                      (lambda (x) (void))]
                                     [(lambda (x) (not (exn:break? x)))
                                      (lambda (x)
                                        (queue-callback
                                         (lambda ()
                                           (switch-to-compose-view)
                                           (message-box 
                                            (string-constant error-sending-bug-report)
                                            (format (string-constant error-sending-bug-report-expln)
                                                    (if (exn? x)
                                                        (exn-message x)
                                                        (format "~s" x)))))))])
                       (parameterize ([current-alist-separator-mode 'amp])
                         (call/input-url 
                          url
                          (case-lambda
                            [(x) (post-pure-port x post-data)]
                            [(x y) (post-pure-port x post-data y)])
                          (lambda (port) (render-html-to-text port response-text #t #f))))
                       (queue-callback
                        (lambda ()
                          (send response-abort enable #f)
                          (send response-reset enable #t)
                          (send response-close enable #t)
                          (set! cancel-kill-thread #f)
                          (send bug-frame set-ok-to-close #t)))))))])
        (set! cancel-kill-thread http-thread)
        (send response-abort enable #t)
        (switch-to-respose-view)))
    
    (define (get-strings canvas)
      (let ([t (send canvas get-editor)])
        (let loop ([n 0])
          (cond
            [(> n (send t last-paragraph)) null]
            [else (cons (send t get-text
                              (send t paragraph-start-position n)
                              (send t paragraph-end-position n))
                        (loop (+ n 1)))]))))
    
    (define (sanity-checking)
      (let ([no-value?
             (lambda (f)
               (cond
                 [(is-a? f editor-canvas%)
                  (= 0 (send (send f get-editor) last-position))]
                 [else (string=? "" (send f get-value))]))])
        (let/ec done-checking
          (for-each
           (lambda (field field-name)
             (when (no-value? field)
               (message-box (string-constant illegal-bug-report)
                            (format (string-constant pls-fill-in-field) field-name))
               (done-checking #f)))
           (list name summary)
           (list (string-constant bug-report-field-name)
                 (string-constant bug-report-field-summary)))
          
          (when (and (no-value? description)
                     (no-value? reproduce))
             (message-box (string-constant illegal-bug-report)
                          (string-constant pls-fill-in-either-description-or-reproduce))
             (done-checking #f))
          
          (unless (member #\@ (string->list (or (preferences:get 'drscheme:email) "")))
            (message-box (string-constant illegal-bug-report)
                         (string-constant malformed-email-address))
            (done-checking #f))
          (done-checking #t))))
    
    (define (ok)
      (when (sanity-checking)
        (send-bug-report)))
    
    (define (cancel)
      (cleanup-frame))
    
    (define (cleanup-frame)
      (send bug-frame close))
        
    (send severity set-selection 1)
    (send version set-value   
          (format "~a"
                  (version:version)))

    (send environment set-value   
          (format "~a ~s (~a) (get-display-depth) = ~a"
                  (system-type)
                  (system-type #t)
                  (system-library-subpath)
                  (get-display-depth)))
    
    (send (send collections get-editor)
          insert       
          (format "~s"
                  (map (lambda (x)
                         (list x 
                               (if (directory-exists? x)
                                   (directory-list x)
                                   "non-existent path")))
                       (current-library-collection-paths))))
    
    (send human-language set-value (format "~a" (this-language)))
    
    (send (send collections get-editor) auto-wrap #t)
    (send (send docs-installed get-editor) auto-wrap #t)
    (send synthesized-button-panel set-alignment 'right 'center)
    
    (align-labels)
    (send button-panel set-alignment 'right 'center)
    (send button-panel stretchable-height #f)
    (send (if (string=? "" (preferences:get 'drscheme:full-name))
              name
              summary)
          focus)
    
    (send (send docs-installed get-editor) insert
          (format "~s" (find-doc-directories)))
    
    (send bug-frame show #t))
  
  (define (ask-yes-or-no title msg parent)
    (gui-utils:get-choice msg 
                          (string-constant yes)
                          (string-constant no)
                          title
                          #f
                          parent)))
