#lang racket/base
(require string-constants
         net/head
         racket/gui/base
         framework
         racket/class
         net/url
         net/uri-codec
         browser/htmltext
         setup/dirs
         "private/buginfo.ss")

(provide help-desk:report-bug)

(define bug-www-server "bugs.racket-lang.org")
(define bug-www-server-port 80)

;; this one should be defined by help desk.
(define frame-mixin
  (namespace-variable-value 'help-desk:frame-mixin #f (lambda () (lambda (x) x))))

(preferences:set-default 'drracket:email "" string? #:aliases '(drscheme:email))
(preferences:set-default 'drracket:full-name "" string? #:aliases '(drscheme:full-name))

(define bug-frame%
  (class (frame-mixin (frame:standard-menus-mixin frame:basic%))
    (init title)
    
    ;; a bunch of stuff we don't want
    (define/override (file-menu:between-print-and-close menu) (void))
    (define/override (edit-menu:between-find-and-preferences menu) (void))
    (define/override (file-menu:create-open?)        #f)
    (define/override (file-menu:create-open-recent?) #f)
    (define/override (file-menu:create-new?)         #f)
    (define/override (file-menu:create-save?)        #f)
    (define/override (file-menu:create-revert?)      #f)
    
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
                              (label (string-constant dialog-back))
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
  (new grow-box-spacer-pane% (parent response-button-panel))
  
  (define top-panel (make-object vertical-panel% outermost-panel))
  
  (define (switch-to-response-view)
    (send response-text lock #f)
    (send response-text erase)
    (render-html-to-text ; hack to get nice text in
     (open-input-string
      "&nbsp;<br><br><br><br><br><div align=\"center\"><h2><b>Submitting bug report...</b></h2></div>")
     response-text #t #f)
    (send response-text lock #t)
    (send single active-child response-panel))
  (define (switch-to-compose-view)
    (send single active-child outermost-panel)
    (send (if (string=? "" (preferences:get 'drracket:full-name))
              name
              summary)
          focus))
  
  (define lps null)
  
  ; build/label : ((union string (list-of string))
  ;                (area-container<%> -> item<%>) 
  ;                boolean
  ;                area-container<%>
  ;             -> item<%>)
  ; constructs and arranges the gui objects for the bug report form
  ; effect: updates lps with the new label panel, for future alignment
  (define build/label
    (lambda (text make-item top? [stretch? #f] [top-panel top-panel] [vertical? #f])
      (define hp (make-object (if vertical?
                                  vertical-panel%
                                  horizontal-panel%)
                            top-panel))
      (define lp (make-object vertical-panel% hp))
      (define ip (make-object vertical-panel% hp))
      (if (string? text)
          (make-object message% text lp)
          (map (lambda (s)
                 (make-object message% s lp))
               text))
      (define item (make-item ip))
      (set! lps (cons lp lps))
      (unless stretch? 
        (send hp stretchable-height #f)
        (send lp stretchable-height #f)
        (send ip stretchable-height #f))
      (send lp stretchable-width #f)
      (send lp stretchable-height #f)
      (send lp set-alignment (if vertical? 'left 'right) (if top? 'top 'center))
      (send ip set-alignment 'left 'top)
      item))
  
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
              (preferences:set 'drracket:full-name (send text get-value)))
            (preferences:get 'drracket:full-name)))))
     #f))
  
  (define email  
    (build/label
     (string-constant bug-report-field-email)
     (lambda (panel)
       (keymap:call/text-keymap-initializer
        (lambda ()
          (make-object text-field% #f panel
            (lambda (text event)
              (preferences:set 'drracket:email (send text get-value)))
            (preferences:get 'drracket:email)))))
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
                     [canvas (new canvas:basic% 
                                  (style '(hide-hscroll))
                                  (parent panel) 
                                  (editor text))])
                (send text set-paste-text-only #t)
                (send text auto-wrap #t)
                (send text set-max-undo-history 'forever)
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
  (define synthesized-button-panel
    (new horizontal-panel% [parent synthesized-dialog]
         [alignment '(right center)] [stretchable-height #f]))
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
  
  (define memory-use
    (build/label 
     (string-constant bug-report-field-memory-use)
     (lambda (panel)
       (keymap:call/text-keymap-initializer
        (lambda ()
          (make-object text-field% #f panel void ""))))
     #f
     #f
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
  
  (define button-panel
    (new horizontal-panel% [parent outermost-panel]
         [alignment '(right center)] [stretchable-height #f]))
  (define synthesized-button (make-object button%
                               (string-constant bug-report-show-synthesized-info)
                               button-panel (lambda x (show-synthesized-info))))
  (define _spacer (new horizontal-pane% (parent button-panel)))
  (define cancel-button (make-object button% (string-constant cancel) button-panel (lambda x (cancel))))
  (define ok-button (make-object button% (string-constant bug-report-submit) button-panel (lambda x (ok))))
  (define _grow-box
    (new grow-box-spacer-pane% [parent button-panel]))
  
  (define (get-query)
    (append (list (cons 'help-desk "true")
                  (cons 'replyto (preferences:get 'drracket:email))
                  (cons 'originator (preferences:get 'drracket:full-name))
                  (cons 'subject (send summary get-value))
                  (cons 'severity (send severity get-string-selection))
                  (cons 'class (translate-class (send bug-class get-string-selection)))
                  (cons 'release (send version get-value))
                  (cons 'description (apply string-append (map (lambda (x) (string-append x "\n")) 
                                                               (get-strings description))))
                  (cons 'how-to-repeat (apply string-append 
                                              (map (lambda (x) (string-append x "\n")) 
                                                   (get-strings reproduce))))
                  (cons 'platform (get-environment)))
            (map (λ (bri) (cons (string->symbol (format "~a" (bri-label bri)))
                                (bri-value bri)))
                 (get-bug-report-infos))))
  
  (define (get-environment)
    (string-append (send environment get-value)
                   "\n"
                   (format "Human Language: ~a\n" (send human-language get-value))
                   (format "(current-memory-use) ~a\n" (send memory-use get-value))
                   "\nCollections:\n"
                   (format "~a" (send (send collections get-editor) get-text))
                   "\n"
                   (apply 
                    string-append
                    (map (lambda (extra)
                           (format "~a: ~a\n"
                                   (car extra)
                                   (send (cdr extra) get-value)))
                         extras))))
  
  ; send-bug-report : (-> void)
  ;; initiates sending the bug report and switches the GUI's mode
  (define (send-bug-report)
    (letrec ([query (get-query)]
             [url
              (string->url (format "http://~a:~a/cgi-bin/bug-report"
                                   bug-www-server
                                   bug-www-server-port))]
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
                        (lambda (port)
                          (send response-text lock #f)
                          (send response-text erase)
                          (render-html-to-text port response-text #t #f)
                          (send response-text lock #t))))
                     (queue-callback
                      (lambda ()
                        (send response-abort enable #f)
                        (send response-reset enable #t)
                        (send response-close enable #t)
                        (set! cancel-kill-thread #f)
                        (send bug-frame set-ok-to-close #t)
                        (send response-close focus)))))))])
      (set! cancel-kill-thread http-thread)
      (send response-abort enable #t)
      (switch-to-response-view)))
  
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
        
        (unless (regexp-match #rx"@" (or (preferences:get 'drracket:email) ""))
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
  
  (define (directories-contents dirs)
    (map (lambda (d)
           (cons (path->string d)
                 (if (directory-exists? d)
                     (map path->string (directory-list d))
                     '(non-existent-path))))
         dirs))
  
  (define (split-by-directories dirs split-by)
    (let ([res (append (map list (map path->string split-by)) '((*)))]
          [dirs (map path->string dirs)])
      (for-each
       (lambda (d)
         (let* ([l (string-length d)]
                [x (assf
                    (lambda (d2)
                      (or (eq? d2 '*)
                          (let ([l2 (string-length d2)])
                            (and (< l2 l) (equal? d2 (substring d 0 l2))
                                 (member (string-ref d l2) '(#\/ #\\))))))
                    res)])
           (append x (list (if (string? (car x))
                               (substring d (add1 (string-length (car x))))
                               d)))))
       dirs)
      (filter (lambda (x) (pair? (cdr x))) res)))
  
  (send response-ec allow-tab-exit #t)
  
  (send severity set-selection 1)
  (send version set-value (format "~a" (version:version)))
  
  (send environment set-value
        (format "~a ~s (~a) (get-display-depth) = ~a"
                (system-type)
                (system-type 'machine)
                (system-library-subpath)
                (get-display-depth)))
  
  (send (send collections get-editor)
        insert
        (format "~s" (directories-contents (get-collects-search-dirs))))
  
  (send human-language set-value (format "~a" (this-language)))
  (send memory-use set-value (format "~a" (current-memory-use)))
  
  (send (send collections get-editor) auto-wrap #t)
  
  ;; Currently, the help-menu is left empty
  (frame:remove-empty-menus bug-frame)
  
  (align-labels)
  (switch-to-compose-view)
  
  (send bug-frame show #t))

(define (ask-yes-or-no title msg parent)
  (gui-utils:get-choice msg 
                        (string-constant yes)
                        (string-constant no)
                        title
                        #f
                        parent))
