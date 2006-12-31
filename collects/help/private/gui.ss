(module gui (lib "a-unit.ss")
  (require (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           
           (lib "class.ss")
           (lib "contract.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "file.ss")
           
           (lib "string-constant.ss" "string-constants")
           (lib "external.ss" "browser")
           
           (lib "browser-sig.ss" "browser")
           (lib "url-sig.ss" "net")
           (lib "url-structs.ss" "net")
           (lib "uri-codec.ss" "net")
           "sig.ss"
           "../bug-report.ss"
           (lib "bday.ss" "framework" "private")
           
           "standard-urls.ss"
           "docpos.ss"
           "manuals.ss"
	   "get-help-url.ss"
           
           "internal-hp.ss")
  
  (import browser^ url^)
  (export gui^)
      
      (define help-desk-frame<%>
        (interface (frame:standard-menus<%>)
          order-manuals
          get-language-name
          change-search-to-status
          set-search-status-contents
          change-status-to-search))

      (define bug-report/help-desk-mixin
        (mixin (frame:standard-menus<%>) ()
          (define/override (file-menu:create-open-recent?) #f)
          (define/override (help-menu:about-string)
            (string-constant plt:hd:about-help-desk))
          (define/override (help-menu:about-callback i e)
            (message-box (string-constant plt:hd:about-help-desk)
                         (format 
                          (string-constant plt:hd:help-desk-about-string)
                          (version:version) 1995 2007)
                         this))
          (define/override (help-menu:create-about?) #t)
          (define/override (help-menu:after-about menu)
            (make-object menu-item% (string-constant plt:hd:help-on-help) menu
              (lambda (i e)
                (message-box
                 (string-constant plt:hd:help-on-help)
                 (string-constant plt:hd:help-on-help-details)
                 this)))
            (new menu-item%
                 (label (string-constant bug-report-submit-menu-item))
                 (parent menu)
                 (callback
                  (lambda (x y)
                    (help-desk:report-bug)))))
          (super-new)))
      
      (define (browser-scroll-frame-mixin %)
        (class %
          (inherit get-hyper-panel)
          
          (define/override (on-subwindow-char w e)
            (or (let ([txt (send (send (get-hyper-panel) get-canvas) get-editor)])
                  (and txt
                       (let ([km (send txt get-hyper-keymap)])
                         (send km handle-key-event txt e))))
                (super on-subwindow-char w e)))
          
          (super-new)))
      
      ;; redirect urls to outside pages to external browsers (depending on the preferences settings)
      ;; also catches links into documentation that isn't installed yet and sends that
      ;; to the missing manuals page.
      (define make-catch-url-frame-mixin
        (let ()
          (define (catch-url-hyper-panel-mixin %)
            (class %
              (define/override (get-canvas%)
                (catch-url-canvas-mixin (super get-canvas%)))
              (super-new)))
          
          (define (catch-url-canvas-mixin %)
            (class %
              
              (define/override (get-editor%) (hd-editor-mixin (super get-editor%)))
              
              (define/override (remap-url url)
                (cond
                  [(url? url)
                   (cond
                     
                     ;; .plt files are always internal, no matter where from
                     ;; they will be caught elsewhere.
                     [(and (url-path url)
                           (not (null? (url-path url)))
                           (regexp-match #rx".plt$" (path/param-path (car (last-pair (url-path url))))))
                      url]
                     
                     ;; files on download.plt-scheme.org in /doc are considered
                     ;; things that we should view in the browser itself.
                     [(is-download.plt-scheme.org/doc-url? url)
                      url]
                     
                     ;; one of the "collects" hosts:
                     [(and (equal? internal-port (url-port url))
                           (ormap (lambda (host)
				    (equal? host (url-host url)))
				  doc-hosts))
		      ;; Two things can go wrong with the URL:
		      ;;  1. The corresponding doc might not be installed
		      ;;  2. There's a relative reference from X to Y, and
		      ;;     X and Y are installed in different directories,
		      ;;     so the host is wrong for Y
		      ;; Resolve 2, then check 1.
		      (let* ([path (url-path url)]
			     [manual (and (pair? path)
					  (path/param-path (car path)))])
			(if manual
			    ;; Find out where this manual is really located:
			    (let* ([path (find-doc-directory (string->path manual))]
				   [real-url (and path
						  (get-help-url path))]
				   [url (if real-url
					    ;; Use the actual host:
					    (make-url (url-scheme url)
						      (url-user url)
						      (url-host (string->url real-url))
						      (url-port url)
						      (url-path-absolute? url)
						      (url-path url)
						      (url-query url)
						      (url-fragment url))
					    ;; Can't do better than the original URL?
					    ;;  The manual is not installed.
					    url)])
			      (if (or (not path)
				      (not (has-index-installed? path)))
				  ;; Manual not installed...
				  (let ([doc-pr (assoc (string->path manual) known-docs)])
				    (string->url
				     (make-missing-manual-url manual
							      (cdr doc-pr) 
							      (url->string url))))
				  ;; Manual here; use revised URL
				  url))
			    ;; Not a manual? Shouldn't happen.
			    url))]

		     ;; one of the other internal hosts
                     [(and (equal? internal-port (url-port url))
			   (is-internal-host? (url-host url)))
		      url]
                     
                     ;; send the url off to another browser
                     [(and (string? (url-scheme url))
                               (not (member (url-scheme url) '("http"))))
                      (send-url (url->string url))
                      #f]
                     [(preferences:get 'drscheme:help-desk:ask-about-external-urls)
                      (case (ask-user-about-separate-browser)
                        [(separate)
                         (send-url (url->string url))
                         #f]
                        [(internal)
                         url]
                        [else #f])]
                     [(preferences:get 'drscheme:help-desk:separate-browser)
                      (send-url url)
                      #f]
                     [else url])]
                  [else url]))
              (super-new)))
          
          ;; has-index-installed? : path -> boolean
          (define (has-index-installed? path)
	    (and (get-index-file path) #t))
          
          (define sk-bitmap #f)
          
          (define hd-editor-mixin
            (mixin (hyper-text<%> editor<%>) ()
              (define/augment (url-allows-evaling? url)
                (and (is-internal-host? (url-host url))
                     (equal? internal-port (url-port url))))
              
              (define show-sk? #t)
              
              (define/override (on-event evt)
                (cond
                  [(and show-sk? 
                        (sk-bday?)
                        (send evt button-down? 'right))
                   (let ([admin (get-admin)])
                     (let ([menu (new popup-menu%)])
                       (new menu-item% 
                            (parent menu)
                            (label (string-constant happy-birthday-shriram))
                            (callback (lambda (x y)
                                        (set! show-sk? #f)
                                        (let ([wb (box 0)]
                                              [hb (box 0)]
                                              [xb (box 0)]
                                              [yb (box 0)])
                                          (send admin get-view xb yb wb hb)
                                          (send admin needs-update (unbox xb) (unbox yb) (unbox wb) (unbox hb))))))
                       (send (get-canvas) popup-menu menu
                             (+ (send evt get-x) 1)
                             (+ (send evt get-y) 1))))]
                  [else (super on-event evt)]))
              
              
              (inherit dc-location-to-editor-location get-admin)
              (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
                (super on-paint before? dc left top right bottom dx dy draw-caret)
                (when before?
                  (when (and show-sk? (sk-bday?))
                    (unless sk-bitmap
                      (set! sk-bitmap (make-object bitmap% (build-path (collection-path "icons") "sk.jpg"))))
                    
                    (let ([admin (get-admin)])
                      (when admin
                        (let*-values ([(view-w view-h) (get-view-w/h admin)]
                                      [(view-x view-y)
                                       (values (- (/ view-w 2) (/ (send sk-bitmap get-width) 2))
                                               (- view-h (send sk-bitmap get-height)))]
                                      ;; note: view coordinates are not exactly canvas dc coordinates
                                      ;; but they are off by a fixed amount (same on all platforms)
                                      ;; (note: dc-location in this method means canvas dc, which is
                                      ;;  different from the dc coming in here (offscreen bitmaps))
                                      [(editor-x editor-y) (dc-location-to-editor-location view-x view-y)]
                                      [(dc-x dc-y) (values (+ editor-x dx)
                                                           (+ editor-y dy))])
                          (send dc draw-bitmap sk-bitmap dc-x dc-y)))))))
              (define/private (get-view-w/h admin)
                (let ([wb (box 0)]
                      [hb (box 0)])
                  (send admin get-view #f #f wb hb)
                  (values (unbox wb)
                          (unbox hb))))
              
              (inherit get-canvas)
              (define/override (init-browser-status-line top-level-window)
                (send top-level-window change-search-to-status))
              (define/override (update-browser-status-line top-level-window s) 
                (send top-level-window set-search-status-contents s))
              (define/override (close-browser-status-line top-level-window) 
                (send top-level-window change-status-to-search))
              
              (super-new)))
          
          (lambda (%)
            (class %
              (define/override (get-hyper-panel%)
                (catch-url-hyper-panel-mixin (super get-hyper-panel%)))
              (super-new)))))
      
      (define (is-download.plt-scheme.org/doc-url? url)
        (and (equal? "download.plt-scheme.org" (url-host url))
             (not (null? (url-path url)))
             (equal? (path/param-path (car (url-path url))) "doc")))
      
      ;; ask-user-about-separate-browser : -> (union #f 'separate 'internal)
      (define (ask-user-about-separate-browser)
        (define separate-default? (preferences:get 'drscheme:help-desk:separate-browser))

	(let-values ([(result checked?) 
		      (message+check-box/custom
		       (string-constant help-desk)
		       (string-constant plt:hd:ask-about-separate-browser)
		       (string-constant dont-ask-again-always-current)
		       (string-constant plt:hd:homebrew-browser)
		       (string-constant plt:hd:separate-browser)
		       (string-constant cancel)
		       #f ; no parent
		       (cons
			(if separate-default?
			    'default=2
			    'default=1)
			'(no-default)))])
          (when checked?
            (preferences:set 'drscheme:help-desk:ask-about-external-urls #f))
          (case result
            [(2)
             (preferences:set 'drscheme:help-desk:separate-browser #t)
             'separate]
            [(1)
             (preferences:set 'drscheme:help-desk:separate-browser #f)
             'internal]
            [(#f 3)
             #f]
            [else (error 'ack)])))
      
      (define make-help-desk-framework-mixin 
        (mixin (frame:searchable<%> frame:standard-menus<%>) ()
          (define/override (get-text-to-search)
            (send (send (send this get-hyper-panel) get-canvas) get-editor))
          
          (define/override (file-menu:create-new?) #t)
          (define/override (file-menu:new-callback x y) (new-help-desk))
          
          (define/override (file-menu:create-open-recent?) #f)
          
          (define/override (file-menu:create-open?) #f)
          (define/override (file-menu:create-print?) #t)
          
          (define/override (file-menu:print-callback x y)
            (let ([ed (send (send (send this get-hyper-panel) get-canvas) get-editor)])
              (and ed
                   (send ed print))))
          
          (define/override (file-menu:between-open-and-revert file-menu)
            (super file-menu:between-open-and-revert file-menu)
            (instantiate menu:can-restore-menu-item% ()
              (parent file-menu)
              (callback (lambda (_1 _2) (open-url-callback)))
              (label (string-constant open-url...)))
            (instantiate menu:can-restore-menu-item% ()
              (parent file-menu)
              (label (string-constant reload))
              (callback (lambda (_1 _2) (send (send this get-hyper-panel) reload)))))
          
          (define/private (open-url-callback)
            (let ([url (get-url-from-user this)])
              (when url
                (let* ([hp (send this get-hyper-panel)]
                       [hc (send hp get-canvas)])
                  (send hc goto-url url #f)))))
          
          (define/override (on-size w h)
            (preferences:set 'drscheme:help-desk:frame-width w)
            (preferences:set 'drscheme:help-desk:frame-height h)
            (super on-size w h))
          
          (super-new
           (width (preferences:get 'drscheme:help-desk:frame-width))
           (height (preferences:get 'drscheme:help-desk:frame-height)))
          
          (frame:reorder-menus this)))
      
      (define make-search-button-mixin
        (mixin (frame:basic<%> hyper-frame<%>) ()
          (field [search-panel #f])
          
          ;; order-manuals : as in drscheme:language:language<%>
          ;; by default, search in all manuals
          (define/public (order-manuals x) (values x #t))
          
          ;; the name of the language to put in the top of the search results,
          ;; or #f if nothing is to be put there.
          (define/public (get-language-name) #f)
          
          (define/override (make-root-area-container class parent)
            (let* ([search-panel-parent (super make-root-area-container vertical-panel% parent)]
                   [main-panel (make-object class search-panel-parent)])
              (set! search-panel (instantiate vertical-panel% ()
                                   (parent search-panel-parent)
                                   (stretchable-height #f)))
              main-panel))
          
          ;; these methods have the same name as the methods in the browser.
          ;; they are called during super initialization, so they protect themselves...
          (define/public (change-search-to-status)
            (when search/status-panel
              (send search/status-panel active-child status-panel)))
          (define/public (set-search-status-contents s)
            (when status-message
              (send status-message set-label (trim-string 200 s))))
          (define/private (trim-string n str)
            (cond
              [(<= (string-length str) n) str]
              [else (string-append (substring str 0 98)
                                   " ... "
                                   (substring str (- (string-length str) 
                                                     97)
                                              (string-length str)))]))
          
          (define/public (change-status-to-search) 
            (when search/status-panel
              (send search/status-panel active-child field-panel)
              (send search-field focus)))
          
          (field [search/status-panel #f]
                 [field-panel #f]
                 [search-field #f]
                 [status-panel #f]
                 [status-message #f]
                 [choices-panel #f])
          
          (super-new (label (string-constant help-desk)))
          
          (let ([hp (send this get-hyper-panel)])
            (send hp set-init-page home-page-url)
            (send (send hp get-canvas) allow-tab-exit #t))
          
          (inherit get-menu-bar get-hyper-panel)
          (let ()
            (define search-menu (instantiate menu% ()
                                  (label (string-constant plt:hd:search))
                                  (parent (get-menu-bar))))
            (define search-menu-item (instantiate menu:can-restore-menu-item% ()
                                       (label (string-constant plt:hd:search))
                                       (parent search-menu)
                                       (shortcut #\e)
                                       (callback
                                        (lambda (x y) (search-callback #f)))))
            (define lucky-menu-item (instantiate menu:can-restore-menu-item% ()
                                      (label (string-constant plt:hd:feeling-lucky))
                                      (parent search-menu)
                                      (shortcut #\u)
                                      (callback
                                       (lambda (x y) (search-callback #t)))))
            (define stupid-internal-define-syntax1
              (set! search/status-panel (new panel:single% 
                                             (parent search-panel)
                                             (stretchable-width #t))))
            (define stupid-internal-define-syntax2
              (set! field-panel (new horizontal-panel% (parent search/status-panel))))
            (define stupid-internal-define-syntax3
              (set! status-panel (new horizontal-panel% (parent search/status-panel))))
            (define stupid-internal-define-syntax4
              (set! status-message (new message% 
                                        (parent status-panel)
                                        (stretchable-width #t)
                                        (label ""))))
            (define stupid-internal-define-syntax5
              (set! search-field (instantiate text-field% ()
                                   (label (string-constant plt:hd:find-docs-for))
                                   (callback (lambda (x y)
                                               (let ([on? (not (equal? "" (send search-field get-value)))])
                                                 (send search-button enable on?)
                                                 (send search-menu enable on?))))
                                   (parent field-panel))))
            
            ;; exposed to derived classes
            (define stupid-internal-define-syntax6
              (set! choices-panel (instantiate horizontal-panel% ()
                                    (parent search-panel)
                                    (alignment '(center center)))))
            
            (define search-button (instantiate button% ()
                                    (label (string-constant plt:hd:search))
                                    (parent field-panel)
                                    (callback (lambda (x y) (search-callback #f)))
                                    (style '(border))))
            (define search-where (instantiate choice% ()
                                   (label #f)
                                   (parent choices-panel)
                                   (selection (preferences:get 'drscheme:help-desk:search-where))
                                   (choices
                                    (list
                                     (string-constant plt:hd:search-for-keyword)
                                     (string-constant plt:hd:search-for-keyword-or-index)
                                     (string-constant plt:hd:search-for-keyword-or-index-or-text)))
                                   (callback
                                    (lambda (x y)
                                      (preferences:set 'drscheme:help-desk:search-where
                                                       (send search-where get-selection))))))
            (define search-how (instantiate choice% ()
                                 (label #f)
                                 (parent choices-panel)
                                 (selection (preferences:get 'drscheme:help-desk:search-how))
                                 (choices 
                                  (list
                                   (string-constant plt:hd:exact-match)
                                   (string-constant plt:hd:containing-match)
                                   (string-constant plt:hd:regexp-match)))
                                 (callback
                                  (lambda (x y)
                                    (preferences:set 'drscheme:help-desk:search-how
                                                     (send search-how get-selection))))))
            
            (define grow-box-spacer (make-object grow-box-spacer-pane% choices-panel))
            (define (search-callback lucky?)
              (let-values ([(manuals doc.txt?) (order-manuals (map path->bytes (map car (find-doc-names))))])
                (let ([url (make-results-url
                            (send search-field get-value)
                            (case (send search-where get-selection)
                              [(0) "keyword"]
                              [(1) "keyword-index"]
                              [(2) "keyword-index-text"])
                            (case (send search-how get-selection)
                              [(0) "exact-match"]
                              [(1) "containing-match"]
                              [(2) "regexp-match"])
                            lucky?
                            (map bytes->path manuals)
                            doc.txt?
                            (get-language-name))])
                  (send (send (get-hyper-panel) get-canvas) goto-url url #f))))
            
            (send search-button enable #f)
            (send search-menu enable #f)
            (send search-field focus))))
      
      (define help-desk-frame-mixin #f)
      (define addl-mixins (lambda (x) x))
      (define (add-help-desk-mixin m)
        (if help-desk-frame-mixin
            (error 'add-help-desk-mixin "help desk frame has already been created")
            (set! addl-mixins (compose m addl-mixins))))
      (define (make-help-desk-frame-mixin)
        (or help-desk-frame-mixin
            (begin
              (set! help-desk-frame-mixin
                    (compose
                     addl-mixins
                     (lambda (x) (class* x (help-desk-frame<%>) (super-new)))
                     make-catch-url-frame-mixin
                     bug-report/help-desk-mixin
                     make-help-desk-framework-mixin
                     browser-scroll-frame-mixin
                     frame:searchable-mixin
                     frame:standard-menus-mixin
                     make-search-button-mixin))
              help-desk-frame-mixin)))
      
      (define new-help-desk
        (opt-lambda ([link home-page-url])
          (let ([f (new ((make-help-desk-frame-mixin) hyper-no-show-frame%))])
            (send f show #t)
            (goto-url link f)
            f)))
      
      (define (goto-hd-location sym)
        (let ([loc (get-hd-location sym)])
          (goto-url loc)))
      
      (define (goto-manual-link manual index-key)
        (goto-url (prefix-with-server (finddoc-page-anchor manual index-key))))
      
      (define (search-for-docs search-string search-type match-type lucky? docs)
        (let ([fr (or (find-help-desk-frame)
                      (new-help-desk))])
          (search-for-docs/in-frame fr search-string search-type match-type lucky? docs)))
      
      (define (search-for-docs/in-frame fr search-string search-type match-type lucky? docs)
        (send fr show #t)
        (let-values ([(manuals doc.txt?) (send fr order-manuals (map path->bytes docs))])
          (goto-url (make-results-url search-string
                                      search-type 
                                      match-type
                                      lucky?
                                      (map bytes->path manuals)
                                      doc.txt?
                                      (send fr get-language-name))
                    fr)))
      
      (define goto-url
        (opt-lambda (link [fr (find-help-desk-frame)])
          (if fr
              (send (send (send fr get-hyper-panel) get-canvas) goto-url link #f)
              (new-help-desk link))))
      
      (define (show-help-desk)
        (let ([fr (find-help-desk-frame)])
          (if fr
              (send fr show #t)
              (new-help-desk))))
      
      (define (find-help-desk-frame)
        (let loop ([frames (send (group:get-the-frame-group) get-frames)])
          (cond
            [(null? frames) #f]
            [else (let ([frame (car frames)])
                    (if (is-a? frame help-desk-frame<%>)
                        frame
                        (loop (cdr frames))))])))
      
      (define (get-url-from-user parent)
        (define d (make-object dialog% (string-constant open-url) parent 500))
        (define t
          (keymap:call/text-keymap-initializer
           (lambda ()
             (make-object text-field% (string-constant url:) d
               (lambda (t e)
                 (update-ok))))))
        (define p (make-object horizontal-panel% d))
        (define browse (make-object button% (string-constant browse...) p
                         (lambda (b e)
                           (let ([f (get-file)])
                             (when f
                               (send t set-value (encode-file-path-as-url f))
                               (update-ok))))))
          
        (define (encode-file-path-as-url f)
          (apply
           string-append
           "file:"
           (map
            (λ (x) (string-append "/" (uri-path-segment-encode (path->string x))))
            (explode-path f))))
        
        (define spacer (make-object vertical-pane% p))
        (define result #f)
        (define (ok-callback b e)
          (let* ([s (send t get-value)]
                 [done (lambda ()
                         ;; Might be called twice!
                         (preferences:set 'drscheme:help-desk:last-url-string s)
                         (send d show #f))])
            (with-handlers ([exn:fail?
                             (lambda (x)
                               (message-box (string-constant bad-url) 
                                            (format (string-constant bad-url:this)
                                                    (exn-message x))
                                            d))])
              (let* ([removed-spaces (regexp-replace #rx"^[ \t]*" s "")]
                     [str (cond
                            [(regexp-match #rx":" removed-spaces) removed-spaces]
                            [(regexp-match #rx"^[a-zA-Z][a-zA-Z.]*($|/)" removed-spaces)
                             (string-append "http://" removed-spaces)]
                            [else
                             (string-append "file:" removed-spaces)])]
                     
                     ;; just convert the string to test it out; don't use result...
                     [url (string->url str)])
                (set! result str)
                (done)))))
        (define cancel-callback (lambda (b e) (send d show #f)))
        (define-values (ok cancel)
          (gui-utils:ok/cancel-buttons
           p
           ok-callback
           cancel-callback))
        (define (update-ok)
          (send ok enable 
                (positive? (send (send t get-editor) 
                                 last-position))))
        (define last-url-string (preferences:get 'drscheme:help-desk:last-url-string))
        (when last-url-string 
          (send t set-value last-url-string)
          (let ([text (send t get-editor)])
            (send text set-position 0 (send text last-position))))
        (send p set-alignment 'right 'center)
        (update-ok)
        (send d center)
        (send t focus)
        (send d show #t)
        result))
