#lang racket/unit
  (require string-constants
           racket/match
           racket/class
           racket/string
           racket/file
           racket/math
           "drsig.rkt"
           mred
           framework
           net/url
           net/head
           setup/plt-installer
           help/bug-report
           setup/unpack)
  
  (import [prefix drracket:unit: drracket:unit^]
          [prefix drracket:app: drracket:app^]
          [prefix help: drracket:help-desk^]
          [prefix drracket:multi-file-search: drracket:multi-file-search^]
          [prefix drracket:init: drracket:init^]
          [prefix drracket: drracket:interface^])
  (export (rename drracket:frame^
                  [-mixin mixin]))
  
  (define last-keybindings-planet-attempt "")
  
  (define basics-mixin
    (mixin (frame:standard-menus<%>) (drracket:frame:basics<%>)
      
      (define/override (on-subwindow-focus win on?)
        (when the-keybindings-frame
          (when on? 
            (send the-keybindings-frame set-bindings
                  (if (can-show-keybindings?)
                      (get-keybindings-to-show)
                      '())))))
      
      (define/override (on-subwindow-char receiver event)
        (let ([user-key? (send (keymap:get-user) 
                               handle-key-event
                               (if (is-a? receiver editor-canvas%)
                                   (send receiver get-editor)
                                   receiver)
                               event)])
          ;; (printf "user-key? ~s\n" user-key?) returns #t for key release events -- is this a problem? (we'll find out!)
          (or user-key?
              (super on-subwindow-char receiver event))))
      
      (inherit get-edit-target-window get-edit-target-object get-menu-bar)
      (define/private (get-menu-bindings)
        (let ([name-ht (make-hasheq)])
          (let loop ([menu-container (get-menu-bar)])
            (for-each
             (λ (item)
               (when (is-a? item selectable-menu-item<%>)
                 (let ([short-cut (send item get-shortcut)])
                   (when short-cut
                     (let ([keyname
                            (string->symbol
                             (keymap:canonicalize-keybinding-string
                              (string-append
                               (menu-item->prefix-string item)
                               (case short-cut
                                 [(#\;) "semicolon"]
                                 [(#\:) "colon"]
                                 [(#\space) "space"]
                                 [else 
                                  (cond
                                    [(symbol? short-cut) (symbol->string short-cut)]
                                    [(char? short-cut) (string short-cut)])]))))])
                       (hash-set! name-ht keyname (send item get-plain-label))))))
               (when (is-a? item menu-item-container<%>)
                 (loop item)))
             (send menu-container get-items)))
          (when (eq? (system-type) 'windows)
            (for-each (λ (top-level-menu) 
                        (when (is-a? top-level-menu menu%)
                          (let ([amp-key
                                 (let loop ([str (send top-level-menu get-label)])
                                   (cond
                                     [(regexp-match #rx"[^&]*[&](.)(.*)" str)
                                      =>
                                      (λ (m)
                                        (let ([this-amp (list-ref m 1)]
                                              [rest (list-ref m 2)])
                                          (cond
                                            [(equal? this-amp "&")
                                             (loop rest)]
                                            [else 
                                             (string-downcase this-amp)])))]
                                     [else #f]))])
                            (when amp-key
                              (hash-set! name-ht 
                                         (format "m:~a" amp-key)
                                         (format "~a menu" (send top-level-menu get-plain-label)))
                              (hash-set! name-ht 
                                         (format "m:s:~a" amp-key)
                                         (format "~a menu" (send top-level-menu get-plain-label)))))))
                      (send (get-menu-bar) get-items)))
          name-ht))
      
      (define/private (menu-item->prefix-string item)
        (apply
         string-append
         (map (λ (prefix)
                (case prefix
                  [(alt) (if (eq? (system-type) 'windows)
                             "m:"
                             "a:")]
                  [(cmd) "d:"]
                  [(meta) "m:"]
                  [(ctl) "c:"]
                  [(shift) "s:"]
                  [(opt option) "a:"]
                  [else (error 'menu-item->prefix-string "unknown prefix ~s\n" prefix)]))
              (send item get-shortcut-prefix))))
      
      (define/private (copy-hash-table ht)
        (let ([res (make-hasheq)])
          (hash-for-each
           ht
           (λ (x y) (hash-set! res x y)))
          res))
      
      (define/private (can-show-keybindings?)
        (let ([edit-object (get-edit-target-object)])
          (and edit-object
               (is-a? edit-object editor<%>)
               (let ([keymap (send edit-object get-keymap)])
                 (is-a? keymap keymap:aug-keymap<%>)))))
      
      ;; pre: (can-show-keybindings?) = #t
      (define/private (get-keybindings-to-show)
        (define edit-object (get-edit-target-object))
        (define keymap (send edit-object get-keymap))
        (define menu-names (get-menu-bindings))
        (define table (send keymap get-map-function-table))
        (define bindings (hash-map table list))
        (define w/menus 
          (append (hash-map menu-names list)
                  (filter (λ (binding) (not (bound-by-menu? binding menu-names)))
                          bindings)))
        (sort
         w/menus
         (λ (x y) (string-ci<=? (cadr x) (cadr y)))))
      
      (define/private (show-keybindings)
        (if (can-show-keybindings?)
            (show-keybindings-to-user (get-keybindings-to-show) this)
            (bell)))
      
      (define/private (bound-by-menu? binding menu-table)
        (ormap (λ (constituent)
                 (hash-ref menu-table (string->symbol constituent) (λ () #f)))
               (regexp-split #rx";" (symbol->string (car binding)))))
      
      (define/override (help-menu:before-about help-menu)
        (make-help-desk-menu-item help-menu))
      
      (define/override (help-menu:about-callback item evt) (drracket:app:about-drscheme))
      (define/override (help-menu:about-string) (string-constant about-drscheme))
      (define/override (help-menu:create-about?) #t)
      
      (define/public (get-additional-important-urls) '())
      (define/override (help-menu:after-about menu)
        (drracket-help-menu:after-about menu this))
      
      (define/override (file-menu:new-string) (string-constant new-menu-item))
      (define/override (file-menu:open-string) (string-constant open-menu-item))
      
      (define/override (file-menu:between-open-and-revert file-menu) 
        (make-object menu-item% 
          (string-constant install-plt-file-menu-item...)
          file-menu
          (λ (item evt)
            (install-plt-file this)))
        (super file-menu:between-open-and-revert file-menu))
      
      (define/override (file-menu:between-print-and-close menu)
        (super file-menu:between-print-and-close menu)
        (instantiate menu-item% ()
          (label (string-constant mfs-multi-file-search-menu-item))
          (parent menu)
          (callback
           (λ (_1 _2)
             (drracket:multi-file-search:multi-file-search))))
        (new separator-menu-item% (parent menu)))
      
      (define/override (edit-menu:between-find-and-preferences menu)
        (super edit-menu:between-find-and-preferences menu)
        (when (current-eventspace-has-standard-menus?)
          (new separator-menu-item% [parent menu]))
        (let ([keybindings-on-demand
               (λ (menu-item)
                 (let ([last-edit-object (get-edit-target-window)])
                   (send menu-item enable (can-show-keybindings?))))])
          (instantiate menu% ()
            (label (string-constant keybindings-menu-item))
            (parent menu)
            (demand-callback
             (λ (keybindings-menu)
               (for-each (λ (old) (send old delete)) 
                         (send keybindings-menu get-items))
               (new menu-item%
                    (parent keybindings-menu)
                    (label (string-constant keybindings-show-active))
                    (callback (λ (x y) (show-keybindings)))
                    (help-string (string-constant keybindings-info))
                    (demand-callback keybindings-on-demand))
               (new menu-item%
                    (parent keybindings-menu)
                    (label (string-constant keybindings-add-user-defined-keybindings))
                    (callback
                     (λ (x y)
                       (with-handlers ([exn? (λ (x)
                                               (printf "~a\n" (exn-message x)))])
                         (let ([filename (finder:get-file
                                          #f
                                          (string-constant keybindings-choose-user-defined-file)
                                          #f
                                          ""
                                          this)])
                           (when filename
                             (add-keybindings-item/update-prefs filename)))))))
               (new menu-item%
                    (parent keybindings-menu)
                    (label (string-constant keybindings-add-user-defined-keybindings/planet))
                    (callback
                     (λ (x y)
                       (let ([planet-spec (get-text-from-user (string-constant drscheme)
                                                              (string-constant keybindings-type-planet-spec)
                                                              this
                                                              last-keybindings-planet-attempt)])
                         (when planet-spec
                           (set! last-keybindings-planet-attempt planet-spec)
                           (cond
                             [(planet-string-spec? planet-spec)
                              =>
                              (λ (planet-sexp-spec)
                                (add-keybindings-item/update-prefs planet-sexp-spec))]
                             [else
                              (message-box (string-constant drscheme)
                                           (format (string-constant keybindings-planet-malformed-spec)
                                                   planet-spec)
                                           #:dialog-mixin frame:focus-table-mixin)]))))))
               (let ([ud (preferences:get 'drracket:user-defined-keybindings)])
                 (unless (null? ud)
                   (new separator-menu-item% (parent keybindings-menu))
                   (for-each (λ (item)
                               (new menu-item%
                                    (label (format (string-constant keybindings-menu-remove)
                                                   (if (path? item)
                                                       (path->string item)
                                                       (format "~s" item))))
                                    (parent keybindings-menu)
                                    (callback
                                     (λ (x y) (remove-keybindings-item item)))))
                             ud)))))))
        (unless (current-eventspace-has-standard-menus?)
          (make-object separator-menu-item% menu)))
      
      (super-new)))
  
  (define (add-keybindings-item/update-prefs item)
    (when (add-keybindings-item item)
      (preferences:set 'drracket:user-defined-keybindings
                       (cons item
                             (preferences:get 'drracket:user-defined-keybindings)))))
  
  (define (planet-string-spec? p)
    (let ([sexp
           (with-handlers ([exn:fail:read? (λ (x) #f)])
             (read (open-input-string p)))])
      (and sexp
           (planet-spec? sexp)
           sexp)))
  
  (define (planet-spec? p)
    (match p
      [`(planet ,(? string?) (,(? string?) ,(? string?) ,(? number?))) #t]
      [`(planet ,(? string?) (,(? string?) ,(? string?) ,(? number?) ,(? number?))) #t]
      [else #f]))
  
  ;; add-keybindings-item : keybindings-item[path or planet spec] -> boolean
  ;; boolean indicates if the addition happened sucessfully
  (define (add-keybindings-item item)
    (with-handlers ([exn:fail?
                     (λ (x)
                       (message-box (string-constant drscheme)
                                    (format (string-constant keybindings-error-installing-file)
                                            (if (path? item)
                                                (path->string item)
                                                (format "~s" item))
                                            (exn-message x))
                                    #:dialog-mixin frame:focus-table-mixin)
                       #f)])
      (keymap:add-user-keybindings-file item)
      #t))
  
  (define (remove-keybindings-item item)
    (keymap:remove-user-keybindings-file item)
    (preferences:set
     'drracket:user-defined-keybindings
     (remove item
             (preferences:get 'drracket:user-defined-keybindings))))
  
  ;; install-plt-file : (union #f dialog% frame%) -> void
  ;; asks the user for a .plt file, either from the web or from
  ;; a file on the disk and installs it.
  (define (install-plt-file parent)
    (define pref (preferences:get 'drracket:install-plt-dialog))
    (define dialog
      (new dialog% [parent parent]
           [label (string-constant install-plt-file-dialog-title)]
           [alignment '(left center)]))
    (define tab-panel
      (new tab-panel% [parent dialog]
           [callback (λ (x y) (update-panels))]
           [choices (list (string-constant install-plt-web-tab)
                          (string-constant install-plt-file-tab))]))
    (define outer-swapping-panel
      (new horizontal-panel% [parent tab-panel]
           [stretchable-height #f]))
    (define spacing-panel
      (new horizontal-panel% [parent outer-swapping-panel]
           [stretchable-width #f]
           [min-width 20]))
    (define swapping-panel
      (new panel:single% [parent outer-swapping-panel]
           [alignment '(left center)]
           [stretchable-width #t] [stretchable-height #f]))
    (define file-panel
      (new horizontal-panel% [parent swapping-panel]
           [stretchable-width #t] [stretchable-height #f]))
    (define url-panel
      (new horizontal-panel% [parent swapping-panel]
           [stretchable-height #f]))
    (define button-panel
      (new horizontal-panel% [parent dialog]
           [stretchable-height #f] [alignment '(right center)]))
    (define file-text-field
      (new text-field% [parent file-panel]
           [callback void] [min-width 300] [stretchable-width #t]
           [init-value (caddr pref)]
           [label (string-constant install-plt-filename)]))
    (define file-button
      (new button% [parent file-panel]
           [callback (λ (x y) (browse))]
           [label (string-constant browse...)]))
    (define url-text-field
      (new text-field% [parent url-panel]
           [min-width 300] [stretchable-width #t] [callback void]
           [init-value (cadr pref)]
           [label (string-constant install-plt-url)]))
    (define-values (ok-button cancel-button)
      (gui-utils:ok/cancel-buttons
       button-panel
       (λ (x y) (set! cancel? #f) (send dialog show #f))
       (λ (x y) (send dialog show #f))))
    ;; browse : -> void
    ;; gets the name of a file from the user and updates file-text-field
    (define (browse)
      (let ([filename (parameterize ([finder:default-extension "plt"]
                                     [finder:default-filters
                                      (if (eq? (system-type) 'macosx)
                                          (finder:default-filters)
                                          '(("PLT Files" "*.plt")
                                            ("Any" "*.*")))])
                        (finder:get-file #f "" #f "" dialog))])
        (when filename
          (send file-text-field set-value (path->string filename)))))
    ;; from-web? : -> boolean
    ;; returns #t if the user has selected a web address
    (define (from-web?)
      (zero? (send tab-panel get-selection)))
    (define cancel? #t)
    (define (update-panels)
      (define w? (from-web?))
      (define t  (if w? url-text-field file-text-field))
      (send swapping-panel active-child (if w? url-panel file-panel))
      (send t focus)
      (send (send t get-editor) set-position
            0 (string-length (send t get-value))))
    ;; initialize
    (send tab-panel set-selection (if (car pref) 0 1))
    (update-panels)
    (send dialog show #t)
    (preferences:set 'drracket:install-plt-dialog
                     (list (from-web?)
                           (send url-text-field get-value)
                           (send file-text-field get-value)))
    (cond
      [cancel? (void)]
      [(from-web?)
       (install-plt-from-url
        (let* ([url (send url-text-field get-value)]
               ;; trim whitespaces
               [url (regexp-replace #rx"^ +" url "")]
               [url (regexp-replace #rx" +$" url "")])
          (if (regexp-match? #rx"^(?:[^/:]*://|$)" url)
            url
            (string-append "http://" url)))
        parent)]
      [else (parameterize ([error-display-handler
                            drracket:init:original-error-display-handler])
              (run-installer
               (string->path (send file-text-field get-value))))]))

  ;; install-plt-from-url : string (union #f dialog%) -> void
  ;; downloads and installs a .plt file from the given url
  (define (install-plt-from-url s-url parent)
    (define-values (port size)
      (let-values ([(port header)
                    (get-pure-port/headers (string->url s-url)
                                           #:redirections 5)])
        (define size
          (let* ([content-header (extract-field "content-length" header)]
                 [m (and content-header
                         (regexp-match "[0-9]+" content-header))])
            (and m (string->number (car m)))))
        (values port size)))
    (let* ([tmp-filename (make-temporary-file "tmp~a.plt")]
           [header (purify-port port)]
           [d (make-object dialog% (string-constant downloading) parent)] 
           [message (make-object message% (string-constant downloading-file...) d)] 
           [gauge (if size 
                      (make-object gauge% #f 100 d) 
                      #f)] 
           [exn #f] 
           ; Semaphores to avoid race conditions: 
           [wait-to-start (make-semaphore 0)] 
           [wait-to-break (make-semaphore 0)] 
           ; Thread to perform the download: 
           [t (thread 
               (λ () 
                 (semaphore-wait wait-to-start) 
                 (with-handlers ([exn:fail?
                                  (λ (x) 
                                    (set! exn x))] 
                                 [exn:break? ; throw away break exceptions 
                                  void])
                   (semaphore-post wait-to-break) 
                   (with-output-to-file tmp-filename 
                     (λ () 
                       (let loop ([total 0]) 
                         (when gauge 
                           (send gauge set-value  
                                 (inexact->exact 
                                  (floor (* 100 (/ total size)))))) 
                         (let ([s (read-string 1024 port)]) 
                           (unless (eof-object? s) 
                             (unless (eof-object? s) 
                               (display s) 
                               (loop (+ total (string-length s))))))))
                     #:mode 'binary #:exists 'truncate))
                 (send d show #f)))]) 
      (send d center) 
      (make-object button% (string-constant &stop)
        d
        (λ (b e) 
          (semaphore-wait wait-to-break) 
          (set! tmp-filename #f) 
          (send d show #f) 
          (break-thread t))) 
      ; Let thread run only after the dialog is shown 
      (queue-callback (λ () (semaphore-post wait-to-start))) 
      (send d show #t) 
      (when exn (raise exn))
      (define unpack-err (open-output-string))
      (cond
        [(with-handlers ((exn:fail? values))
           (parameterize ([error-display-handler drracket:init:original-error-display-handler]
                          [current-error-port unpack-err])
             (fold-plt-archive tmp-filename void void void void void))
           #f)
         =>
         (λ (exn)
           (delete-file tmp-filename)
           (message-box (string-constant drscheme)
                        (string-append 
                         (string-constant install-plt-error-header)
                         "\n\n"
                         (exn-message exn)
                         "\n\n"
                         (get-output-string unpack-err))
                        #:dialog-mixin frame:focus-table-mixin))]
        [else
         (parameterize ([error-display-handler drracket:init:original-error-display-handler])
           (run-installer tmp-filename
                          (λ ()
                            (delete-file tmp-filename))))])))
  
  (define keybindings-frame%
    (class frame%
      (init-field bindings)
      
      (define/override (on-size w h)
        (preferences:set 'drracket:keybindings-window-size (cons w h))
        (super on-size w h))

      (super-new)
  
      (define/public (set-bindings _bindings)
        (set! bindings _bindings)
        (update-bindings))
      
      (define bp (make-object horizontal-panel% this))
      (define search-field (new text-field% 
                                [parent this]
                                [label (string-constant mfs-search-string)]
                                [callback (λ (a b) (update-bindings))]))
      (define b-name (new button%
                          [label (string-constant keybindings-sort-by-name)]
                          [parent bp]
                          [callback
                           (λ x 
                             (set! by-key? #f)
                             (update-bindings))]))
      (define b-key (new button%
                         [label (string-constant keybindings-sort-by-key)]
                         [parent bp]
                         [callback (λ x 
                                     (set! by-key? #t)
                                     (update-bindings))]))
      (define lb (new list-box% [parent this] [label #f] [choices '()]))
      (define by-key? #f)
      (define bp2 (make-object horizontal-panel% this))
      (define cancel (make-object button% (string-constant close)
                       bp2 (λ x (send this show #f))))

      (define/private (update-bindings)
        (let ([format-binding/name
               (λ (b) (format "~a (~a)" (cadr b) (car b)))]
              [format-binding/key
               (λ (b) (format "~a (~a)" (car b) (cadr b)))]
              [predicate/key
               (λ (a b) (string-ci<=? (format "~a" (car a))
                                      (format "~a" (car b))))]
              [predicate/name
               (λ (a b) (string-ci<=? (cadr a) (cadr b)))])
          (send lb set
                (if by-key?
                    (map format-binding/key (sort (filter-search bindings) predicate/key))
                    (map format-binding/name (sort (filter-search bindings) predicate/name))))))
      
      (define/private (filter-search bindings)
        (let ([str (send search-field get-value)])
          (if (equal? str "")
              bindings
              (let ([reg (regexp (regexp-quote str #f))])
                (filter (λ (x) (or (regexp-match reg (cadr x))
                                   (regexp-match reg (format "~a" (car x)))))
                        bindings)))))
      (send search-field focus)
      (send bp stretchable-height #f)
      (send bp set-alignment 'center 'center)
      (send bp2 stretchable-height #f)
      (send bp2 set-alignment 'right 'center)
      (update-bindings)))

  (define the-keybindings-frame #f)
  
  (define (show-keybindings-to-user bindings frame)
    (unless the-keybindings-frame
      (set! the-keybindings-frame
            (new keybindings-frame% 
                 [label (string-constant keybindings-frame-title)]
                 [width (car (preferences:get 'drracket:keybindings-window-size))]
                 [height (cdr (preferences:get 'drracket:keybindings-window-size))]
                 [bindings bindings])))
    (send the-keybindings-frame show #t))
  
  (define -mixin
    (mixin (frame:editor<%> frame:text-info<%> drracket:frame:basics<%>) (drracket:frame:<%>)
      (inherit get-editor get-menu% get-menu-bar)
      (define show-menu #f)
      (define/public get-show-menu (λ () show-menu))
      (define/public update-shown (λ () (void)))
      (define/public (add-show-menu-items show-menu) (void))
      (define sort-menu-sort-keys (make-hasheq))
      (define/public (set-show-menu-sort-key item val)
        (cond
          [sort-menu-sort-keys 
           (for ([(k v) (in-hash sort-menu-sort-keys)])
             (when (eq? k item)
               (error 'set-show-menu-sort-key
                      "set menu item ~s twice, to ~s and ~s"
                      (send item get-label)
                      v val))
             (when (= v val)
               (error 'set-show-menu-sort-key
                      "two menu items have the same val: ~s and ~s"
                      (send k get-label)
                      (send item get-label))))
           (hash-set! sort-menu-sort-keys item val)]
          [else
           (error 'set-show-menu-sort-key 
                  "the sort menu has already been created and its order has been set")]))
      (super-new)
      (set! show-menu (make-object (get-menu%) (string-constant view-menu-label)
                        (get-menu-bar)))
      (add-show-menu-items show-menu)
      (sort-show-menu-items show-menu sort-menu-sort-keys)
      (set! sort-menu-sort-keys #f)))
  
  (define (sort-show-menu-items show-menu show-menu-sort-keys)
    (define items (send show-menu get-items))
    (for ([itm (in-list items)])
      (send itm delete))
    (define (get-key item)
      (hash-ref show-menu-sort-keys item 
                (λ () 
                  (define lab
                    (cond
                      [(is-a? item labelled-menu-item<%>)
                       (send item get-label)]
                      [else ""]))
                  (cond 
                    [(regexp-match #rx"^Show (.*)$" lab)
                     => (λ (x) (list-ref x 1))]
                    [(regexp-match #rx"^Hide (.*)$" lab)
                     => (λ (x) (list-ref x 1))]
                    [else lab]))))
    (define (cmp item-x item-y)
      (define x (get-key item-x))
      (define y (get-key item-y))
      (cond
        [(and (number? x) (number? y)) (< x y)]
        [(and (string? x) (string? y)) (string<=? x y)]
        [(and (number? x) (string? y)) #t]
        [(and (string? x) (number? y)) #f]))
    (define sorted-items (sort items cmp))
    
    (define (different-slots? item-key next-item-key)
      (or (not (= (quotient item-key 100)
                  (quotient next-item-key 100)))
          (not (= (sgn item-key)
                  (sgn next-item-key)))))
    
    (for ([item (in-list sorted-items)]
          [next-item (in-list (append (cdr sorted-items) (list #f)))])
      (define item-key (get-key item))
      (define next-item-key (and next-item (get-key next-item)))
      (define add-sep?
        (cond
          [(and (number? item-key) (number? next-item-key))
           (different-slots? item-key next-item-key)]
          [(or (and (string? item-key) (string? next-item-key))
               (not next-item-key))
           #f]
          [else #t]))
      (send item restore)  
      (when add-sep?
        (new separator-menu-item% [parent show-menu]))))
  
  
  (define (create-root-menubar)
    (define mb (new menu-bar% (parent 'root)))
    (define file-menu (new menu% 
                           (label (string-constant file-menu))
                           (parent mb)))
    (define help-menu (new menu% 
                           (label (string-constant help-menu))
                           (parent mb)))
    (new menu-item%
         (label (string-constant new-menu-item))
         (parent file-menu)
         (shortcut #\n)
         (callback
          (λ (x y)
            (handler:edit-file #f)
            #t)))
    (new menu-item%
         (label (string-constant open-menu-item))
         (parent file-menu)
         (shortcut #\o)
         (callback
          (λ (x y)
            (handler:open-file)
            #t)))
    (new menu%
         (label (string-constant open-recent-menu-item))
         (parent file-menu)
         (demand-callback
          (λ (menu)
            (handler:install-recent-items menu))))
    (new menu-item%
         [label (string-constant mfs-multi-file-search-menu-item)]
         [parent file-menu]
         [callback
          (λ (_1 _2)
            (drracket:multi-file-search:multi-file-search))])
    (unless (current-eventspace-has-standard-menus?)
      (new separator-menu-item% (parent file-menu))
      (new menu-item%
           (label (string-constant quit-menu-item-others))
           (parent file-menu)
           (shortcut #\q)
           (callback
            (λ (x y)
              (when (exit:user-oks-exit)
                (exit:exit))
              #t))))
    (make-help-desk-menu-item help-menu)
    (drracket-help-menu:after-about help-menu #f))
  
  (define (make-help-desk-menu-item help-menu)
    (define (docs-menu-item label)
      (new menu-item%
           [label label]
           [parent help-menu]
           [callback (λ (item evt) (help:help-desk) #t)]))
    (docs-menu-item (string-constant racket-documentation))
    (new separator-menu-item% [parent help-menu])
    (docs-menu-item (string-constant help-desk)))

  (define (drracket-help-menu:after-about menu dlg-parent)
    (drracket:app:add-important-urls-to-help-menu menu '())
    (new menu-item%
         [label (string-constant bug-report-submit-menu-item)]
         [parent menu]
         [callback
          (λ (x y)
            (define saved (saved-bug-report-titles/ids))
            (cond
              [(null? saved)
               (help-desk:report-bug #f #:frame-mixin basics-mixin)]
              [else
               (define which #f)
               (define (done the-one)
                 (set! which the-one)
                 (send dlg show #f))
               (define dlg (new dialog% 
                                [label (string-constant drscheme)]
                                [parent dlg-parent]))
               (define btn1 (new button% 
                                 [parent dlg]
                                 [label (string-constant new-bug-report)]
                                 [callback (λ (x y) (done #f))]))
               (new message% [parent dlg] [label (string-constant saved-unsubmitted-bug-reports)])
               (define btns
                 (cons btn1
                       (for/list ([a-brinfo (in-list saved)])
                         (new button%
                              [parent dlg]
                              [label (brinfo-title a-brinfo)]
                              [callback
                               (λ (x y) (done (brinfo-id a-brinfo)))]))))
               (define width (apply max (map (λ (x) (let-values ([(w h) (send x get-client-size)]) w))
                                             btns)))
               (for ([x (in-list btns)])
                 (send x min-width width))
               (send btn1 focus)
               (send dlg show #t)
               (help-desk:report-bug which #:frame-mixin basics-mixin)]))])
    (new menu%
         [label (string-constant saved-bug-reports-menu-item)]
         [parent menu]
         [demand-callback
          (let ([last-time (gensym)]) ;;  a unique thing to guarantee the menu is built the first time
            (λ (saved-bug-reports-menu)
              (define this-time (saved-bug-report-titles/ids))
              (unless (equal? last-time this-time)
                (set! last-time this-time)
                (for ([x (in-list (send saved-bug-reports-menu get-items))])
                  (send x delete))
                (cond
                  [(null? this-time)
                   (send (new menu-item%
                              [parent saved-bug-reports-menu]
                              [label (string-constant no-saved-bug-reports)]
                              [callback void])
                         enable #f)]
                  [else
                   (for ([a-brinfo (in-list this-time)])
                     (new menu-item%
                          [parent saved-bug-reports-menu]
                          [label (brinfo-title a-brinfo)]
                          [callback
                           (λ (x y)
                             (help-desk:report-bug (brinfo-id a-brinfo) #:frame-mixin basics-mixin))]))
                   (new separator-menu-item% [parent saved-bug-reports-menu])
                   (new menu-item%
                        [parent saved-bug-reports-menu]
                        [label (string-constant disacard-all-saved-bug-reports)]
                        [callback (λ (x y) (discard-all-saved-bug-reports))])]))))])
    (drracket:app:add-language-items-to-help-menu menu))
