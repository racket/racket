#lang racket/unit
  (require framework
           mzlib/class
           mred
           racket/file
           racket/path
           racket/list
           mzlib/thread
           mzlib/async-channel
           string-constants
           "drsig.rkt")
  
  (import [prefix drracket:frame: drracket:frame^]
          [prefix drracket:unit: drracket:unit^]
          [prefix drracket: drracket:interface^])
  (export drracket:multi-file-search^)
  
  ;; multi-file-search : -> void
  ;; opens a dialog to configure the search and initiates the search
  (define (multi-file-search)
    (let ([search-info  (configure-search)])
      (when search-info
        (open-search-window search-info))))
  
  ;; searcher = (string (string int int int -> void) -> void)
  ;; this performs a single search.
  ;; the first argument is the filename to be searched
  ;; the second argument is called for each match.
  ;;     the arguments are: line-string line-number col-number match-length
  
  
  ;; search-type = (make-search-type string make-searcher (listof (cons string boolean)))
  ;; the param strings are the labels for checkboxes
  ;; the param booleans are the default values for the checkboxes
  ;; these are the available searches
  (define-struct search-type (label make-searcher params))
  
  ;; search-info = (make-search-info string boolean (union #f regexp) search-type string)
  ;; the search-string field is only informative; not used for actual searching
  (define-struct search-info (dir recur? filter searcher search-string))
  
  ;; search-types : (listof search-type)
  (define search-types
    (list (make-search-type
           (string-constant mfs-string-match/graphics)
           (λ (info search-string) (exact-match-searcher info search-string))
           (list (cons (string-constant mfs-case-sensitive-label) #f)))
          (make-search-type
           (string-constant mfs-regexp-match/no-graphics)
           (λ (info search-string) (regexp-match-searcher info search-string))
           (list))))
  
  ;; search-entry = (make-search-entry string number number number)
  (define-struct search-entry (filename line-string line-number col-number match-length))
  
  ;; open-search-window : search-info -> void
  ;; thread: eventspace main thread
  ;; opens a window and creates the thread that does the search
  (define (open-search-window search-info)
    (define frame (new search-size-frame%
                       [name 
                        (let ([fmt-s (string-constant mfs-drscheme-multi-file-search-title)])
                          (format 
                           fmt-s
                           (gui-utils:trim-string (search-info-search-string search-info)
                                                  (- 200 (string-length fmt-s)))))]))
    (define panel (make-object saved-vertical-resizable% (send frame get-area-container)))
    (define button-panel (make-object horizontal-panel% (send frame get-area-container)))
    (define open-button (make-object button% (string-constant mfs-open-file) button-panel
                          (λ (x y) (open-file-callback))))
    (define stop-button (make-object button% (string-constant mfs-stop-search) button-panel
                          (λ (x y) (stop-callback))))
    (define grow-box-pane (make-object grow-box-spacer-pane% button-panel))
    
    (define zoom-text (make-object racket:text%))
    (define results-text (make-object results-text% zoom-text))
    (define results-ec (instantiate searching-canvas% ()
                         (parent panel)
                         (editor results-text)
                         (frame frame)))
    (define zoom-ec (instantiate searching-canvas% ()
                      (parent panel)
                      (editor zoom-text)
                      (frame frame)))
    
    (define (open-file-callback)
      (send results-text open-file))
    
    ;; sometimes, breaking the other thread puts
    ;; the break message in the channel behind
    ;; many many requests. Rather than show those,
    ;; we use the `broken?' flag as a shortcut.
    (define broken? #f)
    (define (stop-callback)
      (break-thread search-thd)
      (set! broken? #t)
      (send stop-button enable #f))
    
    ;; channel : async-channel[(union 'done search-entry)]
    (define channel (make-async-channel 100))
    (define search-thd (thread (λ () (do-search search-info channel))))
    
    (send frame set-text-to-search results-text) ;; just to initialize it to something.
    (send results-text lock #t)
    (send frame reflow-container)
    (send panel set-percentages (preferences:get 'drracket:multi-file-search:percentages))
    (send button-panel set-alignment 'right 'center)
    (send button-panel stretchable-height #f)
    (send frame show #t)
    
    (let loop ()
      (let ([match (yield channel)])
        (yield)
        (cond
          [(eq? match 'done)
           (send results-text search-complete)
           (send stop-button enable #f)]
          [(or broken? (eq? match 'break))
           (send results-text search-interrupted)]
          [else
           (send results-text add-match
                 (search-info-dir search-info)
                 (search-entry-filename match)
                 (search-entry-line-string match)
                 (search-entry-line-number match)
                 (search-entry-col-number match)
                 (search-entry-match-length match))
           (loop)]))))
  
  (define results-super-text% 
    (text:hide-caret/selection-mixin
     (text:line-spacing-mixin
      (text:basic-mixin
       (editor:standard-style-list-mixin 
        (editor:basic-mixin
         text%))))))
  
  ;; results-text% : derived from text%
  ;; init args: zoom-text
  ;;   zoom-text : (instance-of text%)
  ;; public-methods:
  ;;   add-match : string string int int int int -> void
  ;;      adds a match to the text
  ;;   search-interrupted : -> void
  ;;      inserts a message saying "search interrupted".
  ;;      search-complete is not expected to be called if this method is called.
  ;;   search-complete : -> void
  ;;      inserts a message saying "no matches found" if none were reported
  (define results-text%
    (class results-super-text%
      (init-field zoom-text)
      (inherit insert last-paragraph erase
               paragraph-start-position paragraph-end-position
               last-position change-style
               set-clickback set-position
               end-edit-sequence begin-edit-sequence
               lock)
      
      [define filename-delta (make-object style-delta% 'change-bold)]
      [define match-delta (let ([d (make-object style-delta%)])
                            (send d set-delta-foreground
                                  (make-object color%
                                    0
                                    160
                                    0))
                            d)]
      [define hilite-line-delta (make-object style-delta% 'change-style 'italic)]
      [define unhilite-line-delta (make-object style-delta% 'change-style 'normal)]
      [define widest-filename #f]
      [define/private indent-all-lines
        ;; indent-all-lines : number -> void
        ;; inserts `offset' spaces to the beginning of each line,
        ;; except the last one. Must be at least one such line in the text.
        (λ (offset)
          (let ([spaces (make-string offset #\space)])
            (let loop ([para (- (last-paragraph) 1)])
              (let ([para-start (paragraph-start-position para)])
                (insert spaces para-start para-start)
                (change-style filename-delta para-start (+ para-start offset)))
              (unless (zero? para)
                (loop (- para 1))))))]
      
      ;; match-shown? : boolean
      ;; indicates if a match has ever been shown.
      ;; if not, need to clean out the "searching" message
      ;; and show a match. Done in `add-match'
      [define match-shown? #f]
      
      ;; current-file : (union #f string)
      ;; the name of the currently viewed file, if one if viewed.
      ;; line-in-current-file and col-in-current-file are linked
      [define current-file #f]
      [define line-in-current-file #f]
      [define col-in-current-file #f]
      
      [define old-line #f]
      [define/private hilite-line
        (λ (line)
          (begin-edit-sequence)
          (lock #f)
          (when old-line
            (change-style unhilite-line-delta
                          (paragraph-start-position old-line)
                          (paragraph-end-position old-line)))
          (when line
            (change-style hilite-line-delta
                          (paragraph-start-position line)
                          (paragraph-end-position line)))
          (set! old-line line)
          (lock #t)
          (end-edit-sequence))]
      
      [define/public (open-file)
        (when current-file
          (let ([f (handler:edit-file current-file)])
            (when (and f
                       (is-a? f drracket:unit:frame<%>))
              (let* ([t (send f get-definitions-text)]
                     [pos (+ (send t paragraph-start-position line-in-current-file)
                             col-in-current-file)])
                (send t set-position pos)))))]
      
      [define/public add-match
        (λ (base-filename full-filename line-string line-number col-number match-length)
          (lock #f)
          (let* ([new-line-position (last-position)]
                 [short-filename 
                  (path->string
                   (find-relative-path 
                    (normalize-path base-filename)
                    (normalize-path full-filename)))]
                 [this-match-number (last-paragraph)]
                 [len (string-length short-filename)]
                 [insertion-start #f]
                 [show-this-match
                  (λ ()
                    (set! match-shown? #t)
                    (set! current-file full-filename)
                    (set! line-in-current-file line-number)
                    (set! col-in-current-file col-number)
                    (set-position new-line-position new-line-position)
                    (send zoom-text begin-edit-sequence)
                    (send zoom-text lock #f)
                    (unless (really-same-file? full-filename (send zoom-text get-filename))
                      (send zoom-text load-file/gui-error full-filename))
                    (send zoom-text set-position (send zoom-text paragraph-start-position line-number))
                    (let ([start (+ (send zoom-text paragraph-start-position line-number)
                                    col-number)])
                      (send zoom-text change-style match-delta start (+ start match-length)))
                    (send zoom-text lock #t)
                    (send zoom-text set-caret-owner #f 'global)
                    (hilite-line this-match-number)
                    (send zoom-text end-edit-sequence))])
            (unless match-shown?
              (erase))
            (unless widest-filename
              (set! widest-filename len))
            (if (<= len widest-filename)
                (begin
                  (set! insertion-start (last-position))
                  (insert (make-string (- widest-filename len) #\space) 
                          (last-position) (last-position)))
                (begin
                  (indent-all-lines (- len widest-filename))
                  (set! insertion-start (last-position))
                  (set! widest-filename len)))
            (let ([filename-start (last-position)])
              (insert short-filename (last-position) (last-position))
              (insert ": " (last-position) (last-position))
              (change-style filename-delta insertion-start (last-position))
              (let ([line-start (last-position)])
                (insert line-string (last-position) (last-position))
                (change-style match-delta
                              (+ line-start col-number)
                              (+ line-start col-number match-length)))
              (set-clickback filename-start (last-position)
                             (λ (_1 _2 _3)
                               (show-this-match)))
              (insert #\newline (last-position) (last-position))
              
              (unless match-shown?
                (show-this-match))))
          (lock #t))]
      
      (define/public (search-interrupted)
        (lock #f)
        (insert #\newline (last-position) (last-position))
        (insert (string-constant mfs-search-interrupted) (last-position) (last-position))
        (lock #t))
      
      (define/public (search-complete)
        (unless match-shown?
          (lock #f)
          (insert #\newline (last-position) (last-position))
          (insert (string-constant mfs-no-matches-found) (last-position) (last-position))
          (lock #t)))
      
      (inherit get-style-list set-style-list set-styles-sticky)
      (super-instantiate ())
      (send zoom-text lock #t)
      (set-styles-sticky #f)
      (insert (string-constant mfs-searching...))))
  
  (define (really-same-file? fn1 fn2)
    (define p1 (with-handlers ((exn:fail? (λ (x) #f))) (open-input-file fn1)))
    (define p2 (with-handlers ((exn:fail? (λ (x) #f))) (open-input-file fn2)))
    (cond
      [(and p1 p2)
       (begin0
         (= (port-file-identity p1) (port-file-identity p2))
         (close-input-port p1)
         (close-input-port p2))]
      [else
       (when p1 (close-input-port p1))
       (when p2 (close-input-port p2))
       #f]))
  
  ;; collaborates with search-size-frame%
  (define searching-canvas%
    (class canvas:basic%
      (init-field frame)
      (inherit get-editor)
      (define/override (on-focus on?)
        (when on?
          (send frame set-text-to-search (get-editor)))
        (super on-focus on?))
      (super-instantiate ())))
  
  ;; thread: eventspace main thread
  (define search-size-frame%
    (class (drracket:frame:basics-mixin 
            (frame:searchable-mixin
             frame:standard-menus%))
      (init-field name)
      (define/override (on-size w h)
        (preferences:set 'drracket:multi-file-search:frame-size (cons w h))
        (super on-size w h))
      (let ([size (preferences:get 'drracket:multi-file-search:frame-size)])
        (super-instantiate ()
          (label name)
          (width (car size))
          (height (cdr size))))))
  
  
  ;; this vertical-resizable class just remembers the percentage between the
  ;; two panels
  ;; thread: eventspace main thread
  (define saved-vertical-resizable%
    (class panel:vertical-dragable%
      (inherit get-percentages)
      (define/augment (after-percentage-change)
        (let ([ps (get-percentages)])
          (when (= (length ps) 2)
            (preferences:set 'drracket:multi-file-search:percentages ps)))
        (inner (void) after-percentage-change))
      (super-instantiate ())))
  
  ;; configure-search : -> (union #f search-info)
  ;; thread: eventspace main thread
  ;; configures the search
  (define (configure-search)
    (keymap:call/text-keymap-initializer
     (λ ()
       (define dialog (make-object dialog% (string-constant mfs-configure-search)
                        #f 500 #f #f #f '(resize-border)))
       (define outer-files-panel (make-object vertical-panel% dialog '(border)))
       (define outer-method-panel (make-object vertical-panel% dialog '(border)))
       (define button-panel (make-object horizontal-panel% dialog))
       (define files-label (make-object message% (string-constant mfs-files-section) outer-files-panel))
       (define files-inset-outer-panel (make-object horizontal-panel% outer-files-panel))
       (define files-inset-panel (make-object horizontal-panel% files-inset-outer-panel))
       (define files-panel (make-object vertical-panel% files-inset-outer-panel))
       (define method-label (make-object message% (string-constant mfs-search-section) outer-method-panel))
       (define method-inset-outer-panel (make-object horizontal-panel% outer-method-panel))
       (define method-inset-panel (make-object horizontal-panel% method-inset-outer-panel))
       (define method-panel (make-object vertical-panel% method-inset-outer-panel))
       
       (define dir-panel (make-object horizontal-panel% files-panel))
       (define dir-field 
         (new combo-field%
              [parent dir-panel] 
              [label (string-constant mfs-dir)] 
              [choices (preferences:get 'drracket:multi-file-search:directories)]
              [stretchable-width #t] 
              [stretchable-height #f]
              [callback (λ (x y) (dir-field-callback))]))

       (define dir-button (make-object button% (string-constant browse...) dir-panel 
                            (λ (x y) (dir-button-callback))))
       
       (define recur-check-box (make-object check-box% (string-constant mfs-recur-over-subdirectories) files-panel
                                 (λ (x y) (recur-check-box-callback))))
       
       (define filter-panel (make-object horizontal-panel% files-panel))
       (define filter-check-box (make-object check-box% (string-constant mfs-regexp-filename-filter) filter-panel
                                  (λ (x y) (filter-check-box-callback))))
       (define filter-text-field (make-object text-field% #f filter-panel 
                                   (λ (x y) (filter-text-field-callback))))
       
       (define methods-choice (make-object choice% #f (map search-type-label search-types) method-panel 
                                (λ (x y) (methods-choice-callback))))
       (define search-text-field (make-object text-field% (string-constant mfs-search-string) method-panel
                                   (λ (x y) (search-text-field-callback))))
       (define active-method-panel (make-object panel:single% method-panel))
       (define methods-check-boxess
         (let ([pref (preferences:get 'drracket:multi-file-search:search-check-boxes)])
           (map
            (λ (search-type prefs-settings)
              (let ([p (make-object vertical-panel% active-method-panel)]
                    [params (search-type-params search-type)])
                (send p set-alignment 'left 'center)
                (map (λ (flag-pair prefs-setting)
                       (let ([cb (make-object check-box% 
                                   (car flag-pair)
                                   p
                                   (λ (evt chk) (method-callback chk)))])
                         (send cb set-value prefs-setting)
                         cb))
                     params
                     (if (= (length params) (length prefs-settings))
                         prefs-settings
                         (map (λ (x) #f) params)))))
            search-types
            (if (= (length search-types) (length pref))
                pref
                (map (λ (x) '()) search-types)))))
       (define-values (ok-button cancel-button)
         (gui-utils:ok/cancel-buttons
          button-panel
          (λ (x y) (ok-button-callback))
          (λ (x y) (cancel-button-callback))))
       (define spacer (make-object grow-box-spacer-pane% button-panel))
       
       ;; initialized to a searcher during the ok button callback
       ;; so the user can be informed of an error before the dialog
       ;; closes.
       (define searcher #f)
       
       ;; initialized to a regexp if the user wants to filter filenames,
       ;; during the ok-button-callback, so errors can be signaled.
       (define filter #f)
       
       ;; title for message box that signals error messages
       (define message-box-title (string-constant mfs-drscheme-multi-file-search))
       
       (define (ok-button-callback)
         (cond
           [(with-handlers ([exn:fail:filesystem?
                             (λ (x) #f)])
              (directory-exists? (send dir-field get-value)))
            
            (let ([df (send dir-field get-value)])
              (when (path-string? df)
                (define new-l (cons df (remove df (preferences:get 'drracket:multi-file-search:directories))))
                (preferences:set 'drracket:multi-file-search:directories
                                 (take new-l (min (length new-l) 10)))))
            
            (let ([_searcher
                   ((search-type-make-searcher (list-ref search-types (send methods-choice get-selection)))
                    (map (λ (cb) (send cb get-value))
                         (send (send active-method-panel active-child) get-children))
                    (send search-text-field get-value))])
              (if (string? _searcher)
                  (message-box message-box-title _searcher dialog)
                  (let ([regexp (with-handlers ([(λ (x) #t)
                                                 (λ (exn)
                                                   (format "~a" (exn-message exn)))])
                                  (and (send filter-check-box get-value)
                                       (regexp (send filter-text-field get-value))))])
                    (if (string? regexp)
                        (message-box message-box-title regexp dialog)
                        (begin (set! searcher _searcher)
                               (set! filter regexp)
                               (set! ok? #t)
                               (send dialog show #f))))))]
           [else
            (message-box message-box-title
                         (format (string-constant mfs-not-a-dir) (send dir-field get-value))
                         dialog)]))
       (define (cancel-button-callback)
         (send dialog show #f))
       
       (define (method-callback chk)
         (preferences:set
          'drracket:multi-file-search:search-check-boxes
          (let loop ([methods-check-boxess methods-check-boxess])
            (cond
              [(null? methods-check-boxess) null]
              [else
               (cons (let loop ([methods-check-boxes (car methods-check-boxess)])
                       (cond
                         [(null? methods-check-boxes) null]
                         [else (cons (send (car methods-check-boxes) get-value)
                                     (loop (cdr methods-check-boxes)))]))
                     (loop (cdr methods-check-boxess)))]))))
       
       (define (dir-field-callback)
         (let ([df (send dir-field get-value)])
           (when (path-string? df)
             (preferences:set 'drracket:multi-file-search:directory df))))
       
       (define (filter-check-box-callback) 
         (preferences:set 'drracket:multi-file-search:filter? (send filter-check-box get-value))
         (send filter-text-field enable (send filter-check-box get-value)))
       (define (filter-text-field-callback)
         (preferences:set 'drracket:multi-file-search:filter-regexp (send filter-text-field get-value)))
       
       (define (recur-check-box-callback)
         (preferences:set 'drracket:multi-file-search:recur? (send recur-check-box get-value)))
       (define (methods-choice-callback)
         (define which (send methods-choice get-selection))
         (preferences:set 'drracket:multi-file-search:search-type which) 
         (set-method which))
       (define (set-method which)
         (send active-method-panel active-child
               (list-ref (send active-method-panel get-children)
                         which)))
       (define (search-text-field-callback)
         (preferences:set 'drracket:multi-file-search:search-string (send search-text-field get-value)))
       (define (dir-button-callback) 
         (define old-d (string->path (send dir-field get-value)))
         (define new-d (get-directory #f 
                                      #f 
                                      (and (directory-exists? old-d)
                                           old-d)))
         (when (and new-d
                    (directory-exists? new-d))
           (define str (path->string new-d))
           (preferences:set 'drracket:multi-file-search:directory str)
           (send dir-field set-value str)))
       
       (define (get-files)
         (let ([dir (string->path (send dir-field get-value))])
           (and (directory-exists? dir)
                (if (send recur-check-box get-value)
                    (build-recursive-file-list dir filter)
                    (build-flat-file-list dir filter)))))
       
       (define ok? #f)
       
       (send button-panel set-alignment 'right 'center)
       (send dir-panel stretchable-height #f)
       (send outer-files-panel stretchable-height #f)
       (send outer-files-panel set-alignment 'left 'center)
       (send files-inset-panel min-width 20)
       (send files-inset-panel stretchable-width #f)
       (send files-panel set-alignment 'left 'center)
       
       (send recur-check-box set-value (preferences:get 'drracket:multi-file-search:recur?))
       (send filter-check-box set-value (preferences:get 'drracket:multi-file-search:filter?))
       (send search-text-field set-value (preferences:get 'drracket:multi-file-search:search-string))
       (send filter-text-field set-value (preferences:get 'drracket:multi-file-search:filter-regexp))
       (send dir-field set-value (let ([p (preferences:get 'drracket:multi-file-search:directory)])
                                   (or p
                                       (let ([p (path->string (car (filesystem-root-list)))])
                                         (preferences:set 'drracket:multi-file-search:directory p)
                                         p))))
       
       (send outer-method-panel stretchable-height #f)
       (send outer-method-panel set-alignment 'left 'center)
       (send method-inset-panel min-width 20)
       (send method-inset-panel stretchable-width #f)
       (send method-panel set-alignment 'left 'center)
       (send filter-panel stretchable-height #f)
       
       (send methods-choice set-selection (preferences:get 'drracket:multi-file-search:search-type))
       (set-method (preferences:get 'drracket:multi-file-search:search-type)) 
       
       (send search-text-field focus)
       (let ([t (send search-text-field get-editor)])
         (send t set-position 0 (send t last-position)))
       (send dialog show #t)
       
       (and
        ok?
        (make-search-info
         (send dir-field get-value)
         (send recur-check-box get-value)
         (and (send filter-check-box get-value)
              (regexp (send filter-text-field get-value)))
         searcher
         (send search-text-field get-value))))))
  
  
  ;; do-search : search-info text -> void
  ;; thread: searching thread
  ;; called in a new thread that may be broken (to indicate a stop)
  (define (do-search search-info channel)
    (let* ([dir (search-info-dir search-info)]
           [filter (search-info-filter search-info)]
           [searcher (search-info-searcher search-info)]
           [get-filenames (if (search-info-recur? search-info)
                              (build-recursive-file-list dir filter)
                              (build-flat-file-list dir filter))])
      (with-handlers ([exn:break? (λ (x) (async-channel-put channel 'break))])
        (let loop ()
          (let ([filename (get-filenames)])
            (when filename
              (searcher filename
                        (λ (line-string line-number col-number match-length)
                          (async-channel-put
                           channel
                           (make-search-entry
                            filename
                            line-string
                            line-number
                            col-number
                            match-length))))
              (loop))))
        (async-channel-put channel 'done))))
  
  ;; build-recursive-file-list : string (union regexp #f) -> (-> (union string #f))
  ;; thread: search thread
  (define (build-recursive-file-list dir filter)
    (letrec ([touched (make-hash)]
             [next-thunk (λ () (process-dir dir (λ () #f)))]
             [process-dir
              ; string[dirname] (listof string[filename]) -> (listof string[filename])
              (λ (dir k)
                (let* ([key (normalize-path dir)]
                       [traversed? (hash-ref touched key (λ () #f))])
                  (if traversed? 
                      (k)
                      (begin
                        (hash-set! touched key #t)
                        (process-dir-contents 
                         (map (λ (x) (build-path dir x))
                              (directory-list dir))
                         k)))))]
             [process-dir-contents
              ; string[dirname] (listof string[filename]) -> (listof string[filename])
              (λ (contents k)
                (cond
                  [(null? contents) 
                   (k)]
                  [else 
                   (let ([file/dir (car contents)])
                     (cond
                       [(and (file-exists? file/dir)
                             (or (not filter)
                                 (regexp-match filter (path->string file/dir))))
                        (set! next-thunk
                              (λ ()
                                (process-dir-contents (cdr contents) k)))
                        file/dir]
                       [(directory-exists? file/dir)
                        (process-dir-contents 
                         (cdr contents)
                         (λ ()
                           (process-dir file/dir k)))]
                       [else 
                        (process-dir-contents (cdr contents) k)]))]))])
      (λ () (next-thunk))))
  
  ;; build-flat-file-list : path (union #f regexp) -> (-> (union string #f))
  ;; thread: searching thread
  (define (build-flat-file-list dir filter)
    (let ([contents (map (λ (x) (build-path dir x)) (directory-list dir))])
      (λ ()
        (let loop ()
          (cond
            [(null? contents)
             #f]
            [(and filter (regexp-match filter (path->string (car contents))))
             (begin0
               (car contents)
               (set! contents (cdr contents)))]
            [else
             (set! contents (cdr contents))
             (loop)])))))
  
  ;; exact-match-searcher : make-searcher
  (define (exact-match-searcher params key)        ;; thread: main eventspace thread
    (let ([case-sensitive? (car params)])
      (λ (filename add-entry)                 ;; thread: searching thread
        (let ([text (make-object text:line-spacing%)])
          (send text load-file filename)
          (let loop ([pos 0])
            (let ([found (send text find-string key 'forward pos 'eof #t case-sensitive?)])
              (when found
                (let* ([para (send text position-paragraph found)]
                       [para-start (send text paragraph-start-position para)]
                       [line-string (send text get-text para-start
                                          (send text paragraph-end-position para))]
                       [line-number para]
                       [col-number (- found para-start)]
                       [match-length (string-length key)])
                  (add-entry line-string line-number col-number match-length)
                  (loop (+ found 1))))))))))
  
  ;; regexp-match-searcher : make-searcher
  ;; thread: searching thread
  (define (regexp-match-searcher parmas key)       ;; thread: main eventspace thread
    (let ([re:key (with-handlers ([(λ (x) #t)
                                   (λ (exn)
                                     (format "~a" (exn-message exn)))])
                    (regexp key))])
      (if (string? re:key)
          re:key
          (λ (filename add-entry)             ;; thread: searching thread
            (call-with-input-file filename
              (λ (port)
                (let loop ([line-number 0])
                  (let ([line (read-line port)])
                    (cond
                      [(eof-object? line) (void)]
                      [else
                       (let ([match (regexp-match-positions re:key line)])
                         (when match
                           (let ([pos (car match)])
                             (add-entry line line-number 
                                        (car pos)
                                        (- (cdr pos) (car pos))))))
                       (loop (+ line-number 1))]))))
              #:mode 'text)))))
