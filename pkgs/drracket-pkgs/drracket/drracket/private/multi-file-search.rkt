#lang racket/base

(require framework
         racket/class
         racket/contract
         racket/unit
         racket/path
         racket/list
         racket/gui/base
         mzlib/async-channel
         string-constants
         drracket/private/drsig
         mrlib/close-icon
         "get-module-path.rkt")

(define sc-browse-collections "Browse\nCollections")
(define sc-add-another-directory "Add Another Directory")

(provide multi-file-search@)

;; search-type = (make-search-type string make-searcher (listof (cons string boolean)))
;; the param strings are the labels for checkboxes
;; the param booleans are the default values for the checkboxes
;; these are the available searches
(define-struct search-type (label make-searcher params) #:transparent)

;; search-info = (make-search-info (listof string) boolean (union #f regexp) search-type string)
;; the search-string field is only informative; not used for actual searching
(define-struct search-info (dirs recur? filter searcher search-string) #:transparent)

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

;; search-entry = (make-search-entry string string number number number)
(define-struct search-entry (base-dir filename line-string line-number col-number match-length) 
  #:transparent)

;; to make these available to be defined in the unit with the right names.
(define _search-types search-types)
(define _search-type-params search-type-params)

(define-unit multi-file-search@
  (import [prefix drracket:frame: drracket:frame^]
          [prefix drracket:unit: drracket:unit^]
          [prefix drracket: drracket:interface^])
  (export drracket:multi-file-search^)
  
  (define search-type-params _search-type-params)
  (define search-types _search-types)
  
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
    (define button-panel (new horizontal-panel%
                              [parent (send frame get-area-container)]
                              [alignment '(right center)]
                              [stretchable-height #f]))
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
                 (search-entry-base-dir match)
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
      
      (define/public (add-match base-filename full-filename line-string line col match-length)
        (lock #f)
        (define new-line-position (last-position))
        (define short-filename 
          (path->string
           (find-relative-path 
            (normalize-path base-filename)
            (normalize-path full-filename))))
        (define this-match-number (last-paragraph))
        (define len (string-length short-filename))
        (define insertion-start #f)
        (define (show-this-match)
          (set! match-shown? #t)
          (set! current-file full-filename)
          (set! line-in-current-file line)
          (set! col-in-current-file col)
          (set-position new-line-position new-line-position)
          (send zoom-text begin-edit-sequence)
          (send zoom-text lock #f)
          (unless (really-same-file? full-filename (send zoom-text get-filename))
            (send zoom-text load-file/gui-error full-filename))
          (send zoom-text set-position (send zoom-text paragraph-start-position line))
          (let ([start (+ (send zoom-text paragraph-start-position line)
                          col)])
            (send zoom-text change-style match-delta start (+ start match-length)))
          (send zoom-text lock #t)
          (send zoom-text set-caret-owner #f 'global)
          (hilite-line this-match-number)
          (send zoom-text end-edit-sequence))
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
                          (+ line-start col)
                          (+ line-start col match-length)))
          (set-clickback filename-start (last-position)
                         (λ (_1 _2 _3)
                           (show-this-match)))
          (insert #\newline (last-position) (last-position))
          
          (unless match-shown?
            (show-this-match)))
        (lock #t))
      
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
      (super-new)
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
      (super-new)))
  
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
        (super-new [label name]
                   [width (car size)]
                   [height (cdr size)]))))
  
  
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
      (super-new)))
  
  
  ;; do-search : search-info text -> void
  ;; thread: searching thread
  ;; called in a new thread that may be broken (to indicate a stop)
  (define (do-search search-info channel)
    (define filter (search-info-filter search-info))
    (define searcher (search-info-searcher search-info))
    (with-handlers ([exn:break? (λ (x) (async-channel-put channel 'break))])
      (for ([dir (in-list (search-info-dirs search-info))])
        (define get-filenames (if (search-info-recur? search-info)
                                  (build-recursive-file-list dir filter)
                                  (build-flat-file-list dir filter)))
        (let loop ()
          (let ([filename (get-filenames)])
            (when filename
              (searcher filename
                        (λ (line-string line-number col-number match-length)
                          (async-channel-put
                           channel
                           (make-search-entry
                            dir
                            filename
                            line-string
                            line-number
                            col-number
                            match-length))))
              (loop)))))
      (async-channel-put channel 'done)))
  
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
             (loop)]))))))

;; configure-search : -> (union #f search-info)
;; thread: eventspace main thread
;; configures the search
(define (configure-search)
  (keymap:call/text-keymap-initializer
   (λ ()
     (define dialog (new dialog%
                         [label (string-constant mfs-configure-search)]
                         [width 500]
                         [style '(resize-border)]
                         [stretchable-height #f]))
     (define outer-files-panel (make-object vertical-panel% dialog '(border)))
     (define outer-method-panel (make-object vertical-panel% dialog '(border)))
     (define button-panel (new horizontal-panel% 
                               [parent dialog]
                               [alignment '(right center)]
                               [stretchable-height #f]))
     (define files-label (make-object message% (string-constant mfs-files-section) 
                           outer-files-panel))
     (define files-inset-outer-panel (make-object horizontal-panel% outer-files-panel))
     (define files-inset-panel (make-object horizontal-panel% files-inset-outer-panel))
     (define files-panel (make-object vertical-panel% files-inset-outer-panel))
     (define method-label (make-object message% (string-constant mfs-search-section) 
                            outer-method-panel))
     (define method-inset-outer-panel (make-object horizontal-panel% outer-method-panel))
     (define method-inset-panel (make-object horizontal-panel% method-inset-outer-panel))
     (define method-panel (make-object vertical-panel% method-inset-outer-panel))
     
     (define multi-dir+browse-collections-panel (new horizontal-panel% 
                                                     [alignment '(center top)]
                                                     [stretchable-height #f]
                                                     [parent files-panel]))
     (define multi-dir-panel (new vertical-panel% [parent multi-dir+browse-collections-panel]))
     (define dir-fields '())
     (define (add-a-dir-field init-value)
       (send dialog begin-container-sequence)
       (define need-to-add-closers? (and (pair? dir-fields) (null? (cdr dir-fields))))
       (define dir-panel (new horizontal-panel%
                              [parent multi-dir-panel]
                              [stretchable-height #f]))
       (define dir-field
         (new combo-field%
              [parent dir-panel] 
              [label (string-constant mfs-dir)] 
              [choices (preferences:get 'drracket:multi-file-search:directories)]
              [init-value init-value]
              [stretchable-width #t] 
              [stretchable-height #f]
              [callback (λ (x y) (update-directory-prefs))]))
     
       (define dir-button (new button% 
                               [label (string-constant browse...)]
                               [parent dir-panel]
                               [callback (λ (x y) (dir-button-callback))]))
       (define (dir-button-callback) 
         (define old-d (string->path (send dir-field get-value)))
         (define new-d (get-directory #f 
                                      #f 
                                      (and (directory-exists? old-d)
                                           old-d)))
         (when (and new-d
                    (directory-exists? new-d))
           (define str (path->string new-d))
           (send dir-field set-value str)
           (update-directory-prefs)))
       (set! dir-fields (cons dir-field dir-fields))
       (update-directory-prefs)
       (cond
         [(null? (cdr dir-fields))
          ;; len=1 : add to none of them
          (void)]
         [(null? (cddr dir-fields))
          ;; len=2 : add to all of them
          (for ([dir-panel (in-list (send multi-dir-panel get-children))])
            (add-a-closer dir-panel))]
         [else
          ;; len>2 : add to this one
          (add-a-closer dir-panel)])
       (send dialog end-container-sequence))
       
     (define (add-a-closer dir-panel)
       (define ci
         (new close-icon%
              [parent dir-panel]
              [callback
               (λ () (remove-a-dir-panel dir-panel))]))
       (send dir-panel change-children
             (λ (l)
               (append (remove ci l) (list ci)))))
     
     (define (remove-a-dir-panel dir-panel)
       (define dir-field (for/or ([child (in-list (send dir-panel get-children))])
                           (and (is-a? child combo-field%)
                                child)))
       (set! dir-fields (remove dir-field dir-fields))
       (send multi-dir-panel change-children (λ (l) (remove dir-panel l)))
       (update-directory-prefs)
       (when (and (pair? dir-fields)
                  (null? (cdr dir-fields)))
         ;; only one dir field left, get rid of the close-icon
         (let loop ([parent multi-dir-panel])
           (for ([child (in-list (send parent get-children))])
             (cond
               [(is-a? child close-icon%) 
                (send parent change-children (λ (l) (remove child l)))]
               [(is-a? child area-container<%>) (loop child)])))))
     
     (define (update-directory-prefs)
       (define new-pref
         (for/list ([dir-field (in-list dir-fields)])
           (define dfv (send dir-field get-value))
           (and (path-string? dfv) dfv)))
       (when (andmap values new-pref)
         (preferences:set 'drracket:multi-file-search:directory new-pref)))
     
     (define browse-collections-button 
       (new button%
            [label sc-browse-collections]
            [parent multi-dir+browse-collections-panel]
            [callback (λ (x y) 
                        (define paths (get-module-path-from-user #:dir? #t))
                        (when paths
                          (define delta-dirs (- (length dir-fields) (length paths)))
                          (cond
                            [(< delta-dirs 0)
                             (for ([x (in-range (- delta-dirs))])
                               (add-a-dir-field ""))]
                            [(> delta-dirs 0)
                             (for ([x (in-range delta-dirs)]
                                   [dir-field (in-list (send multi-dir-panel get-children))])
                               (remove-a-dir-panel dir-field))])
                          (for ([path (in-list paths)]
                                [dir-field (in-list dir-fields)])
                            (send dir-field set-value (path->string path)))
                          (update-directory-prefs)))]))
     (define recur+another-parent (new horizontal-panel%
                                       [parent files-panel]
                                       [stretchable-height #f]))
     (define recur-check-box (new check-box% 
                                  [label (string-constant mfs-recur-over-subdirectories)]
                                  [parent recur+another-parent]
                                  [callback (λ (x y) (recur-check-box-callback))]))
     (new horizontal-panel% [parent recur+another-parent]) ;; spacer
     (define another-dir-button (new button%
                                     [label sc-add-another-directory]
                                     [parent recur+another-parent]
                                     [callback (λ (x y) (add-a-dir-field ""))]))
     
     (define filter-panel (make-object horizontal-panel% files-panel))
     (define filter-check-box (make-object check-box% 
                                (string-constant mfs-regexp-filename-filter) 
                                filter-panel
                                (λ (x y) (filter-check-box-callback))))
     (define filter-text-field (make-object text-field% #f filter-panel 
                                 (λ (x y) (filter-text-field-callback))))
     
     (define methods-choice (make-object choice%
                              #f (map search-type-label search-types) method-panel 
                              (λ (x y) (methods-choice-callback))))
     (define search-text-field (make-object text-field% 
                                 (string-constant mfs-search-string) method-panel
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
       (define dirs (for/list ([df (in-list dir-fields)])
                      (send df get-value)))
       (define dont-exist
         (for/list ([dir (in-list dirs)]
                    #:unless 
                    (with-handlers ([exn:fail:filesystem? (λ (x) #f)])
                      (and (path-string? dir)
                           (directory-exists? dir))))
           dir))
         
       (cond
         [(null? dont-exist)
          (define new-l (append dirs
                                (remove* dirs
                                         (preferences:get
                                          'drracket:multi-file-search:directories))))
          (preferences:set 'drracket:multi-file-search:directories
                           (take new-l (min (length new-l) 10)))
          
          (define _searcher
            ((search-type-make-searcher (list-ref search-types (send methods-choice get-selection)))
             (map (λ (cb) (send cb get-value))
                  (send (send active-method-panel active-child) get-children))
             (send search-text-field get-value)))
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
                           (send dialog show #f)))))]
         [else
          (message-box message-box-title
                       (format (string-constant mfs-not-a-dir)
                               (car dont-exist))
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
     
     (define (filter-check-box-callback) 
       (preferences:set 'drracket:multi-file-search:filter? (send filter-check-box get-value))
       (send filter-text-field enable (send filter-check-box get-value)))
     (define (filter-text-field-callback)
       (preferences:set 'drracket:multi-file-search:filter-regexp
                        (send filter-text-field get-value)))
     
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
       (preferences:set 'drracket:multi-file-search:search-string
                        (send search-text-field get-value)))
     
     (define ok? #f)
     
     (send outer-files-panel stretchable-height #f)
     (send outer-files-panel set-alignment 'left 'center)
     (send files-inset-panel min-width 20)
     (send files-inset-panel stretchable-width #f)
     (send files-panel set-alignment 'left 'center)
     
     (send recur-check-box set-value (preferences:get 'drracket:multi-file-search:recur?))
     (send filter-check-box set-value (preferences:get 'drracket:multi-file-search:filter?))
     (send search-text-field set-value (preferences:get 'drracket:multi-file-search:search-string))
     (send filter-text-field set-value (preferences:get 'drracket:multi-file-search:filter-regexp))
     (for ([pth/f (in-list (preferences:get 'drracket:multi-file-search:directory))])
       (define pth (or pth/f (path->string (car (filesystem-root-list)))))
       (add-a-dir-field pth))
     
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
       (for/list ([dir-field (in-list dir-fields)])
         (send dir-field get-value))
       (send recur-check-box get-value)
       (and (send filter-check-box get-value)
            (regexp (send filter-text-field get-value)))
       searcher
       (send search-text-field get-value))))))





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


(preferences:set-default 'drracket:multi-file-search:directories 
                         '()
                         (lambda (x) (and (list? x) (andmap string? x))))
;; drracket:mult-file-search:search-check-boxes : (listof (listof boolean))
(preferences:set-default 'drracket:multi-file-search:search-check-boxes 
                         (map (λ (x) (map cdr (search-type-params x))) search-types)
                         (listof (listof boolean?)))
(preferences:set-default 'drracket:multi-file-search:recur? #t boolean?)
(preferences:set-default 'drracket:multi-file-search:filter? #t boolean?)
(preferences:set-default 'drracket:multi-file-search:filter-regexp
                         "\\.(rkt[^~]?|scrbl|ss|scm)$" string?)
(preferences:set-default 'drracket:multi-file-search:search-string "" string?)
(preferences:set-default 'drracket:multi-file-search:search-type
                         1
                         (λ (x) 
                           (and (exact-nonnegative-integer? x)
                                (< x (length search-types)))))

(preferences:set-default 'drracket:multi-file-search:percentages
                         '(1/3 2/3)
                         (and/c (listof (between/c 0 1))
                                (λ (x) (= 1 (apply + x)))))

(preferences:set-default 'drracket:multi-file-search:frame-size '(300 . 400) 
                         (cons/c dimension-integer? dimension-integer?))
(preferences:set-default 'drracket:multi-file-search:directory 
                         ;; #f means the filesystem root, but that's
                         ;; expensive to compute under windows so we
                         ;; delay the computation until the dialog
                         ;; is opened.
                         '(#f)
                         (non-empty-listof (or/c #f (and/c string? path-string?))))


(module+ main (configure-search))
