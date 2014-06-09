#lang racket/base
(require racket/unit
         mrlib/hierlist
         racket/class
         racket/contract
         racket/string
         racket/list
         racket/gui/base
         drracket/private/drsig
         "tooltip.rkt"
         "local-member-names.rkt"
         string-constants
         framework
         setup/getinfo
         setup/xref
         scribble/xref
         scribble/tag
         net/url
         syntax/toplevel
         browser/external
         (only-in mzlib/struct make-->vector)

         ;; ensure that this module is always loaded since it is shared below for pretty big
         (prefix-in : mred/mred))

  (define original-output (current-output-port))
  (define (oprintf . args) (apply fprintf original-output args))
  
  (define-values (sc-use-language-in-source 
                  sc-use-teaching-language
                  sc-choose-a-language
                  mouse-event-uses-shortcut-prefix?)
    (let* ([shortcut-prefix (get-default-shortcut-prefix)]
           [menukey-string 
            (apply string-append
                   (map (λ (x)
                          (case x
                            [(cmd) "⌘"]
                            [else (format "~a-" x)]))
                        shortcut-prefix))])
      (define (mouse-event-uses-shortcut-prefix? evt)
        (andmap (λ (prefix)
                  (case prefix
                    [(alt) (case (system-type)
                             [(windows) (send evt get-meta-down)]
                             [else (send evt get-alt-down)])]
                    [(cmd) (send evt get-meta-down)]
                    [(meta) (send evt get-meta-down)]
                    [(ctl) (send evt get-control-down)]
                    [(shift) (send evt get-shiftdown)]
                    [(option) (send evt get-alt-down)]))
                shortcut-prefix))
      (values (string-append (string-constant the-racket-language)
                             (format " (~aR)" menukey-string))
              (string-append (string-constant teaching-languages)
                             (format " (~aT)" menukey-string))
              (string-append (string-constant other-languages)
                             (format " (~aO)" menukey-string))
              mouse-event-uses-shortcut-prefix?)))
  
  (provide language-configuration@)
  
  (define-unit language-configuration@
    (import [prefix drracket:unit: drracket:unit^]
            [prefix drracket:rep: drracket:rep^]
            [prefix drracket:init: drracket:init^]
            [prefix drracket:language: drracket:language^]
            [prefix drracket:app: drracket:app^]
            [prefix drracket:tools: drracket:tools^]
            [prefix drracket:help-desk: drracket:help-desk^]
            [prefix drracket:module-language: drracket:module-language/int^]
            [prefix drracket: drracket:interface^])
    (export drracket:language-configuration/internal^)
    
    (define struct:language-settings struct:drracket:language-configuration:language-settings)
    (define language-settings? drracket:language-configuration:language-settings?)
    (define language-settings-language drracket:language-configuration:language-settings-language)
    (define language-settings-settings drracket:language-configuration:language-settings-settings)
    (define make-language-settings drracket:language-configuration:language-settings)
    (define language-settings make-language-settings)
    
    ;; settings-preferences-symbol : symbol
    ;; this pref used to depend on `version', but no longer does.
    (define settings-preferences-symbol 'drracket:language-settings)
    
    ;; get-settings-preferences-symbol : -> symbol
    (define (get-settings-preferences-symbol) settings-preferences-symbol)
    
    ;; default-language-position : (listof string)
    ;; if a language is registered with this position, it is
    ;; considered the default language
    (define initial-language-position
      (list (string-constant initial-language-category)
            (string-constant no-language-chosen)))
    
    ;; languages : (listof (instanceof language<%>))
    ;; all of the languages supported in DrRacket
    (define languages null)
    
    (define languages-allowing-executable-creation '())
    (define (language-allows-executable-creation? candidate-lang)
      (define candidates-positions (send candidate-lang get-language-position))
      (for/or ([allowed-lang (in-list languages-allowing-executable-creation)])
        (equal? (send allowed-lang get-language-position)
                candidates-positions)))
    
    ;; add-language : (instanceof language%) -> void
    ;; only allows addition on phase2
    ;; effect: updates `languages'
    (define (add-language language [front? #f]
                          #:allow-executable-creation? [allow-executable-creation? #f])
      
      (drracket:tools:only-in-phase 'drracket:language:add-language 'phase2)
      (for-each
       (λ (i<%>)
         (unless (is-a? language i<%>)
           (error 'drracket:language:add-language
                  (string-append
                   "expected language ~e to implement ~e,"
                   " forgot to use `drracket:language:get-default-mixin'?")
                  language i<%>)))
       (drracket:language:get-language-extensions))
      
      (ensure-no-duplicate-numbers language languages)
      (when allow-executable-creation?
        (set! languages-allowing-executable-creation
              (cons language languages-allowing-executable-creation)))
      (set! languages 
            (if front? 
                (cons language languages)
                (append languages (list language)))))
    
    (define (ensure-no-duplicate-numbers l1 languages)
      (for-each
       (λ (l2)
         (when (equal? (send l1 get-language-numbers)
                       (send l2 get-language-numbers))
           (error 'drracket:language-configuration:add-language
                  "found two languages with the same result from get-language-numbers: ~s, ~s and ~s"
                  (send l1 get-language-numbers)
                  (send l1 get-language-position)
                  (send l2 get-language-position))))
       languages))
    
    ;; get-languages : -> (listof languages)
    (define (get-languages) 
      (drracket:tools:only-in-phase
       'drracket:language-configuration:get-languages
       'init-complete)
      languages)
    
    ;; get-default-language-settings : -> language-settings
    ;; uses `default-language-position' to find the default language.
    ;; if that language is not available, just takes the first language.
    ;; if there are no languages defined yet, signal an error -- drscheme is in trouble.
    (define (get-default-language-settings)
      (when (null? languages)
        (error 'get-default-language-settings "no languages registered!"))
      (let ([lang (or (ormap (λ (x)
                               (and (equal? (send x get-language-position)
                                            initial-language-position)
                                    x))
                             (get-languages))
                      (list-ref (get-languages) 0))])
        (language-settings lang (send lang default-settings))))
    
    
    

;                                                              
;                                                              
;                                                              
;                                                              
;  ;;;                                                         
;  ;;;                                                         
;  ;;;  ;;;;;  ;;; ;;   ;; ;;; ;;; ;;;  ;;;;;   ;; ;;;   ;;;;  
;  ;;; ;;;;;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;;;;;; ;;;;;;;  ;; ;;; 
;  ;;; ;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;;   ;;;;; ;;; ;;; ;;; ;;; ;;; ;;;   ;;;;; ;;; ;;; ;;;;;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;     
;  ;;; ;;; ;;; ;;; ;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;;;;;;  ;;;;;; 
;  ;;;  ;;;;;; ;;; ;;;  ;; ;;;  ;; ;;;  ;;;;;;  ;; ;;;   ;;;;  
;                          ;;;                     ;;;         
;                      ;;;;;;                  ;;;;;;          
;                                                              
;                                                              
;                                          
;                                          
;                                          
;                                          
;      ;;; ;;;         ;;;                 
;      ;;;             ;;;                 
;   ;; ;;; ;;;  ;;;;;  ;;;   ;;;    ;; ;;; 
;  ;;;;;;; ;;; ;;;;;;; ;;;  ;;;;;  ;;;;;;; 
;  ;;; ;;; ;;; ;;  ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;;   ;;;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;;;;;; ;;; ;;; ;;; ;;;  ;;;;;  ;;;;;;; 
;   ;; ;;; ;;;  ;;;;;; ;;;   ;;;    ;; ;;; 
;                                      ;;; 
;                                  ;;;;;;  
;                                          
;                                          
    
    
    ;; language-dialog : (boolean language-setting -> (union #f language-setting))
    ;;                   (boolean language-setting (union #f (instanceof top-level-window%))
    ;;                    -> 
    ;;                    (union #f language-setting))
    ;; allows the user to configure their language. The input language-setting is used
    ;; as the defaults in the dialog and the output language setting is the user's choice
    ;; todo: when button is clicked, ensure language is selected
    (define language-dialog
      (λ (show-welcome? language-settings-to-show [parent #f])
        (define ret-dialog%
          (class (frame:focus-table-mixin dialog%)
            (define/override (on-subwindow-char receiver evt)
              (case (send evt get-key-code)
                [(escape) (cancel-callback)]
                [(#\return numpad-enter) (enter-callback)]
                [else
                 (or (key-pressed receiver evt)
                     (super on-subwindow-char receiver evt))]))
            (super-new)))
        
        (define dialog (instantiate ret-dialog% ()
                         (label (if show-welcome?
                                    (string-constant welcome-to-drscheme)
                                    (string-constant language-dialog-title)))
                         (parent parent)
                         (style '(resize-border))))
        (define welcome-before-panel (instantiate horizontal-pane% ()
                                       (parent dialog)
                                       (stretchable-height #f)))
        (define language-dialog-meat-panel (make-object vertical-pane% dialog))
        
        (define welcome-after-panel (instantiate vertical-pane% () 
                                      (parent dialog)
                                      (stretchable-height #f)))
        
        (define button-panel (instantiate horizontal-pane% ()
                               (parent dialog)
                               (stretchable-height #f)))
        
        ;; initialized below
        (define ok-button #f)
        (define cancel-button #f)
        
        ;; cancelled? : boolean
        ;; flag that indicates if the dialog was cancelled.
        (define cancelled? #t)
        
        ;; enter-callback : -> bool
        ;; returns #f if no language is selected (so the event will be
        ;; processed by the hierlist widget, which will toggle subtrees)
        (define (enter-callback)
          (cond [(get-selected-language)
                 (set! cancelled? #f)
                 (send dialog show #f)
                 #t]
                [else #f]))
        
        ;; ok-callback : -> void
        ;; similar to the above, but shows an error dialog if no language os
        ;; selected
        (define (ok-callback)
          (unless (enter-callback)
            (message-box (string-constant drscheme)
                         (string-constant please-select-a-language)
                         #:dialog-mixin frame:focus-table-mixin)))
        
        ;; cancel-callback : -> void
        (define (cancel-callback)
          (send dialog show #f))
        
        ;; a handler for "ok"-related stuff
        (define ok-handler
          ;; this is called before the buttons are made: keep track of state
          ;; in that case
          (let ([enabled? #t])
            (define (enable! state)
              (set! enabled? state)
              (when ok-button (send ok-button enable state)))
            (λ (msg)
              (case msg
                [(disable)     (enable! #f)]
                [(enable)      (enable! #t)]
                [(enable-sync) (enable! enabled?)]
                [(execute)     (enter-callback) (void)]
                [else (error 'ok-handler "internal error (~e)" msg)]))))
        
        (define-values (get-selected-language get-selected-language-settings key-pressed)
          (fill-language-dialog language-dialog-meat-panel
                                button-panel
                                language-settings-to-show
                                #f
                                ok-handler
                                (and (is-a? parent drracket:unit:frame<%>)
                                     (send parent get-definitions-text))))
        
        ;; create ok/cancel buttons
        (make-object horizontal-pane% button-panel)
        (set!-values (ok-button cancel-button)
                     (gui-utils:ok/cancel-buttons button-panel
                                                  (λ (x y) (ok-callback))
                                                  (λ (x y) (cancel-callback))))
        (ok-handler 'enable-sync) ; sync enable status now
        (make-object grow-box-spacer-pane% button-panel)
        
        (when show-welcome?
          (add-welcome dialog welcome-before-panel welcome-after-panel))
        
        (send dialog stretchable-width #f)
        (send dialog stretchable-height #f)
        
        (unless parent
          (send dialog center 'both))
        (send dialog show #t)
        (if cancelled?
            #f
            (language-settings
             (get-selected-language)
             (get-selected-language-settings)))))
    
    ;; fill-language-dialog :
    ;;    (vertical-panel panel language-setting -> language-setting)
    ;;    (union dialog #f) [...more stuff...]
    ;; -> (-> (union #f language<%>)) (-> settings[corresponding to fst thnk result])
    ;; allows the user to configure their language. The input language-setting is used
    ;; as the defaults in the dialog and the output language setting is the user's choice
    ;; if re-center is a dialog, when the show details button is clicked, the dialog is recenterd.
    (define fill-language-dialog
      (λ (parent show-details-parent language-settings-to-show
                 [re-center #f]
                 [ok-handler void]
                 [definitions-text #f]) ; en/disable button, execute it
        
        (define-values (language-to-show settings-to-show)
          (let ([request-lang-to-show (language-settings-language language-settings-to-show)])
            (cond
              [(equal? initial-language-position (send request-lang-to-show get-language-position))
               (values (list-ref (get-languages) 0)
                       (send (list-ref (get-languages) 0) default-settings))
               (values #f #f)]
              [else (values request-lang-to-show
                            (language-settings-settings language-settings-to-show))])))
        
        ;; hier-list items that implement this interface correspond to
        ;; actual language selections
        (define hieritem-language<%>
          (interface (hierarchical-list-item<%>)
            get-language
            selected))
        
        (define selectable-hierlist%
          (class hierarchical-list%
            (init parent)
            
            (inherit get-selected
                     client->screen
                     get-editor)
            (define/override (on-char evt)
              (let ([code (send evt get-key-code)])
                (case code
                  [(up)   (select-next sub1)]
                  [(down) (select-next add1)]
                  ;; right key is fine, but nicer to close after a left
                  [(left) (super on-char evt)
                          (cond [(get-selected)
                                 => (λ (i)
                                      (when (is-a? i hierarchical-list-compound-item<%>)
                                        (send i close)))])]
                  [else (super on-char evt)])))
            
            (inherit get-items)
            
            ;; select-next : (num -> num) -> void
            ;; finds the next/prev leaf after the selected child on the open
            ;; fringe using `inc' for a direction.
            (define/private (select-next inc)
              (define current (get-selected))
              (define (choose item)
                (when current (send current select #f))
                (send item select #t)
                ;; make it visible
                (let loop ([item item])
                  (let ([parent (send item get-parent)])
                    (if parent
                        (loop parent)
                        (send item scroll-to))))
                (send item scroll-to))
              (define (selectable? item)
                (and (send item get-allow-selection?)
                     ;; opened all the way to the top
                     (let loop ([p (send item get-parent)])
                       (or (not p)
                           (and (send p is-open?)
                                (loop (send p get-parent)))))))
              (let* ([fringe     (get-fringe)]
                     [fringe-len (vector-length fringe)]
                     [n (if current
                            (let loop ([i (sub1 (vector-length fringe))])
                              (cond [(< i 0) (error 'select-next "item not found in fringe")]
                                    [(eq? current (vector-ref fringe i))
                                     (min (sub1 fringe-len) (max 0 (inc i)))]
                                    [else (loop (sub1 i))]))
                            (modulo (inc fringe-len) (add1 fringe-len)))])
                ;; need to choose item n, but go on looking for one that is
                ;; selectable and open
                (let loop ([n n])
                  (when (< -1 n fringe-len)
                    (let ([item (vector-ref fringe n)])
                      (if (selectable? item)
                          (choose item)
                          (loop (inc n))))))))
            
            (define cached-fringe #f)
            (define/public (clear-fringe-cache) (set! cached-fringe #f))
            (define (get-fringe)
              (unless cached-fringe
                (let ([fringe
                       (let loop ([items (get-items)])
                         (apply append
                                (map (λ (item)
                                       (if (is-a? item hierarchical-list-compound-item<%>)
                                           (cons item
                                                 (loop (send item get-items)))
                                           (list item)))
                                     items)))])
                  (set! cached-fringe (list->vector fringe))))
              cached-fringe)
            
            (define/override (on-select i)
              (cond
                [(and i (is-a? i hieritem-language<%>))
                 (define pos (send (send i get-language) get-language-position))
                 (if (eq? this teaching-languages-hier-list)
                     (preferences:set 'drracket:language-dialog:teaching-hierlist-default pos)
                     (preferences:set 'drracket:language-dialog:hierlist-default pos))
                 (if (eq? this teaching-languages-hier-list)
                     (set! most-recent-teaching-languages-hier-list-selection pos)
                     (set! most-recent-languages-hier-list-selection pos))
                 (something-selected this i)]
                [else
                 (non-language-selected)]))
            ;; this is used only because we set `on-click-always'
            (define/override (on-click i)
              (when (and i (is-a? i hierarchical-list-compound-item<%>))
                (send i toggle-open/closed)))
            ;; double-click selects a language
            (define/override (on-double-select i)
              (when (and i (is-a? i hieritem-language<%>))
                (something-selected this i)
                (ok-handler 'execute)))
            
            (define tooltip-timer 
              (new timer%
                   [notify-callback (λ () (show-tooltip))]))
            (define tooltip-frame #f)
            (define hieritem-language-to-show-in-tooltip #f)
            (define hieritem-tooltip-x #f)
            (define hieritem-tooltip-y #f)
            (define hieritem-tooltip-w #f)
            (define hieritem-tooltip-h #f)
            (define/override (on-event evt)
              (super on-event evt)
              (cond
                [(or (send evt entering?)
                     (send evt moving?))
                 (define-values (ex ey) (send (get-editor) dc-location-to-editor-location
                                              (send evt get-x) 
                                              (send evt get-y)))
                 (define-values (_to-show-in-tooltip _x _y _w _h)
                   (find-snip ex ey))
                 (unless (equal? _to-show-in-tooltip
                                 hieritem-language-to-show-in-tooltip)
                   (set! hieritem-language-to-show-in-tooltip _to-show-in-tooltip)
                   (set! hieritem-tooltip-x _x)
                   (set! hieritem-tooltip-y _y)
                   (set! hieritem-tooltip-w _w)
                   (set! hieritem-tooltip-h _h)
                   (when tooltip-frame (send tooltip-frame show #f))
                   (send tooltip-timer stop)
                   (when hieritem-language-to-show-in-tooltip (send tooltip-timer start 200 #t)))]
                [(send evt leaving?)
                 (set! hieritem-language-to-show-in-tooltip #f)
                 (send tooltip-timer stop)]))
            (define bl (box 0))
            (define bt (box 0))
            (define br (box 0))
            (define bb (box 0))
            (define/private (find-snip x y)
              (let loop ([snip (send (get-editor) find-first-snip)]
                         [editor (get-editor)]
                         [x x]
                         [y y])
                (cond
                  [(not snip) (values #f #f #f #f #f)]
                  [else 
                   (send editor get-snip-location snip bl bt #f)
                   (send editor get-snip-location snip br bb #t)
                   (cond
                     [(and (is-a? snip hierarchical-item-snip%)
                           (is-a? (send snip get-item) hieritem-language<%>)
                           (<= (unbox bl) x (unbox br))
                           (<= (unbox bt) y (unbox bb)))
                      (define w (- (unbox br) (unbox bl)))
                      (define h (- (unbox bb) (unbox bt)))
                      (send editor local-to-global bl bt)
                      (define-values (x y) (client->screen
                                            (inexact->exact (round (unbox bl)))
                                            (inexact->exact (round (unbox bt)))))
                      (define-values (dl dt) (get-display-left-top-inset))
                      (values (send snip get-item)
                              (- x dl)
                              (- y dt)
                              (inexact->exact (round w))
                              (inexact->exact (round h)))]
                     [(is-a? snip editor-snip%)
                      (define-values (es ex ey ew eh)
                        (loop (send (send snip get-editor) find-first-snip)
                              (send snip get-editor)
                              (- x (unbox bl))
                              (- y (unbox bt))))
                      (if es
                          (values es ex ey ew eh)
                          (loop (send snip next) editor x y))]
                     [else
                      (loop (send snip next) editor x y)])])))
            (define/private (show-tooltip)
              (when hieritem-language-to-show-in-tooltip
                (define msg (send (send hieritem-language-to-show-in-tooltip get-language)
                                  get-one-line-summary))
                (when msg
                  (unless tooltip-frame
                    (set! tooltip-frame (new tooltip-frame%
                                             [frame-to-track
                                              (let loop ([w this])
                                                (cond
                                                  [(is-a? w top-level-window<%>)
                                                   w]
                                                  [(is-a? w area<%>)
                                                   (loop (send w get-parent))]
                                                  [else #f]))])))
                  (send tooltip-frame set-tooltip (list msg))
                  (send tooltip-frame show-over 
                        (+ hieritem-tooltip-x hieritem-tooltip-w 4)
                        hieritem-tooltip-y
                        0 
                        0))))
                 
            (define/public (hide-tooltip)
              (when tooltip-frame
                (send tooltip-frame show #f)))
            
            (super-new [parent parent])
            ;; do this so we can expand/collapse languages on a single click
            (inherit on-click-always allow-deselect)
            (on-click-always #t)
            (allow-deselect #t)))
        
        (define outermost-panel (new horizontal-panel% 
                                     [parent parent]
                                     [alignment '(left top)]))
        (define languages-choice-panel (new vertical-panel%
                                            [parent outermost-panel]
                                            [stretchable-height #f]
                                            [alignment '(left top)]))
        
        (define the-racket-language-panel (new vertical-panel%
                                               [parent languages-choice-panel]
                                               [alignment '(left top)]
                                               [stretchable-height #f]))
        
        (define use-language-in-source-rb
          (new radio-box% 
               [label #f]
               [choices (list sc-use-language-in-source)]
               [parent the-racket-language-panel]
               [callback
                (λ (rb evt)
                  (use-language-in-source-rb-callback))]))
        (define (use-language-in-source-rb-callback)
          (module-language-selected)
          (send use-chosen-language-rb set-selection #f)
          (send use-teaching-language-rb set-selection #f))
        (define in-source-discussion-panel (new horizontal-panel% 
                                                [parent the-racket-language-panel]
                                                [stretchable-height #f]))
        (define in-source-discussion-spacer (new horizontal-panel% 
                                                 [parent in-source-discussion-panel]
                                                 [stretchable-width #f]
                                                 [min-width 32]))
        (define in-source-discussion-editor-canvas 
          (add-discussion in-source-discussion-panel 
                          definitions-text
                          use-language-in-source-rb-callback))
        (define most-recent-languages-hier-list-selection
          (preferences:get 'drracket:language-dialog:hierlist-default))
        (define most-recent-teaching-languages-hier-list-selection 
          (preferences:get 'drracket:language-dialog:teaching-hierlist-default))
        
        (define use-teaching-language-rb
          (new radio-box% 
               [label #f]
               [choices (list sc-use-teaching-language)]
               [parent languages-choice-panel]
               [callback
                (λ (rb evt)
                  (use-teaching-language-rb-callback))]))
        (define (use-teaching-language-rb-callback)
          (cond
            [most-recent-teaching-languages-hier-list-selection
             (select-a-language-in-hierlist teaching-languages-hier-list
                                            (cdr most-recent-teaching-languages-hier-list-selection))]
            [else
             (select-first-language-in-hierlist teaching-languages-hier-list)])
          (send use-chosen-language-rb set-selection #f)
          (send use-language-in-source-rb set-selection #f)
          (send use-teaching-language-rb set-selection 0)
          (send other-languages-hier-list select #f)
          (send teaching-languages-hier-list focus))
        
        (define teaching-languages-hier-list-panel
          (new horizontal-panel% [parent languages-choice-panel] [stretchable-height #f]))
        (define teaching-languages-hier-list-spacer
          (new horizontal-panel% 
               [parent teaching-languages-hier-list-panel]
               [stretchable-width #f]
               [min-width 16]))
        
        (define teaching-languages-hier-list 
          (new selectable-hierlist% 
               [parent teaching-languages-hier-list-panel]
               [style '(no-border no-hscroll auto-vscroll transparent)]))
        
        (define use-chosen-language-rb
          (new radio-box%
               [label #f]
               [choices (list sc-choose-a-language)]
               [parent languages-choice-panel]
               [callback
                (λ (this-rb evt)
                  (use-chosen-language-rb-callback))]))
        (define (use-chosen-language-rb-callback)
          (show-other-languages)
          (when most-recent-languages-hier-list-selection
            (select-a-language-in-hierlist other-languages-hier-list
                                           most-recent-languages-hier-list-selection))
          (send use-language-in-source-rb set-selection #f)
          (send use-teaching-language-rb set-selection #f)
          (send teaching-languages-hier-list select #f)
          (send other-languages-hier-list focus))
        (define (show-other-languages)
          (when (member ellipsis-spacer-panel (send languages-hier-list-panel get-children))
            (send languages-hier-list-panel change-children
                  (λ (l)
                    (list languages-hier-list-spacer other-languages-hier-list)))))
        
        (define languages-hier-list-panel (new horizontal-panel% 
                                               [parent languages-choice-panel] 
                                               [stretchable-height #f]))
        (define ellipsis-spacer-panel (new horizontal-panel% 
                                           [parent languages-hier-list-panel]
                                           [stretchable-width #f]
                                           [min-width 32]))
        (define ellipsis-message 
          (new (class canvas%
                 (define/override (on-paint)
                   (define dc (get-dc))
                   (send dc set-font normal-control-font)
                   (send dc draw-text "..." 0 0))
                 (define/override (on-event evt)
                   (when (send evt button-up?)
                     (show-other-languages)))
                 (inherit get-dc min-width min-height)
                 (super-new [style '(transparent)]
                            [parent languages-hier-list-panel]
                            [stretchable-width #f]
                            [stretchable-height #t])
                 (let ()
                   (define dc (get-dc))
                   (define-values (w h _1 _2) (send dc get-text-extent "..." normal-control-font))
                   (min-width (inexact->exact (ceiling w)))
                   (min-height (inexact->exact (ceiling h)))))))
        
        (define languages-hier-list-spacer (new horizontal-panel% 
                                                [parent languages-hier-list-panel]
                                                [stretchable-width #f]
                                                [min-width 16]))
        
        (define other-languages-hier-list 
          (new selectable-hierlist% 
               [parent languages-hier-list-panel]
               [style '(no-border no-hscroll auto-vscroll transparent)]))
        (define details-outer-panel (make-object vertical-pane% outermost-panel))
        (define details/manual-parent-panel (make-object vertical-panel% details-outer-panel))
        (define details-panel (make-object panel:single% details/manual-parent-panel))
        
        (define no-details-panel (make-object vertical-panel% details-panel))
        
        (define languages-table (make-hasheq))
        (define languages (get-languages))
        
        ;; selected-language : (union (instanceof language<%>) #f)
        ;; invariant: selected-language and get/set-selected-language-settings
        ;;            match the user's selection in the languages-hier-list.
        ;;            or #f if the user is not selecting a language.
        (define selected-language #f)
        ;; get/set-selected-language-settings (union #f (-> settings))
        (define get/set-selected-language-settings #f)
        
        (define details-computed? #f)
        
        ;; language-mixin : (implements language<%>) 
        ;;                  (-> (implements area-container<%>))
        ;;                  get/set 
        ;;                  ->
        ;;                  ((implements hierlist<%>) -> (implements hierlist<%>))
        ;; a mixin that responds to language selections and updates the details-panel
        (define (language-mixin language get-language-details-panel get/set-settings)
          (λ (%)
            (class* % (hieritem-language<%>)
              (init-rest args)
              (define/public (get-language) language)
              (define/public (selected)
                (update-gui-based-on-selected-language language
                                                       get-language-details-panel
                                                       get/set-settings))
              (apply super-make-object args))))
        
        (define (update-gui-based-on-selected-language language
                                                       get-language-details-panel
                                                       get/set-settings)
          (let ([ldp (get-language-details-panel)])
            (when ldp
              (send details-panel active-child ldp)))
          (send revert-to-defaults-button enable #t)
          (set! get/set-selected-language-settings get/set-settings)
          (set! selected-language language))
        
        (define (module-language-selected)
          ;; need to deselect things in the languages-hier-list at this point.
          (send other-languages-hier-list select #f)
          (send teaching-languages-hier-list select #f)
          (send use-language-in-source-rb set-selection 0)
          (send use-chosen-language-rb set-selection #f)
          (send use-teaching-language-rb set-selection #f)
          (ok-handler 'enable)
          (send details-button enable #t)
          (update-gui-based-on-selected-language module-language*language
                                                 module-language*get-language-details-panel
                                                 module-language*get/set-settings))
        
        ;; no-language-selected : -> void
        ;; updates the GUI for the situation where no language at all selected, and
        ;; and thus none of the radio buttons should be selected. 
        ;; this generally happens when there is no preference setting for the language
        ;; (ie the user has just started drracket for the first time)
        (define (no-language-selected)
          (non-language-selected)
          (send use-language-in-source-rb set-selection #f)
          (send use-chosen-language-rb set-selection #f)
          (send use-teaching-language-rb set-selection #f))
        
        (define module-language*language 'module-language*-not-yet-set)
        (define module-language*get-language-details-panel 'module-language*-not-yet-set)
        (define module-language*get/set-settings 'module-language*-not-yet-set)
        
        ;; non-language-selected : -> void
        ;; updates the GUI and selected-language and get/set-selected-language-settings
        ;; for when some non-language is selected in the hierlist
        (define (non-language-selected)
          (send revert-to-defaults-button enable #f)
          (send details-panel active-child no-details-panel)
          (set! get/set-selected-language-settings #f)
          (set! selected-language #f)
          (ok-handler 'disable)
          (send details-button enable #f))
        
        ;; something-selected : item -> void
        (define (something-selected hierlist item)
          (send use-language-in-source-rb set-selection #f)
          (cond
            [(eq? hierlist other-languages-hier-list)
             (send use-teaching-language-rb set-selection #f)
             (send use-chosen-language-rb set-selection 0)
             (send teaching-languages-hier-list select #f)]
            [else 
             (send use-teaching-language-rb set-selection 0)
             (send use-chosen-language-rb set-selection #f)
             (send other-languages-hier-list select #f)])
          (ok-handler 'enable)
          (send details-button enable #t)
          (send item selected))
        
        ;; construct-details : (union (-> void) #f)
        (define construct-details void)
        
        (define any-teaching-language-added? #f)
        
        ;; add-language-to-dialog : (instanceof language<%>) -> void
        ;; adds the language to the dialog
        ;; opens all of the turn-down tags
        ;; when `language' matches language-to-show, update the settings
        ;;   panel to match language-to-show, otherwise set to defaults.
        (define (add-language-to-dialog language)
          (define positions (send language get-language-position))
          (define numbers (send language get-language-numbers))
          (define teaching-language? (and (pair? positions)
                                          (equal? (car positions)
                                                  (string-constant teaching-languages))))
          
          (when teaching-language? (set! any-teaching-language-added? #t))
          
          ;; don't show the initial language ...
          (unless (equal? positions initial-language-position)
            (unless (and (list? positions)
                         (list? numbers)
                         (pair? positions)
                         (pair? numbers)
                         (andmap number? numbers)
                         (andmap string? positions)
                         (= (length positions) (length numbers))
                         ((length numbers) . >= . 1))
              (error 'drracket:language
                     (string-append
                      "languages position and numbers must be lists of strings and numbers,"
                      " respectively, must have the same length, and must each contain at"
                      " least one element, got: ~e ~e")
                     positions numbers))
            
            (when (null? (cdr positions))
              (unless (equal? positions (list (string-constant module-language-name)))
                (error 'drracket:language
                       (string-append
                        "Only the module language may be at the top level."
                        " Other languages must have at least two levels"))))
            
            (send other-languages-hier-list clear-fringe-cache)
            (send teaching-languages-hier-list clear-fringe-cache)
            
            #|
              
              inline the first level of the tree into just items in the hierlist
              keep track of the starting (see call to sort method below) by
              adding a second field to the second level of the tree that indicates
              what the sorting number is for its level above (in the second-number mixin)
              
              |#
            (let add-sub-language ([ht languages-table]
                                   [hier-list (if teaching-language?
                                                  teaching-languages-hier-list
                                                  other-languages-hier-list)]
                                   [positions (if teaching-language?
                                                  (cdr positions)
                                                  positions)]
                                   [numbers (if teaching-language?
                                                (cdr numbers)
                                                numbers)]
                                   [first? #t]
                                   
                                   ;; only non-#f during the second iteration
                                   ;; in which case it is the first iterations number
                                   [second-number #f])
              (cond
                [(null? (cdr positions))
                 (let* ([language-details-panel #f]
                        [real-get/set-settings 
                         (case-lambda
                           [() 
                            (cond
                              [(and language-to-show 
                                    settings-to-show
                                    (equal? (send language-to-show get-language-position)
                                            (send language get-language-position)))
                               settings-to-show]
                              [else
                               (send language default-settings)])]
                           [(x) (void)])]
                        [get-language-details-panel (lambda () language-details-panel)]
                        [get/set-settings (lambda x (apply real-get/set-settings x))]
                        [position (car positions)]
                        [number (car numbers)])
                   
                   (set! construct-details
                         (let ([old construct-details])
                           (lambda ()
                             (old)
                             (let-values ([(language-details-panel-real get/set-settings)
                                           (make-details-panel language)])
                               (set! language-details-panel language-details-panel-real)
                               (set! real-get/set-settings get/set-settings))
                             
                             (let-values ([(vis-lang vis-settings)
                                           (cond
                                             [(and (not selected-language)
                                                   (eq? language-to-show language))
                                              (values language-to-show settings-to-show)]
                                             [(eq? selected-language language)
                                              (values language 
                                                      (if (eq? language language-to-show)
                                                          settings-to-show
                                                          (send language default-settings)))]
                                             [else (values #f #f)])])
                               (cond
                                 [(and vis-lang
                                       (equal? (send vis-lang get-language-position)
                                               (send language get-language-position)))
                                  (get/set-settings vis-settings)
                                  (send details-panel active-child language-details-panel)]
                                 [else
                                  (get/set-settings (send language default-settings))])))))
                   
                   (cond
                     [(equal? positions (list (string-constant module-language-name)))
                      (set! module-language*language language)
                      (set! module-language*get-language-details-panel get-language-details-panel)
                      (set! module-language*get/set-settings get/set-settings)]
                     [else
                      (let* ([mixin (compose
                                     number-mixin
                                     (language-mixin language 
                                                     get-language-details-panel
                                                     get/set-settings))]
                             [item
                              (send hier-list new-item
                                    (if second-number
                                        (compose second-number-mixin mixin)
                                        mixin))]
                             [text (send item get-editor)]
                             [delta (send language get-style-delta)])
                        (send item set-number number)
                        (when second-number
                          (send item set-second-number second-number))
                        (send text insert position)
                        (when delta
                          (cond
                            [(list? delta)
                             (for-each (λ (x)
                                         (send text change-style 
                                               (car x)
                                               (cadr x)
                                               (caddr x)))
                                       delta)]
                            [(is-a? delta style-delta%)
                             (send text change-style 
                                   (send language get-style-delta)
                                   0
                                   (send text last-position))])))]))]
                [else
                 (let* ([position (car positions)]
                        [number (car numbers)]
                        [sub-ht/sub-hier-list
                         (hash-ref
                          ht
                          (string->symbol position)
                          (λ ()
                            (if first?
                                (let* ([item (send hier-list new-item number-mixin)]
                                       [x (list (make-hasheq) hier-list item)])
                                  (hash-set! ht (string->symbol position) x)
                                  (send item set-number number)
                                  (send item set-allow-selection #f)
                                  (let* ([editor (send item get-editor)]
                                         [pos (send editor last-position)])
                                    (send editor insert "\n")
                                    (send editor insert position)
                                    (send editor change-style small-size-delta pos (+ pos 1))
                                    (send editor change-style section-style-delta 
                                          (+ pos 1) (send editor last-position)))
                                  x)
                                (let* ([new-list (send hier-list new-list
                                                       (if second-number
                                                           (compose second-number-mixin number-mixin)
                                                           number-mixin))]
                                       [x (list (make-hasheq) new-list #f)])
                                  (send new-list set-number number)
                                  (when second-number
                                    (send new-list set-second-number second-number))
                                  (send new-list set-allow-selection #t)
                                  (send new-list open)
                                  (send (send new-list get-editor) insert position)
                                  (hash-set! ht (string->symbol position) x)
                                  x))))])
                   (cond
                     [first? 
                      (unless (= number (send (caddr sub-ht/sub-hier-list) get-number))
                        (error 'add-language "language ~s; expected number for ~e to be ~e, got ~e"
                               (send language get-language-name)
                               position
                               (send (caddr sub-ht/sub-hier-list) get-number)
                               number))]
                     [else
                      (unless (= number (send (cadr sub-ht/sub-hier-list) get-number))
                        (error 'add-language "language ~s; expected number for ~e to be ~e, got ~e"
                               (send language get-language-name)
                               position
                               (send (cadr sub-ht/sub-hier-list) get-number)
                               number))])
                   (add-sub-language (car sub-ht/sub-hier-list)
                                     (cadr sub-ht/sub-hier-list)
                                     (cdr positions)
                                     (cdr numbers)
                                     #f
                                     (if first? number #f)))]))))
        
        (define number<%>
          (interface ()
            get-number
            set-number))
        
        (define second-number<%>
          (interface ()
            get-second-number
            set-second-number))
        
        ;; number-mixin : (extends object%) -> (extends object%)
        ;; adds the get/set-number methods to this class
        (define (number-mixin %)
          (class* % (number<%>)
            (field (number 0))
            (define/public (get-number) number)
            (define/public (set-number _number) (set! number _number))
            (super-instantiate ())))
        
        ;; second-number-mixin : (extends object%) -> (extends object%)
        ;; adds the get/set-second-number methods to this class
        (define (second-number-mixin %)
          (class* % (second-number<%>)
            (field (second-number 0))
            (define/public (get-second-number) second-number)
            (define/public (set-second-number _second-number) (set! second-number _second-number))
            (super-instantiate ())))
        
        ;; make-details-panel : (instanceof language<%>) 
        ;;                   -> (values panel (case-> (-> settings) (settings -> void)))
        ;; adds a details panel for `language', using
        ;; the language's default settings, unless this is
        ;; the to-show language.
        (define (make-details-panel language)
          (let ([panel (instantiate vertical-panel% ()
                         (parent details-panel)
                         (stretchable-width #f)
                         (stretchable-height #f))])
            (values
             panel
             (send language config-panel panel))))
        
        ;; close-all-languages : -> void
        ;; closes all of the tabs in the language hier-list.
        (define (close-all-languages)
          (define (close-children list)
            (for-each close-this-one (send list get-items)))
          (define (close-this-one item)
            (cond
              [(is-a? item hierarchical-list-compound-item<%>)
               (send item close)
               (close-children item)]
              [else (void)]))
          (close-children other-languages-hier-list)
          (close-children teaching-languages-hier-list))
        
        ;; open-current-language : -> void
        ;; opens the tabs that lead to the current language
        ;; and selects the current language
        (define (open-current-language)
          
          ;; set the initial selection in the hierlists
          (let ([hier-default (preferences:get 'drracket:language-dialog:hierlist-default)])
            (when hier-default
              (select-a-language-in-hierlist other-languages-hier-list hier-default)))
          (let ([hier-default (preferences:get 'drracket:language-dialog:teaching-hierlist-default)])
            (when hier-default
              (select-a-language-in-hierlist teaching-languages-hier-list (cdr hier-default))))
          
          (send languages-hier-list-panel change-children
              (λ (l)
                (list ellipsis-spacer-panel ellipsis-message)))
          
          (cond
            [(not (and language-to-show settings-to-show))
             (no-language-selected)]
            [(is-a? language-to-show drracket:module-language:module-language<%>)
             ;; the above changes the radio button selections,
             ;; so do it before calling module-language-selected
             (module-language-selected)]
            [else
             (define position (send language-to-show get-language-position))
             (cond
               [(and (pair? position)
                     (equal? (car position) 
                             (string-constant teaching-languages)))
                (select-a-language-in-hierlist teaching-languages-hier-list (cdr position))
                (send use-teaching-language-rb set-selection 0)
                (send use-chosen-language-rb set-selection #f)
                (send teaching-languages-hier-list focus)]
               [else
                (send languages-hier-list-panel change-children
                      (λ (l)
                        (list languages-hier-list-spacer other-languages-hier-list)))
                (select-a-language-in-hierlist other-languages-hier-list position)
                (send use-teaching-language-rb set-selection #f)
                (send use-chosen-language-rb set-selection 0)
                (send other-languages-hier-list focus)])
             (send use-language-in-source-rb set-selection #f)]))
        
        (define (select-a-language-in-hierlist hier-list language-position)
          (cond
            [(null? (cdr language-position))
             ;; nothing to open here
             (send (car (send hier-list get-items)) select #t)]
            [else
             (let loop ([hi hier-list]
                        
                        ;; skip the first position, since it is flattened into the dialog
                        [first-pos (cadr language-position)]
                        [position (cddr language-position)])
               (let ([matching-children
                      (filter (λ (x)
                                (equal? (send (send x get-editor) get-text)
                                        first-pos))
                              (send hi get-items))])
                 (cond
                   [(null? matching-children) 
                    (void)]
                   [else
                    (let ([child (car matching-children)])
                      (cond
                        [(null? position)
                         (send child select #t)]
                        [else
                          ;; test can fail when prefs are bad
                         (when (is-a? child hierarchical-list-compound-item<%>)
                           (send child open)
                           (loop child (car position) (cdr position)))]))])))]))
        
        (define (select-first-language-in-hierlist hier-list)
          (let loop ([hi hier-list])
            (for/or ([child (in-list (send hi get-items))])
              (cond
                [(is-a? child hierarchical-list-compound-item<%>)
                 (send child open)
                 (loop child)]
                [(is-a? child hieritem-language<%>)
                 (send child select #t)
                 #t]
                [else 
                 #f]))))
        
        ;; docs-callback : -> void
        (define (docs-callback)
          (void))
        
        ;; details-shown? : boolean
        ;; indicates if the details are currently visible in the dialog
        (define details-shown? (and language-to-show
                                    settings-to-show 
                                    (not (send language-to-show default-settings? settings-to-show))))
        
        ;; details-callback : -> void
        ;; flips the details-shown? flag and resets the GUI
        (define (details-callback)
          (do-construct-details)
          (set! details-shown? (not details-shown?))
          (when re-center
            (send re-center begin-container-sequence))
          (update-show/hide-details)
          (when re-center
            (send re-center center 'both)
            (send re-center end-container-sequence)))
        
        ;; do-construct-details : -> void
        ;; construct the details panels, if they have not been constructed
        (define (do-construct-details)
          (when construct-details
            (send details-button enable #f)
            (construct-details)
            (set! construct-details #f)
            (send details-button enable #t)))
        
        ;; show/hide-details : -> void
        ;; udpates the GUI based on the details-shown? flag
        (define (update-show/hide-details)
          (send details-button set-label 
                (if details-shown? hide-details-label show-details-label))
          (send parent begin-container-sequence)
          (send revert-to-defaults-outer-panel change-children
                (λ (l)
                  (if details-shown? (list revert-to-defaults-button) null)))
          (send details-outer-panel change-children
                (λ (l)
                  (if details-shown? (list details/manual-parent-panel) null)))
          (send parent end-container-sequence))
        
        ;; revert-to-defaults-callback : -> void
        (define (revert-to-defaults-callback)
          (when selected-language
            (get/set-selected-language-settings 
             (send selected-language default-settings))))
        
        (define show-details-label (string-constant show-details-button-label))
        (define hide-details-label (string-constant hide-details-button-label))
        (define details-button (make-object button% 
                                 (if (show-details-label . system-font-space->= . hide-details-label)
                                     show-details-label
                                     hide-details-label)
                                 show-details-parent
                                 (λ (x y)
                                   (details-callback))))
        
        (define revert-to-defaults-outer-panel (make-object horizontal-panel% show-details-parent))
        (define revert-to-defaults-button (make-object button% 
                                            (string-constant revert-to-language-defaults)
                                            revert-to-defaults-outer-panel
                                            (λ (_1 _2)
                                              (revert-to-defaults-callback))))
        
        (send revert-to-defaults-outer-panel stretchable-width #f)
        (send revert-to-defaults-outer-panel stretchable-height #f)
        
        (for-each add-language-to-dialog languages)
        
        (unless any-teaching-language-added?
          (send languages-choice-panel change-children
                (λ (l)
                  (remove* (list teaching-languages-hier-list-panel use-teaching-language-rb)
                           l))))
        
        (define (hier-list-sort-predicate x y)
          (cond
            [(and (x . is-a? . second-number<%>)
                  (y . is-a? . second-number<%>))
             (cond
               [(= (send x get-second-number)
                   (send y get-second-number))
                (< (send x get-number) (send y get-number))]
               [else
                (< (send x get-second-number)
                   (send y get-second-number))])]
            [(and (x . is-a? . number<%>)
                  (y . is-a? . second-number<%>))
             (cond
               [(= (send x get-number)
                   (send y get-second-number))
                #t]
               [else
                (< (send x get-number)
                   (send y get-second-number))])]
            [(and (x . is-a? . second-number<%>)
                  (y . is-a? . number<%>))
             (cond
               [(= (send x get-second-number)
                   (send y get-number))
                #f]
               [else (< (send x get-second-number)
                        (send y get-number))])]
            [(and (x . is-a? . number<%>)
                  (y . is-a? . number<%>))
             (< (send x get-number) (send y get-number))]
            [else #f]))
        (send other-languages-hier-list sort hier-list-sort-predicate)
        (send teaching-languages-hier-list sort hier-list-sort-predicate)
        
        ;; remove the newline at the front of the first inlined category (if there)
        ;; it won't be there if the module language is at the top.
        (for ([hier-list (in-list (list other-languages-hier-list teaching-languages-hier-list))])
          (define items (send hier-list get-items))
          (unless (null? items)
            (define t (send (car items) get-editor))
            (when (equal? "\n" (send t get-text 0 1))
              (send t delete 0 1))))
        
        (send details-outer-panel stretchable-width #f)
        (send details/manual-parent-panel change-children 
              (λ (l)
                (list details-panel)))
        
        (define (config-hier-list hier-list) 
          (send hier-list stretchable-width #t)
          (send hier-list stretchable-height #t)
          (send hier-list accept-tab-focus #t)
          (send hier-list allow-tab-exit #t))
        (config-hier-list other-languages-hier-list)
        (config-hier-list teaching-languages-hier-list)
        (send parent reflow-container)
        (close-all-languages)
        (open-current-language)
        (define (set-min-sizes hier-list)
          (send hier-list min-client-width (text-width (send hier-list get-editor)))
          (send hier-list min-client-height (text-height (send hier-list get-editor))))
        (set-min-sizes other-languages-hier-list)
        (set-min-sizes teaching-languages-hier-list)
        (when details-shown?
          (do-construct-details))
        (update-show/hide-details)
        (when get/set-selected-language-settings
          ;; this call to get/set-selected-language-settings has to come after the call
          ;; to do-construct-details above because do-construct-details sets all of the
          ;; controls to the language's default settings
          (get/set-selected-language-settings settings-to-show))
        (size-discussion-canvas in-source-discussion-editor-canvas)
        (values
         (λ () selected-language)
         (λ () 
           (and get/set-selected-language-settings
                (get/set-selected-language-settings)))
         (λ (receiver evt)
           (case (send evt get-key-code)
             [(#\r) 
              (if (mouse-event-uses-shortcut-prefix? evt)
                  (begin (send use-language-in-source-rb set-selection 0)
                         (use-language-in-source-rb-callback)
                         #t)
                  #f)]
             [(#\t)
              (if (mouse-event-uses-shortcut-prefix? evt)
                  (begin 
                    (send use-teaching-language-rb set-selection 0)
                    (use-teaching-language-rb-callback)
                    #t)
                  #f)]
             [(#\o)
              (if (mouse-event-uses-shortcut-prefix? evt)
                  (begin 
                    (send use-chosen-language-rb set-selection 0)
                    (use-chosen-language-rb-callback)
                    #t)
                  #f)]
             [else #f])))))
    
    (define (add-discussion p definitions-text use-language-in-source-rb-callback)
      (define t (new (text:hide-caret/selection-mixin text:standard-style-list%)))
      (define c (new editor-canvas%
                     [stretchable-width #t]
                     [horizontal-inset 0]
                     [vertical-inset 0]
                     [parent p]
                     [style '(no-border no-vscroll no-hscroll transparent)]
                     [editor t]))
      (send t set-styles-sticky #f)
      (send t set-autowrap-bitmap #f)
      (define size-sd (make-object style-delta%
                        'change-size
                        (send normal-control-font get-point-size)))
      (define (do-insert str tt-style?)
        (define before (send t last-position))
        (send t insert str before before)
        (cond
          [tt-style?
           (send t change-style 
                 (send (send t get-style-list) find-named-style "Standard")
                 before (send t last-position))]
          [else
           (send t change-style 
                 (send (send t get-style-list) basic-style)
                 before (send t last-position))])
        (send t change-style size-sd before (send t last-position)))
      (when (send normal-control-font get-size-in-pixels)
        (send size-sd set-size-in-pixels-on #t))
      (let loop ([strs (regexp-split #rx"#lang" (string-constant racket-language-discussion))])
        (do-insert (car strs) #f)
        (unless (null? (cdr strs))
          (do-insert "#lang" #t)
          (loop (cdr strs))))
      
      (define spacer-snips '())
      (define spacer-poses '())
      
      (for ([lang (in-list '(racket racket/base typed/racket scribble/base))])
        (define the-lang-line (format "#lang ~a" lang))
        (do-insert "  " #t)
        (define before-lang (send t last-position))
        (do-insert the-lang-line #t)
        (define after-lang (send t last-position))
        (define spacer (new spacer-snip%))
        (define spacer-pos (send t last-position))
        (set! spacer-snips (cons spacer spacer-snips))
        (set! spacer-poses (cons spacer-pos spacer-poses))
        (send t insert spacer spacer-pos spacer-pos)
        (do-insert "  [" #f)
        (define before-docs (send t last-position))
        (do-insert "docs" #f)
        (define after-docs (send t last-position))
        (do-insert "]\n" #f)
        (send t set-clickback before-lang after-lang
              (λ (t start end)
                (use-language-in-source-rb-callback)
                (define-values (current-line-start current-line-end) 
                  (if definitions-text
                      (find-language-position definitions-text)
                      (values #f #f)))
                (define existing-lang-line 
                  (and current-line-start
                       (send definitions-text get-text current-line-start current-line-end)))
                (case (message-box/custom
                       (string-constant drscheme)
                       (string-append
                        (string-constant racket-dialect-in-buffer-message)
                        "\n\n"
                        (cond
                          [(and existing-lang-line
                                (equal? existing-lang-line the-lang-line))
                           (format (string-constant racket-dialect-already-same-#lang-line) 
                                   existing-lang-line)]
                          [existing-lang-line
                           (format (string-constant racket-dialect-replace-#lang-line) 
                                   existing-lang-line
                                   the-lang-line)]
                          [else
                           (format (string-constant racket-dialect-add-new-#lang-line)
                                   the-lang-line)]))
                       (cond
                         [(and existing-lang-line
                               (equal? existing-lang-line the-lang-line))
                          (string-constant ok)]
                         [existing-lang-line
                          (string-constant replace-#lang-line)]
                         [else
                          (string-constant add-#lang-line)])
                       (and (not (equal? existing-lang-line the-lang-line))
                            (string-constant cancel))
                       #f #f
                       '(default=1))
                  [(1) 
                   (cond
                     [current-line-start
                      (send definitions-text begin-edit-sequence)
                      (send definitions-text delete current-line-start current-line-end)
                      (send definitions-text insert 
                            the-lang-line
                            current-line-start
                            current-line-start)
                      (send definitions-text end-edit-sequence)]
                     [else
                      (send definitions-text begin-edit-sequence)
                      (send definitions-text insert "\n" 0 0)
                      (send definitions-text insert the-lang-line 0 0)
                      (send definitions-text end-edit-sequence)])]
                  [else (void)])))
        (send t set-clickback before-docs after-docs 
              (λ (t start end)
                (define-values (path tag) (xref-tag->path+anchor (load-collections-xref) 
                                                                 (make-module-language-tag lang)))
                (define url (path->url path))
                (define url2 (if tag
                                 (make-url (url-scheme url)
                                           (url-user url)
                                           (url-host url)
                                           (url-port url)
                                           (url-path-absolute? url)
                                           (url-path url)
                                           (url-query url)
                                           tag)
                                 url))
                (send-url (url->string url2)))))
      
      (do-insert (string-constant racket-language-discussion-end) #f)
      
      (define kmp (send t set-keymap (keymap:get-editor)))
      
      (send (send c get-parent) reflow-container)
      
      (define xb (box 0))
      (define max-spacer-pos
        (for/fold ([m 0]) ([spacer-pos (in-list spacer-poses)])
          (send t position-location spacer-pos xb #f)
          (max m (unbox xb))))
      (for ([spacer-pos (in-list spacer-poses)]
            [spacer-snip (in-list spacer-snips)])
        (send t position-location spacer-pos xb #f)
        (send spacer-snip set-width (- max-spacer-pos (unbox xb))))
      
      (send t hide-caret #t)
      (send t auto-wrap #t)
      (send t lock #t)
      (send c accept-tab-focus #f)
      (send c allow-tab-exit #t)

      c)
    
    (define (find-language-position definitions-text)
      (define prt (open-input-text-editor definitions-text))
      (port-count-lines! prt)
      (define l (with-handlers ((exn:fail? (λ (x) #f)))
                  (read-language prt)))
      (cond
        [l
         (define-values (line col pos) (port-next-location prt))
         (define hash-lang-start (send definitions-text find-string "#lang" 'backward pos 0 #f))
         (if hash-lang-start
             (values hash-lang-start (- pos 1))
             (values #f #f))]
        [else
         (values #f #f)]))
    
    (define spacer-snip%
      (class snip%
        (inherit get-admin)
        (define width 0)
        (define/public (set-width w) 
          (set! width w)
          (define admin (get-admin))
          (when admin 
            (send admin resized this #t)))
        (define/override (get-text [start 0] [end 'eof] [flattened? #f] [force-cr? #f])
          "")
        (define/override (get-extent dc x y wb hb db ab lb sp)
          (super get-extent dc x y wb hb db ab lb sp)
          (when (box? wb) (set-box! wb width)))
        (super-new)
        (inherit set-snipclass)
        (set-snipclass spacer-sc)))
    (define spacer-sc (new snip-class%))
    (send spacer-sc set-classname "drracket:spacer-snipclass")
    (send spacer-sc set-version 0)
    (send (get-the-snip-class-list) add spacer-sc)
    
    (define (size-discussion-canvas canvas)
      (define t (send canvas get-editor))
      (define by (box 0))
      (send t position-location 
            (send t line-end-position (send t last-line))
            #f
            by)
      (send canvas min-height (+ (ceiling (inexact->exact (unbox by))) 24)))
    
    (define section-style-delta (make-object style-delta% 'change-bold))
    (send section-style-delta set-delta-foreground "medium blue")
    (define small-size-delta (make-object style-delta% 'change-size 9))
    
    (define (add-welcome dialog welcome-before-panel welcome-after-panel)
      (let* ([outer-pb%
              (class pasteboard%
                (define/override (can-interactive-move? evt)
                  #f)
                (super-instantiate ()))]
             [outer-pb (make-object outer-pb%)]
             [bitmap 
              (make-object bitmap%
                (build-path (collection-file-path "plt-small-shield.gif" "icons")))]
             [image-snip
              (make-object image-snip% 
                (collection-file-path "plt-small-shield.gif" "icons"))]
             [before-text (make-object text%)]
             [before-snip (make-object editor-snip% before-text #f)]
             [before-ec%
              (class editor-canvas% 
                (inherit get-client-size)
                (define/private (update-size)
                  (let-values ([(cw ch) (get-client-size)])
                    (unless (or (zero? cw)
                                (zero? ch))
                      (let ([image-l-box (box 0)]
                            [image-r-box (box 0)])
                        (send before-text get-snip-location image-snip image-l-box #f #f)
                        (send before-text get-snip-location image-snip image-r-box #f #t)
                        (let* ([image-w (send bitmap get-width)]
                               [before-snip-space (- cw image-w)]
                               [before-snip-w (- before-snip-space
                                                 5 5 ;; space before and after inside snip 
                                                 2   ;; space at end of outer editor
                                                 1   ;; space at beginning of outer editor
                                                 1   ;; space between image and snip
                                                 -5  ;; unknown space
                                                 )])
                          (send before-text set-max-width (max 0 before-snip-w)))))))
                (define/override (on-superwindow-show shown?)
                  (update-size)
                  (super on-superwindow-show shown?))
                (define/override (on-size w h)
                  (update-size)
                  (super on-size w h))
                (super-instantiate ()))]
             [before-ec (instantiate before-ec% ()
                          (parent welcome-before-panel)
                          (editor outer-pb)
                          (stretchable-height #f)
                          (style '(no-vscroll no-hscroll)))]
             [first-line-style-delta (make-object style-delta% 'change-bold)])
        (send first-line-style-delta set-delta-foreground (make-object color% 150 0 150))
        (send before-ec min-width 550)
        
        (let-values ([(cw ch) (send before-ec get-client-size)]
                     [(w h) (send before-ec get-size)])
          (send before-ec min-height 
                (+ (send bitmap get-height) 
                   8  ;; pasteboards apparently want some space here....
                   (- h ch))))
        
        (send outer-pb insert image-snip)
        (send outer-pb insert before-snip)
        (send outer-pb move image-snip 0 0)
        (send outer-pb move before-snip (send bitmap get-width) 0)
        (send outer-pb set-selection-visible #f)
        (send outer-pb lock #t)
        
        ;(send before-snip set-align-top-line #t)
        (send before-text insert 
              (format (string-constant welcome-to-drscheme-version/language)
                      (version:version)
                      (this-language)))
        (send before-text insert #\newline)
        (send before-text insert (string-constant introduction-to-language-dialog))
        (send before-text change-style 
              first-line-style-delta
              0
              (send before-text paragraph-end-position 0))
        (send before-text auto-wrap #t)
        
        (send before-text lock #t)
        (send before-text hide-caret #t)
        
        (for-each (λ (native-lang-string language)
                    (unless (equal? (this-language) language)
                      (instantiate button% ()
                        (label native-lang-string)
                        (parent welcome-after-panel)
                        (stretchable-width #t)
                        (callback (λ (x1 x2) (drracket:app:switch-language-to dialog language))))))
                  (string-constants is-this-your-native-language)
                  (all-languages))))
    
    ;; system-font-space->= : string string -> boolean
    ;; determines which string is wider, when drawn in the system font
    (define (x . system-font-space->= . y)
      (let ([bdc (make-object bitmap-dc%)])
        (send bdc set-bitmap (make-object bitmap% 1 1 #t))
        (send bdc set-font (send the-font-list find-or-create-font
                                 12 'system 'normal 'normal))
        (let-values ([(wx _1 _2 _3) (send bdc get-text-extent x)]
                     [(wy _4 _5 _6) (send bdc get-text-extent y)])
          (wx . >= . wy))))
    
    ;; text-width : (isntanceof text%) -> exact-integer
    ;; calculates the width of widest line in the
    ;; editor. This only makes sense if auto-wrap
    ;; is turned off. Otherwise, you could just use
    ;; the admin's width.
    (define (text-width text)
      (let loop ([n (+ (send text last-line) 1)]
                 [current-max-width 0])
        (cond
          [(zero? n)
           (+
            10 ;; this should be some magic small constant (hopefully less than 10 on all platforms)
            (floor (inexact->exact current-max-width)))]
          [else (let* ([line-number (- n 1)]
                       [box (box 0.0)]
                       [eol-pos (send text line-end-position line-number)]
                       [eol-snip (send text find-snip eol-pos 'before)])
                  (when eol-snip
                    (send text get-snip-location eol-snip box #f #t))
                  (loop (- n 1)
                        (max current-max-width (unbox box))))])))
    
    ;; text-height : (is-a?/c text% -> exact-integer
    (define (text-height text)
      (let ([y-box (box 0)])
        (send text position-location 
              (send text last-position)
              #f
              y-box
              #f
              #f
              #t)
        (+ 16 ;; upper bound on some space I don't know how to get.
           (floor (inexact->exact (unbox y-box))))))
    


;                                             
;                                             
;                                             
;                                             
;  ;;;          ;;;;                          
;              ;;;                            
;  ;;; ;;; ;;  ;;;;   ;;;        ;;;;   ;;;;  
;  ;;; ;;;;;;; ;;;;  ;;;;;      ;;; ;; ;;; ;; 
;  ;;; ;;; ;;; ;;;  ;;; ;;;     ;;;    ;;;    
;  ;;; ;;; ;;; ;;;  ;;; ;;;      ;;;;   ;;;;  
;  ;;; ;;; ;;; ;;;  ;;; ;;; ;;;    ;;;    ;;; 
;  ;;; ;;; ;;; ;;;   ;;;;;  ;;; ;; ;;; ;; ;;; 
;  ;;; ;;; ;;; ;;;    ;;;   ;;;  ;;;;   ;;;;  
;                                             
;                                             
;                                             
;                                             
;                                                                     
;                                                                     
;                                                                     
;                                                                     
;  ;;;                                                                
;  ;;;                                                                
;  ;;;  ;;;;;  ;;; ;;   ;; ;;; ;;; ;;;  ;;;;;   ;; ;;;   ;;;;   ;;;;  
;  ;;; ;;;;;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;;;;;; ;;;;;;;  ;; ;;; ;;; ;; 
;  ;;; ;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;  ;;; ;;; ;;; ;;; ;;; ;;;    
;  ;;;   ;;;;; ;;; ;;; ;;; ;;; ;;; ;;;   ;;;;; ;;; ;;; ;;;;;;;  ;;;;  
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;        ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;;;;;;  ;;;;;; ;; ;;; 
;  ;;;  ;;;;;; ;;; ;;;  ;; ;;;  ;; ;;;  ;;;;;;  ;; ;;;   ;;;;   ;;;;  
;                          ;;;                     ;;;                
;                      ;;;;;;                  ;;;;;;                 
;                                                                     
;                                                                     
    
    (define (add-info-specified-languages)
      (for-each add-info-specified-language
                (find-relevant-directories '(drscheme-language-positions
                                             get-drscheme-language-positions))))
    
    (define (add-info-specified-language directory)
      (let ([info-proc (get-info/full directory)])
        (when info-proc
          (let* ([lang-positions 
                  (append
                   (info-proc 'drscheme-language-positions (λ () null))
                   (indirect-info-field info-proc 'get-drscheme-language-positions directory))]
                 [lang-modules (info-proc 'drscheme-language-modules (λ () null))]
                 [numberss (info-proc 'drscheme-language-numbers 
                                      (λ ()
                                        (map (λ (lang-position)
                                               (map (λ (x) 0) lang-position))
                                             lang-positions)))]
                 [summaries (info-proc 'drscheme-language-one-line-summaries 
                                       (λ ()
                                         (map (λ (lang-position) "")
                                              lang-positions)))]
                 [urls      (info-proc 'drscheme-language-urls 
                                       (λ ()
                                         (map (λ (lang-position) "")
                                              lang-positions)))]
                 [reader-specs
                  (info-proc 'drscheme-language-readers
                             (λ ()
                               (map (λ (lang-position) #f)
                                    lang-positions)))])
            (cond
              [(and (list? lang-positions)
                    (andmap (λ (lang-position numbers)
                              (and (list? lang-position)
                                   (pair? lang-position)
                                   (andmap string? lang-position)
                                   (list? numbers)
                                   (andmap number? numbers)
                                   (= (length numbers)
                                      (length lang-position))))
                            lang-positions
                            numberss)
                    (list? lang-modules)
                    (andmap (λ (x)
                              (or (string? x)
                                  (and (list? x)
                                       (andmap string? x))))
                            lang-modules)
                    (list? summaries)
                    (andmap string? summaries)
                    
                    (list? urls)
                    (andmap string? urls)
                    
                    (list? reader-specs)
                    (andmap (λ (x)
                              ;; approximation (no good test, really)
                              ;; since it depends on the value of a mz
                              ;; parameter to interpret the module spec
                              (or (string? x) (eq? x #f) (symbol? x) (pair? x)))
                            reader-specs)
                    
                    (= (length lang-positions)
                       (length lang-modules)
                       (length summaries)
                       (length urls)
                       (length reader-specs)))
                               
               (for ([lang-module (in-list lang-modules)]
                     [lang-position (in-list lang-positions)]
                     [lang-numbers (in-list numberss)]
                     [one-line-summary (in-list summaries)]
                     [url (in-list urls)]
                     [reader-spec (in-list reader-specs)])
                 (define %
                   ((drracket:language:get-default-mixin)
                    (drracket:language:module-based-language->language-mixin
                     (drracket:language:simple-module-based-language->module-based-language-mixin
                      drracket:language:simple-module-based-language%))))
                 (define reader
                   (if reader-spec
                       (with-handlers ([exn:fail?
                                        (λ (x)
                                          (message-box (string-constant drscheme)
                                                       (if (exn? x)
                                                           (exn-message x)
                                                           (format "uncaught exception: ~s" x))
                                                       #:dialog-mixin frame:focus-table-mixin)
                                          read-syntax/namespace-introduce)])
                         (contract
                          (->* ()
                               (any/c port?)
                               (or/c syntax? eof-object?))
                          (dynamic-require
                           (cond
                             [(string? reader-spec)
                              (build-path
                               directory 
                               (platform-independent-string->path reader-spec))]
                             [else reader-spec])
                           'read-syntax)
                          (string->symbol (format "~s" lang-position))
                          'drscheme))
                       read-syntax/namespace-introduce))
                 (add-language (new %
                                    [module (if (string? lang-module)
                                                (build-path
                                                 directory 
                                                 (platform-independent-string->path lang-module))
                                                `(lib ,@lang-module))]
                                    [language-position lang-position]
                                    [language-id (format "plt:lang-from-module: ~s" lang-module)]
                                    [language-numbers lang-numbers]
                                    [one-line-summary one-line-summary]
                                    [language-url url]
                                    [reader reader])))]
              [else
               (message-box
                (string-constant drscheme)
                (format
                 (string-append
                  "The drscheme-language-position, drscheme-language-modules,"
                  " drscheme-language-numbers, and drscheme-language-readers specifications"
                  " aren't correct. Expected (listof (cons string (listof string))),"
                  " (listof (listof string)), (listof (listof number)), (listof string),"
                  " (listof string), and (listof module-spec) respectively, where the lengths"
                  " of the outer lists are the same. Got ~e, ~e, ~e, ~e, ~e, and ~e")
                 lang-positions
                 lang-modules
                 numberss
                 summaries
                 urls
                 reader-specs)
                #:dialog-mixin frame:focus-table-mixin)])))))
    
    (define (indirect-info-field proc name info-dir)
      (let ([mp (proc name (λ () #f))])
        (if (not mp)
            null
            (if (and (list? mp)
                     (= 2 (length mp))
                     (module-path? (car mp))
                     (symbol? (cadr mp)))
                (parameterize ([current-load-relative-directory info-dir])
                  (dynamic-require (car mp) (cadr mp)))
                (error
                 name
                 "expected either #f or a list containing a module path and symbol, bot ~e"
                 mp)))))
    
    (define (platform-independent-string->path str)
      (apply
       build-path
       (map (λ (x) 
              (cond
                [(string=? ".." x) 'up]
                [(string=? "." x) 'same]
                [else x]))
            (regexp-split #rx"/" str))))
    
    (define read-syntax/namespace-introduce
      (λ (source-name-v [input-port (current-input-port)])
        (let ([v (read-syntax source-name-v input-port)])
          (if (syntax? v)
              (namespace-syntax-introduce v)
              v))))
    
    

;             
;                                               
;                                               
;                                               
;                                               
;  ;;;             ;;; ;;;   ;      ;;;         
;  ;;;                 ;;; ;;;                  
;  ;;; ;;  ;;; ;;; ;;; ;;; ;;;;     ;;; ;;; ;;  
;  ;;;;;;; ;;; ;;; ;;; ;;; ;;;;     ;;; ;;;;;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;      ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;;;;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;;;;;;; ;;; ;;; 
;  ;;;;;;; ;;;;;;; ;;; ;;; ;;;;     ;;; ;;; ;;; 
;  ;;; ;;   ;; ;;; ;;; ;;;  ;;;     ;;; ;;; ;;; 
;                                               
;                                               
;                                               
;                                               
;                                                                     
;                                                                     
;                                                                     
;                                                                     
;  ;;;                                                                
;  ;;;                                                                
;  ;;;  ;;;;;  ;;; ;;   ;; ;;; ;;; ;;;  ;;;;;   ;; ;;;   ;;;;   ;;;;  
;  ;;; ;;;;;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;;;;;; ;;;;;;;  ;; ;;; ;;; ;; 
;  ;;; ;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;  ;;; ;;; ;;; ;;; ;;; ;;;    
;  ;;;   ;;;;; ;;; ;;; ;;; ;;; ;;; ;;;   ;;;;; ;;; ;;; ;;;;;;;  ;;;;  
;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;        ;;; 
;  ;;; ;;; ;;; ;;; ;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;;;;;;  ;;;;;; ;; ;;; 
;  ;;;  ;;;;;; ;;; ;;;  ;; ;;;  ;; ;;;  ;;;;;;  ;; ;;;   ;;;;   ;;;;  
;                          ;;;                     ;;;                
;                      ;;;;;;                  ;;;;;;                 
;                                                                     
;                                                                     
     
   
    
    ;; add-expand-to-front-end : mixin
    ;; overrides front-end to make the language a language that expands its arguments
    (define (add-expand-to-front-end %)
      (class %
        (define/override (front-end/complete-program input settings)
          (wrap-front-end (super front-end/complete-program input settings)))
        (define/override (front-end/interaction input settings)
          (wrap-front-end (super front-end/interaction input settings)))
        (define/private (wrap-front-end thnk)
          (λ ()
            (let ([res (thnk)])
              (cond
                [(syntax? res) (with-syntax ([res res]
                                             [expand-syntax-top-level-with-compile-time-evals
                                              expand-syntax-top-level-with-compile-time-evals])
                                 #'(expand-syntax-top-level-with-compile-time-evals
                                    (quote-syntax res)))]
                [(eof-object? res) res]
                [else `(expand ',res)]))))
        (super-instantiate ())))
    
    (define-struct (simple-settings+assume drracket:language:simple-settings) (no-redef?))
    (define simple-settings+assume->vector (make-->vector simple-settings+assume))

    (define (macro-stepper-mixin %)
      (class %
        (super-new)
        (define/augment (capability-value key)
          (cond
            [(eq? key 'macro-stepper:enabled) #t]
            [else (inner (drracket:language:get-capability-default key)
                         capability-value key)]))))

    (define (assume-mixin %)
      (class %
        (define/override (default-settings) 
          (extend-simple-settings (super default-settings) #t))

        (define/override (marshall-settings settings)
          (simple-settings+assume->vector settings))

        (define/override (unmarshall-settings printable)
          (and (vector? printable)
               (= (vector-length printable) 7)
               (let ([base
                      (super unmarshall-settings
                             (list->vector
                              (reverse
                               (cdr (reverse (vector->list printable))))))])
                 (and base
                      (extend-simple-settings
                       base
                       (and (vector-ref printable 6) #t))))))

        (define/override (config-panel parent)
          (let ([p (new vertical-panel% [parent parent])])
            (let ([base-config (super config-panel p)]
                  [assume-cb (new check-box%
                                  [parent 
                                   (new group-box-panel%
                                        [parent p]
                                        [label (string-constant enforce-primitives-group-box-label)]
                                        [stretchable-height #f]
                                        [stretchable-width #f])]
                                  [label (string-constant enforce-primitives-check-box-label)])])
              (case-lambda
               [() (extend-simple-settings (base-config)
                                           (send assume-cb get-value))]
               [(c)
                (base-config c)
                (send assume-cb set-value (simple-settings+assume-no-redef? c))]))))

        (define/override (default-settings? x)
          (equal? (simple-settings+assume->vector x)
                  (simple-settings+assume->vector (default-settings))))

        (define/private (extend-simple-settings s no-redef?)
          (make-simple-settings+assume (drracket:language:simple-settings-case-sensitive s)
                                       (drracket:language:simple-settings-printing-style s)
                                       (drracket:language:simple-settings-fraction-style s)
                                       (drracket:language:simple-settings-show-sharing s)
                                       (drracket:language:simple-settings-insert-newlines s)
                                       (drracket:language:simple-settings-annotations s)
                                       no-redef?))

        (define/override (use-namespace-require/copy-from-setting? s)
          (not (simple-settings+assume-no-redef? s)))

        (super-new)))

    (define (add-errortrace-key-mixin %)
      (class %
        (define/override (on-execute setting run-in-user-thread)
          (super on-execute setting run-in-user-thread)
          (run-in-user-thread
           (λ ()
             (namespace-require 'errortrace/errortrace-key)
             (namespace-require '(for-syntax errortrace/errortrace-key)))))
        (super-new)))
  
    (define (r5rs-mixin %)
      (class %
        (define/override (on-execute setting run-in-user-thread)
          (super on-execute setting run-in-user-thread)
          (run-in-user-thread
           (λ ()
             (read-square-bracket-as-paren #f)
             (read-curly-brace-as-paren #f)
             (read-accept-infix-dot #f)
             (print-mpair-curly-braces #f)
             (print-vector-length #f))))
        (define/override (get-transformer-module) #f)

        (define/override (default-settings) 
          (make-simple-settings+assume #f 'trad-write 'mixed-fraction-e #f #t 'debug #t))

        (super-new)))
  
    (define (pretty-big-mixin %)
      (class %
        ;; since check syntax no longer shares the gui libraries, 
        ;; we always share it explicitly here
        (define/override (on-execute setting run-in-user-thread)
          (let ([mred-name ((current-module-name-resolver) 'mred/mred #f #f #t)])
            (run-in-user-thread
             (λ ()
               (namespace-attach-module drracket:init:system-namespace mred-name))))
          (super on-execute setting run-in-user-thread))
        (define/override (default-settings) 
          (let ([s (super default-settings)])
            (make-simple-settings+assume (drracket:language:simple-settings-case-sensitive s)
                                         'trad-write
                                         (drracket:language:simple-settings-fraction-style s)
                                         (drracket:language:simple-settings-show-sharing s)
                                         (drracket:language:simple-settings-insert-newlines s)
                                         (drracket:language:simple-settings-annotations s)
                                         (simple-settings+assume-no-redef? s))))
        (super-new)))
    
    (define get-all-scheme-manual-keywords
      (let ([words #f])
        (λ ()
          (unless words
            (set! words (text:get-completions/manuals '(racket/base racket/contract))))
          words)))
    
    (define get-all-manual-keywords
      (let ([words #f])
        (λ ()
          (unless words
            (set! words (text:get-completions/manuals #f)))
          words)))
    
    ;; add-built-in-languages : -> void
    (define (add-built-in-languages)
      (let* ([words #f]
             [extras-mixin
              (λ (mred-launcher? one-line-summary)
                (λ (%)
                  (class* % (drracket:language:language<%>)
                    (define/override (get-one-line-summary) one-line-summary)
                    (inherit get-module get-transformer-module get-init-code
                             use-namespace-require/copy-from-setting?)
                    (define/override (front-end/interaction port settings)
                      (let ([t (super front-end/interaction port settings)])
                        (λ ()
                          (parameterize ([read-accept-lang #f])
                            (t)))))
                    (define/augment (capability-value key)
                      (cond
                        [(eq? key 'drscheme:autocomplete-words) 
                         (get-all-manual-keywords)]
                        [else (inner
                               (drracket:language:get-capability-default key)
                               capability-value key)]))
                    (super-new))))]
             [make-simple
              (λ (module id position numbers mred-launcher? one-line-summary extra-mixin)
                (define %
                  (extra-mixin
                   ((extras-mixin mred-launcher? one-line-summary)
                    ((drracket:language:get-default-mixin)
                     (drracket:language:module-based-language->language-mixin
                      (drracket:language:simple-module-based-language->module-based-language-mixin
                       drracket:language:simple-module-based-language%))))))
                (new % 
                     (module module)
                     (language-id id)
                     (language-position position)
                     (language-numbers numbers)))])
        (add-language
         (make-simple '(lib "lang/plt-pretty-big.rkt")
                      "plt:pretty-big"
                      (list (string-constant legacy-languages)
                            (string-constant pretty-big-scheme))
                      (list -200 3)
                      #t
                      (string-constant pretty-big-scheme-one-line-summary)
                      (λ (%) (pretty-big-mixin
                              (macro-stepper-mixin
                               (assume-mixin (add-errortrace-key-mixin %)))))))
        (add-language
         (make-simple '(lib "r5rs/lang.rkt")
                      "plt:r5rs"
                      (list (string-constant legacy-languages)
                            (string-constant r5rs-language-name))
                      (list -200 -1000)
                      #f
                      (string-constant r5rs-one-line-summary)
                      (lambda (%) (r5rs-mixin
                                   (macro-stepper-mixin
                                    (assume-mixin (add-errortrace-key-mixin %)))))))
        
        (add-language
         (make-simple 'racket/base
                      "plt:no-language-chosen"
                      (list (string-constant initial-language-category)
                            (string-constant no-language-chosen))
                      (list 10000 1000)
                      #f
                      "Helps the user choose an initial language"
                      not-a-language-extra-mixin))))
    
    (define (not-a-language-extra-mixin %)
      (class* % (not-a-language-language<%>)
        (define/override (get-style-delta) (drracket:rep:get-error-delta))
        
        (define/override (first-opened) 
          (not-a-language-message)
          (eprintf "\n"))
        
        (define/override (front-end/interaction input settings)
          (not-a-language-message)
          (λ () eof))
        (define/override (front-end/complete-program input settings)
          (not-a-language-message)
          (λ () eof))
        
        (define/augment (capability-value v)
          (case v
            [(drscheme:define-popup) #f]
            [(gui-debugger:debug-button) #f]
            [(macro-stepper:enabled) #f]
            [(drscheme:check-syntax-button) #f]
            [else (inner (drracket:language:get-capability-default v)
                         capability-value v)]))
        
        (super-new)))
    
    ;; used for identification only
    (define not-a-language-language<%>
      (interface ()))
    
    


;                                                                                                   
;                                                                                                   
;                                                                                                   
;                                                                                                   
;                    ;                  ;;;                                                         
;                  ;;;                  ;;;                                                         
;  ;;; ;;    ;;;   ;;;;      ;;;;;      ;;;  ;;;;;  ;;; ;;   ;; ;;; ;;; ;;;  ;;;;;   ;; ;;;   ;;;;  
;  ;;;;;;;  ;;;;;  ;;;;     ;;;;;;;     ;;; ;;;;;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;;;;;; ;;;;;;;  ;; ;;; 
;  ;;; ;;; ;;; ;;; ;;;      ;;  ;;;     ;;; ;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;  ;;; ;;; ;;; ;;; ;;; 
;  ;;; ;;; ;;; ;;; ;;;  ;;;;  ;;;;; ;;;;;;;   ;;;;; ;;; ;;; ;;; ;;; ;;; ;;;   ;;;;; ;;; ;;; ;;;;;;; 
;  ;;; ;;; ;;; ;;; ;;;  ;;;;;;; ;;; ;;;;;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;     
;  ;;; ;;;  ;;;;;  ;;;;     ;;; ;;;     ;;; ;;; ;;; ;;; ;;; ;;;;;;; ;;;;;;; ;;; ;;; ;;;;;;;  ;;;;;; 
;  ;;; ;;;   ;;;    ;;;      ;;;;;;     ;;;  ;;;;;; ;;; ;;;  ;; ;;;  ;; ;;;  ;;;;;;  ;; ;;;   ;;;;  
;                                                               ;;;                     ;;;         
;                                                           ;;;;;;                  ;;;;;;          
;                                                                                                   
;                                                                                                   
    
    
    (define (not-a-language-message)
      (define (main)
        (when (language-still-unchanged?)
          (o (green-snip (string-constant must-choose-language)))
          (o "\n")
          (o (green-snip (string-constant get-guidance-before)))
          (o (new link-snip%
                  [words (string-constant get-guidance-during)]
                  [callback (lambda (snip)
                              (not-a-language-dialog (find-parent-from-snip snip)))]))
          (o (green-snip (string-constant get-guidance-after)))))
      
      (define (green-snip str)
        (let ([snp (make-object string-snip% str)])
          (send snp set-style green-style)
          snp))
      
      (define green-style
        (let ([list (editor:get-standard-style-list)]
              [green-style-delta (make-object style-delta% 'change-family 'default)])
          (send green-style-delta set-delta-foreground "DarkViolet")
          (send green-style-delta set-delta 'change-italic)
          (send list
                find-or-create-style
                (send list find-named-style "Standard")
                green-style-delta)))
      
      (define (language-still-unchanged?)
        (let ([rep (drracket:rep:current-rep)])
          (cond
            [rep 
             (let* ([next-settings (send (send rep get-definitions-text) get-next-settings)]
                    [next-lang (language-settings-language next-settings)])
               (is-a? next-lang not-a-language-language<%>))]
            
            ;; if we cannot get the REP
            ;; (because a tool is processing the progrm like check syntax)
            ;; then just assume it has not changed.
            [else #t])))
      
      (define o
        (case-lambda
          [(arg) (cond [(string? arg) (eprintf arg)]
                       [(is-a? arg snip%) (write-special arg (current-error-port))])]
          [args (apply eprintf args)]))
      
      (define arrow-cursor (make-object cursor% 'arrow))
      
      (define link-snip%
        (class editor-snip%
          (init-field words callback)
          
          (define/override (adjust-cursor dc x y editorx editory event) arrow-cursor)
          
          (define/override (on-event dc x y editorx editory event)
            (when (send event button-up?)
              (callback this)))
          
          (define/override (copy)
            (new link-snip% [words words] [callback callback]))
          
          (define txt (new text:standard-style-list%))
          
          (super-new [editor txt] [with-border? #f]
                     [left-margin 0]
                     [right-margin 0]
                     [top-margin 0]
                     [bottom-margin 0])
          (inherit get-flags set-flags set-style)
          (set-flags (cons 'handles-events (get-flags)))
          
          (send txt insert words)
          (send txt change-style link-sd 0 (send txt last-position))))
      
      (define link-sd (make-object style-delta% 'change-underline #t))
      (send link-sd set-delta-foreground "blue")
      (send link-sd set-family 'default)
      
      (main))
    
    (define (not-a-language-dialog drs-frame)
      (define dialog (new dialog%
                          (parent drs-frame)
                          (label (string-constant drscheme))))
      (define top-hp (new horizontal-pane% [parent dialog]))
      (define qa-panel (new vertical-panel% 
                            [style '(border)]
                            [parent top-hp] 
                            [stretchable-width #f]))
      (define racketeer-panel (new vertical-panel% 
                                   [style '(border)]
                                   [parent top-hp]
                                   [alignment '(center center)]
                                   [stretchable-width #f]))
      (define button-panel (new horizontal-pane% 
                                (parent dialog) 
                                (stretchable-height #f)
                                (alignment '(right center))))
      
      (define cancel (new button%
                          (parent button-panel)
                          (callback (lambda (x y) (send dialog show #f)))
                          (label (string-constant cancel))))
      
      (define language-chosen? #f)
      
      (define (main)
        (insert-text-pls)
        (display-racketeer)
        (space-em-out)
        (fix-msg-sizes)
        (send dialog show #t))
      
      (define (insert-red-message)
        (new canvas-message% 
             (parent qa-panel)
             (font (get-font #:style 'italic))
             (label (string-constant must-choose-language))
             (color (send the-color-database find-color "red"))))
      
      (define (space-em-out)
        (send qa-panel change-children
              (lambda (l)
                (cond
                  [(null? l) l]
                  [else
                   (let loop ([x (car l)]
                              [r (cdr l)])
                     (cond
                       [(null? r) (list x)]
                       [else (list* x
                                    (new vertical-pane%
                                         (parent qa-panel)
                                         (min-height 5)
                                         (stretchable-height #f))
                                    (loop (car r)
                                          (cdr r)))]))]))))
      
      (define (insert-text-pls)
        (for-each
         display-text-pl
         (sort
          (apply append 
                 (map get-text-pls 
                      (find-relevant-directories '(textbook-pls get-textbook-pls))))
          (λ (x y)
            (cond
              [(string=? (cadr x) (string-constant how-to-design-programs))
               #t]
              [(string=? (string-constant how-to-design-programs) (cadr y))
               #f]
              [else
               (string<=? (cadr x) (cadr y))])))))
      
      (define (display-racketeer)
        (new canvas-message% 
             (parent racketeer-panel)
             (label (string-constant racketeer?)))
        (new canvas-message% 
             [label (read-bitmap (collection-file-path "plt-logo-red-shiny.png" "icons"))]
             [parent racketeer-panel]
             [callback (λ () (change-current-lang-to
                              (λ (x) (is-a? x drracket:module-language:module-language<%>))))])
        (new canvas-message%
             (parent racketeer-panel) 
             (label (string-constant use-language-in-source))
             (color (send the-color-database find-color "blue"))
             (callback (λ () (change-current-lang-to
                              (λ (x) (is-a? x drracket:module-language:module-language<%>)))))
             (font (get-font #:underlined #t))))
      
      (define (display-text-pl lst)
        (let ([icon-lst (car lst)]
              [text-name (cadr lst)]
              [lang (cddr lst)]
              [using-before (string-constant using-a-textbook-before)]
              [using-after (string-constant using-a-textbook-after)])
          (question/answer (lambda (parent)
                             (new canvas-message%
                                  (parent parent)
                                  (label using-before))
                             (new canvas-message%
                                  (parent parent)
                                  (font (get-font #:style 'italic))
                                  (label text-name))
                             (new canvas-message%
                                  (parent parent)
                                  (label using-after)))
                           (default-line2 (last lang) lang)
                           icon-lst)))
      
      (define default-font (send the-font-list find-or-create-font
                                 12
                                 'default
                                 'normal
                                 'normal))
      
      (define (get-font #:point-size [point-size (send default-font get-point-size)]
                        #:family (family (send default-font get-family))
                        #:style (style (send default-font get-style))
                        #:weight (weight (send default-font get-weight))
                        #:underlined (underlined (send default-font get-underlined))
                        #:smoothing (smoothing (send default-font get-smoothing)))
        (send the-font-list find-or-create-font
              point-size
              family
              style
              weight
              underlined
              smoothing))
      
      (define canvas-message%
        (class canvas%
          (init-field label
                      [font (get-font)]
                      [callback void]
                      [color (send the-color-database find-color "black")])
          
          (define/override (on-event evt)
            (cond
              [(send evt button-up?)
               (callback)]
              [else 
               (super on-event evt)]))
          
          (define/override (on-paint)
            (define dc (get-dc))
            (cond
              [(string? label)
               (define old-font (send dc get-font))
               (define old-tf (send dc get-text-foreground))
               (send dc set-text-foreground color)
               (send dc set-font font)
               (send dc draw-text label 0 0 #t)
               (send dc set-font old-font)
               (send dc set-text-foreground old-tf)]
              [(is-a? label bitmap%)
               (send dc draw-bitmap label 0 0)]))
          
          (super-new [stretchable-width #f]
                     [stretchable-height #f]
                     [style '(transparent)])
          
          (inherit min-width min-height get-dc)
          (cond
            [(string? label)
             (define-values (w h _1 _2) (send (get-dc) get-text-extent label font #t))
             (min-width (inexact->exact (ceiling w)))
             (min-height (inexact->exact (ceiling h)))]
            [(is-a? label bitmap%)
             (min-width (inexact->exact (ceiling (send label get-width))))
             (min-height (inexact->exact (ceiling (send label get-height))))])))
      
      (define (question/answer line1 line2 icon-lst)
        (display-two-line-choice 
         icon-lst
         (λ (panel1 panel2)
           (line1 panel1)
           (line2 panel2))))
      
      (define ((default-line2 lang-name lang) panel2)
        (new canvas-message% (parent panel2) (label (string-constant start-with-before)))
        (new canvas-message%
             (parent panel2) 
             (label lang-name)
             (color (send the-color-database find-color "blue"))
             (callback (λ () (change-current-lang-to lang)))
             (font (get-font #:underlined #t)))
        (new canvas-message% (parent panel2) (label (string-constant start-with-after))))
      
      ;; get-text-pls : path -> (listof (list* string string (listof string))
      ;; gets the questions from an info.rkt file.
      (define (get-text-pls info-dir)
        (define (check-text-pls qs)
          (unless (list? qs)
            (error 'textbook-pls "expected a list, got ~e" qs))
          (for-each 
           (lambda (pr)
             (unless (and (pair? pr)
                          (pair? (cdr pr))
                          (pair? (cddr pr))
                          (list? (cdddr pr))
                          (let ([icon-lst (car pr)])
                            (and (list? icon-lst)
                                 (not (null? icon-lst))
                                 (andmap string? icon-lst)))
                          (andmap string? (cdr pr)))
               (error 
                'textbook-pls
                (string-append
                 "expected a list of lists, with each inner list being at least three elements long"
                 " and the first element of the inner list being a list of strings and the rest of"
                 " the elements being strings, got ~e")
                pr)))
           qs)
          qs)
        (let ([proc (get-info/full info-dir)])
          (if proc
              (append
               (check-text-pls (proc 'textbook-pls (λ () '())))
               (check-text-pls
                (indirect-info-field proc 'get-textbook-pls info-dir)))
              '())))
      
      (define msgs '())
      (define (fix-msg-sizes)
        (let ([w (apply max (map (λ (x) (send x get-width)) msgs))])
          (for-each (λ (b) (send b min-width w))
                    msgs)))
      
      (define (display-two-line-choice icon-lst proc)
        (let* ([hp (new horizontal-pane% 
                        (parent qa-panel)
                        (alignment '(center top))
                        (stretchable-height #f))]
               [msg (new message%
                         (label (make-object bitmap%
                                  (apply collection-file-path icon-lst)
                                  'unknown/mask))
                         (parent hp))]
               [vp (new vertical-pane% 
                        (parent hp)
                        (alignment '(left top))
                        (stretchable-height #f))])
          (set! msgs (cons msg msgs))
          (proc (new horizontal-pane% (parent vp))
                (new horizontal-pane% (parent vp)))))
      
      ;; change-current-lang-to : (or/c (-> any/c boolean?) (listof string)) -> void
      ;; closed the guidance dialog and opens the language dialog
      (define (change-current-lang-to lang-strings/predicate)
        (send dialog show #f)
        (let* ([predicate (if (procedure? lang-strings/predicate)
                              lang-strings/predicate
                              (λ (x) (equal? lang-strings/predicate (send x get-language-position))))]
               [lang (ormap (λ (x) (and (predicate x) x))
                            (get-languages))])
          (unless lang
            (error 'change-current-lang-to "unknown language! ~s" lang-strings/predicate))
          
          (let ([new-lang
                 (language-dialog #f
                                  (language-settings lang
                                                     (send lang default-settings))
                                  drs-frame)])
            (when new-lang
              (set! language-chosen? #t)
              (preferences:set settings-preferences-symbol new-lang)
              (send (send drs-frame get-definitions-text) set-next-settings new-lang)))))
      
      (main))
    
    ;; find-parent-from-editor : editor -> (union frame #f)
    (define (find-parent-from-editor ed)
      (cond
        [(send ed get-canvas)
         =>
         (λ (c) (send c get-top-level-window))]
        [else
         (let ([admin (send ed get-admin)])
           (and (is-a? admin editor-snip-editor-admin<%>)
                (find-parent-from-snip (send admin get-snip))))]))
    
    ;; find-parent-from-snip : snip -> (union frame #f)
    (define (find-parent-from-snip snip)
      (let* ([admin (send snip get-admin)]
             [ed (send admin get-editor)])
        (find-parent-from-editor ed))))
