#lang racket/unit

#|

There are three attributes for each preference:

  - default set, or not
  - marshalling function set, or not
  - initialization still okay, or not

the state transitions / contracts are:

  get(true, _, _) -> (true, _, false)
  get(false, _, _) -> error default not yet set

  set is just like get.

  set-default(false, _, true) -> set-default(true, _, true)
  set-default(true, _, _) -> error default already set
  set-default(_, _, false) -> initialization not okay anymore  /* cannot happen, I think */

  set-un/marshall(true, false, true) -> (true, true, true)
  .. otherwise error

  for all syms: 
   prefs-snapshot(_, _, _) -> (_, _, false) 

|#


  (require string-constants
	   mzlib/class
           racket/file
           "sig.rkt"
           "../gui-utils.rkt"
           "../preferences.rkt"
	   mred/mred-sig)
  
  (import mred^
          [prefix exit: framework:exit^]
          [prefix panel: framework:panel^]
          [prefix frame: framework:frame^])
  (export framework:preferences^)
  
  (define past-failure-ps '())
  (define past-failure-vs '())
  (define number-of-consecutive-failures 0)
  (define stop-warning? #f)
  
  (define get-pref-retry-result #f)
  
  (define (get-preference/gui sym [def (λ () (error 'get-preference/gui "unknown pref ~s" sym))])
    (define (try)
      (get-preference sym
                      def
                      #:timeout-lock-there
                      (λ (filename)
                        (define what-to-do
                          (cond
                            [get-pref-retry-result
                             get-pref-retry-result]
                            [else
                             (define-values (res dont-ask-again?)
                               (message+check-box/custom
                                (string-constant error-reading-preferences)
                                (format (string-constant error-reading-preferences-explanation)
                                        sym)
                                (string-constant dont-ask-again-until-drracket-restarted) ;; check label
                                (string-constant try-again)
                                (string-constant give-up-and-use-the-default)
                                #f
                                #f
                                '(caution default=1)
                                1)) ;; cannot return #f here or get-pref-retry-result may get set wrong
                             (when dont-ask-again?
                               (set! get-pref-retry-result res))
                             res]))
                        (case what-to-do
                          [(1) (try)]
                          [(2) (def)]))))
    (try))
                          
  
  (define put-pref-retry-result #f)
  
  (define (put-preferences/gui new-ps new-vs)
    ;; NOTE: old ones must come first in the list, 
    ;; or else multiple sets to the same preference
    ;; will save old values, instead of new ones.
    (define ps (begin0 (append past-failure-ps new-ps)
                       (set! past-failure-ps '())))
    (define vs (begin0 (append past-failure-vs new-vs)
                       (set! past-failure-vs '())))
    
    (define failed #f)
    (define (record-actual-failure)
      (set! number-of-consecutive-failures (+ number-of-consecutive-failures 1))
      (set! past-failure-ps ps)
      (set! past-failure-vs vs)
      (set! failed #t))
    (define (fail-func path)
      (cond
        [(= number-of-consecutive-failures 3)
         (set! number-of-consecutive-failures 0)
         (define the-mode (preferences-lock-file-mode)) 
         (define mb-ans
           (case the-mode
             [(file-lock) 
              (define-values (checked? res)
                (if put-pref-retry-result
                    (values #t 'ok)
                    (message+check-box
                     (string-constant error-saving-preferences-title)
                     (format (string-constant prefs-file-locked-nothing-doing)
                             path)
                     (string-constant dont-notify-again-until-drracket-restarted))))
              (when checked?
                (set! put-pref-retry-result #t))
               2]
             [(exists) 
              (message-box/custom
               (string-constant error-saving-preferences-title)
               (format (string-constant prefs-file-locked)
                       (path->string path))
               (string-constant steal-the-lock-and-retry)
               (string-constant cancel)
               #f
               #f ;;parent
               '(default=2 caution))]
             [else (error 'preferences.rkt "preferences-lock-file-mode returned unknown mode ~s\n" the-mode)]))
         (case mb-ans
           [(2 #f) (record-actual-failure)]
           [(1) 
            (let ([delete-failed #f])
              (with-handlers ((exn:fail:filesystem? (λ (x) (set! delete-failed x))))
                (delete-file path))
              (cond
                [delete-failed
                 (record-actual-failure)
                 (message-box 
                  (string-constant error-saving-preferences-title)
                  (exn-message delete-failed))]
                [else
                 (put-preferences ps vs second-fail-func)]))])]
        [else 
         (record-actual-failure)]))
    (define (second-fail-func path)
      (record-actual-failure)
      (message-box
       (string-constant error-saving-preferences-title)
       (format (string-constant prefs-file-still-locked)
               (path->string path))
       #f
       '(stop ok)))
    
    (with-handlers ((exn:fail? 
                     (λ (exn)
                       (log-warning (format "preferences: failed to save ~a prefs:\n   ~a" 
                                            ps
                                            (exn-message exn))))))
      (begin0
        (put-preferences ps vs fail-func)
        (unless failed
          (set! number-of-consecutive-failures 0)))))
  
  ;; ppanel-tree = 
  ;;  (union (make-ppanel-leaf string (union #f panel) (panel -> panel))
  ;;         (make-ppanel-interior string (union #f panel) (listof panel-tree)))
  (define-struct ppanel (name panel))
  (define-struct (ppanel-leaf ppanel) (maker))
  (define-struct (ppanel-interior ppanel) (children) #:mutable)
  
  ;; ppanels : (listof ppanel-tree)
  (define ppanels null)
  
  (define preferences-dialog #f)
  
  (define (add-panel title make-panel)
    (when preferences-dialog
      (error 'add-panel "preferences dialog already open, cannot add new panels"))
    (let ([titles (if (string? title)
                      (list title)
                      title)])
      (add-to-existing-children
       titles 
       make-panel
       (λ (new-ppanels) (set! ppanels new-ppanels)))))
  
  ;; add-to-existing-children : (listof string) (panel -> panel) (ppanel -> void)
  ;; adds the child specified by the path in-titles to the tree.
  (define (add-to-existing-children in-titles make-panel banger)
    (let loop ([children ppanels]
               [title (car in-titles)]
               [titles (cdr in-titles)]
               [banger banger])
      (cond
        [(null? children)
         (banger (list (build-new-subtree (cons title titles) make-panel)))]
        [else
         (let ([child (car children)])
           (if (string=? (ppanel-name child) title)
               (cond
                 [(null? titles) 
                  (error 'add-child "child already exists with this path: ~e" in-titles)]
                 [(ppanel-leaf? child)
                  (error 'add-child "new child's path conflicts with existing path: ~e" in-titles)]
                 [else
                  (loop
                   (ppanel-interior-children child)
                   (car titles)
                   (cdr titles)
                   (λ (children)
                     (set-ppanel-interior-children! 
                      child
                      children)))])
               (loop 
                (cdr children)
                title
                titles
                (λ (children)
                   (banger (cons child children))))))])))

  ;; build-new-subtree : (cons string (listof string)) (panel -> panel) -> ppanel
  (define (build-new-subtree titles make-panel)
    (let loop ([title (car titles)]
               [titles (cdr titles)])
      (cond
        [(null? titles) (make-ppanel-leaf title #f make-panel)]
        [else
         (make-ppanel-interior 
          title
          #f
          (list (loop (car titles) (cdr titles))))])))
  
  
  (define (hide-dialog)
    (when preferences-dialog
      (send preferences-dialog close)))
  
  (define (show-dialog)
    (if preferences-dialog
        (send preferences-dialog show #t)
        (set! preferences-dialog
              (make-preferences-dialog))))
  
  (define (add-can-close-dialog-callback cb)
    (set! can-close-dialog-callbacks
          (cons cb can-close-dialog-callbacks)))
  
  (define (add-on-close-dialog-callback cb)
    (set! on-close-dialog-callbacks
          (cons cb on-close-dialog-callbacks)))
  
  (define on-close-dialog-callbacks null)
  
  (define can-close-dialog-callbacks null)
  
  (define (make-preferences-dialog)
    (letrec ([stashed-prefs (preferences:get-prefs-snapshot)]
             [cancelled? #f]
             [frame-stashed-prefs%
              (class frame:basic%
                (inherit close)
                (define/override (on-subwindow-char receiver event)
                  (cond
                    [(eq? 'escape (send event get-key-code))
                     (set! cancelled? #t)
                     (close)]
                    [else 
                     (super on-subwindow-char receiver event)]))
                (define/augment (on-close)
                  (when cancelled?
                    (preferences:restore-prefs-snapshot stashed-prefs)))
                (define/override (show on?)
                  (when on?
                    ;; reset the flag and save new prefs when the window becomes visible
                    (set! cancelled? #f)
                    (set! stashed-prefs (preferences:get-prefs-snapshot)))
                  (super show on?))
                (super-new))]
             [frame 
              (new frame-stashed-prefs%
                   [label (string-constant preferences)]
                   [height 200])]
             [build-ppanel-tree
              (λ (ppanel tab-panel single-panel)
                (send tab-panel append (ppanel-name ppanel))
                (cond
                  [(ppanel-leaf? ppanel) 
                   ((ppanel-leaf-maker ppanel) single-panel)]
                  [(ppanel-interior? ppanel)
                   (let-values ([(tab-panel single-panel) (make-tab/single-panel single-panel #t)])
                     (for-each
                      (λ (ppanel) (build-ppanel-tree ppanel tab-panel single-panel))
                      (ppanel-interior-children ppanel)))]))]
             [make-tab/single-panel 
              (λ (parent inset?)
                (letrec ([spacer (and inset?
                                      (instantiate vertical-panel% ()
                                        (parent parent)
                                        (border 10)))]
                         [tab-panel (instantiate tab-panel% ()
                                      (choices null)
                                      (parent (if inset? spacer parent))
                                      (callback (λ (_1 _2) 
                                                  (tab-panel-callback
                                                   single-panel
                                                   tab-panel))))]
                         [single-panel (instantiate panel:single% ()
                                         (parent tab-panel))])
                  (values tab-panel single-panel)))]
             [tab-panel-callback
              (λ (single-panel tab-panel)
                (send single-panel active-child
                      (list-ref (send single-panel get-children)
                                (send tab-panel get-selection))))]
             [panel (make-object vertical-panel% (send frame get-area-container))]
             [_ (let-values ([(tab-panel single-panel) (make-tab/single-panel panel #f)])
                  (for-each
                   (λ (ppanel)
                     (build-ppanel-tree ppanel tab-panel single-panel))
                   ppanels)
                  (let ([single-panel-children (send single-panel get-children)])
                    (unless (null? single-panel-children)
                      (send single-panel active-child (car single-panel-children))
                      (send tab-panel set-selection 0)))
                  (send tab-panel focus))]
             [bottom-panel (make-object horizontal-panel% panel)]
             [ok-callback (λ args
                            (when (andmap (λ (f) (f))
                                          can-close-dialog-callbacks)
                              (for-each
                               (λ (f) (f))
                               on-close-dialog-callbacks)
                              (send frame close)))]
             [cancel-callback (λ () 
                                (set! cancelled? #t)
                                (send frame close))])
      (new button%
           [label (string-constant revert-to-defaults)]
           [callback
            (λ (a b)
              (preferences:restore-defaults))]
           [parent bottom-panel])
      (new horizontal-panel% [parent bottom-panel]) ;; spacer
      (gui-utils:ok/cancel-buttons
       bottom-panel
       ok-callback
       (λ (a b) (cancel-callback))
       (string-constant ok)
       (string-constant undo-changes))
      (make-object grow-box-spacer-pane% bottom-panel)
      (send* bottom-panel
        (stretchable-height #f)
        (set-alignment 'right 'center))
      (send frame show #t)
      frame))
  
  (define (add-to-scheme-checkbox-panel f)
    (set! scheme-panel-procs 
          (let ([old scheme-panel-procs])
            (λ (parent) (old parent) (f parent)))))
  
  (define (add-to-editor-checkbox-panel f)
    (set! editor-panel-procs 
          (let ([old editor-panel-procs])
            (λ (parent) (old parent) (f parent)))))
  
  (define (add-to-general-checkbox-panel f)
    (set! general-panel-procs 
          (let ([old general-panel-procs])
            (λ (parent) (old parent) (f parent)))))
  
  (define (add-to-warnings-checkbox-panel f)
    (set! warnings-panel-procs 
          (let ([old warnings-panel-procs])
            (λ (parent) (old parent) (f parent)))))
  
  (define scheme-panel-procs void)
  (define editor-panel-procs void)
  (define general-panel-procs void)
  (define warnings-panel-procs void)
  
  (define (add-checkbox-panel label proc)
    (add-panel
     label
     (λ (parent)
       (let* ([main (make-object vertical-panel% parent)])
         (send main set-alignment 'left 'center)
         (proc main)
         main))))
  
  ;; add-check : panel symbol string (boolean -> any) (any -> boolean) -> void
  ;; adds a check box preference to `main'.
  (define (add-check main pref title [bool->pref values] [pref->bool values])
    (let* ([callback
            (λ (check-box _)
              (preferences:set pref (bool->pref (send check-box get-value))))]
           [pref-value (preferences:get pref)]
           [initial-value (pref->bool pref-value)]
           [c (make-object check-box% title main callback)])
      (send c set-value initial-value)
      (preferences:add-callback
       pref
       (λ (p v)
         (send c set-value (pref->bool v))))
      (void)))
  
  (define (make-recent-items-slider parent)
    (let ([slider (instantiate slider% ()
                    (parent parent)
                    (label (string-constant number-of-open-recent-items))
                    (min-value 1)
                    (max-value 100)
                    (init-value (preferences:get 'framework:recent-max-count))
                    (callback (λ (slider y)
                                (preferences:set 'framework:recent-max-count
                                                 (send slider get-value)))))])
      (preferences:add-callback
       'framework:recent-max-count
       (λ (p v)
         (send slider set-value v)))))
  
  (define (add-scheme-checkbox-panel)
    (letrec ([add-scheme-checkbox-panel
              (λ ()
                (set! add-scheme-checkbox-panel void)
                (add-checkbox-panel
                 (list 
                  (string-constant editor-prefs-panel-label) 
                  (string-constant scheme-prefs-panel-label))
                 (λ (scheme-panel)
                   (add-check scheme-panel
                              'framework:highlight-parens
                              (string-constant highlight-parens)
                              values values)
                   (add-check scheme-panel
                              'framework:fixup-parens
                              (string-constant fixup-close-parens)
                              values values)
                   (add-check scheme-panel
                              'framework:fixup-open-parens
                              (string-constant fixup-open-brackets)
                              values values)
                   (add-check scheme-panel
                              'framework:paren-match
                              (string-constant flash-paren-match)
                              values values)
                   (scheme-panel-procs scheme-panel))))])
      (add-scheme-checkbox-panel)))
  
  (define (add-editor-checkbox-panel)
    (letrec ([add-editor-checkbox-panel
              (λ ()
                (set! add-editor-checkbox-panel void)
                (add-checkbox-panel 
                 (list (string-constant editor-prefs-panel-label) 
                       (string-constant general-prefs-panel-label))
                 (λ (editor-panel)
                   (add-check editor-panel 'framework:delete-forward? (string-constant map-delete-to-backspace)
                              not not)
                   (add-check editor-panel 
                              'framework:auto-set-wrap?
                              (string-constant wrap-words-in-editor-buffers))
                   
                   (add-check editor-panel 
                              'framework:menu-bindings
                              (string-constant enable-keybindings-in-menus))
                   (when (memq (system-type) '(macosx))
                     (add-check editor-panel 
                                'framework:alt-as-meta
                                (string-constant alt-as-meta))
                     (add-check editor-panel 
                                'framework:special-meta-key
                                (string-constant command-as-meta)))
                   
                   (add-check editor-panel 
                              'framework:coloring-active
                              (string-constant online-coloring-active))
                   
                   (add-check editor-panel
                               'framework:anchored-search
                               (string-constant find-anchor-based))
                   (add-check editor-panel
                              'framework:do-paste-normalization
                              (string-constant normalize-string-preference))
                   (add-check editor-panel
                               'framework:overwrite-mode-keybindings
                               (string-constant enable-overwrite-mode-keybindings))
                   (add-check editor-panel
                               'framework:automatic-parens
                               (string-constant enable-automatic-parens))
                   (when (eq? (system-type) 'windows)
                     (add-check editor-panel
                                 'framework:always-use-platform-specific-linefeed-convention
                                 (string-constant always-use-platform-specific-linefeed-convention)))
                   (add-check editor-panel
                              'framework:line-spacing-add-gap?
                              (string-constant add-spacing-between-lines))
                   
                   (let ([hp (new horizontal-panel% [parent editor-panel] [stretchable-height #f])]
                         [init-pref (preferences:get 'framework:column-guide-width)])
                     (define on-cb
                       (new check-box% 
                            [parent hp]
                            [label (string-constant maximum-char-width-guide-pref-check-box)]
                            [value (car init-pref)]
                            [callback
                             (λ (x y)
                               (update-pref)
                               (update-tf-bkg)
                               (send tf enable (send on-cb get-value)))]))
                     (define tf 
                       (new text-field%
                            [label #f]
                            [parent hp]
                            [init-value (format "~a" (cadr init-pref))]
                            [callback
                             (λ (x y)
                               (update-pref)
                               (update-tf-bkg))]))
                     (define (update-tf-bkg)
                       (send tf set-field-background
                             (send the-color-database find-color 
                                   (cond
                                     [(not (send on-cb get-value)) "gray"]
                                     [(good-val? (string->number (send tf get-value)))
                                      "white"]
                                     [else
                                      "yellow"]))))
                     (define (good-val? n)
                       (and (exact-integer? n)
                            (>= n 2)))
                     (define (update-pref)
                       (define current (preferences:get 'framework:column-guide-width))
                       (define candidate-num (string->number (send tf get-value)))
                       (preferences:set 'framework:column-guide-width
                                        (list (send on-cb get-value)
                                              (if (good-val? candidate-num)
                                                  candidate-num
                                                  (cadr current)))))
                     (update-tf-bkg))
                   
                   (editor-panel-procs editor-panel))))])
      (add-editor-checkbox-panel)))
  
  (define (add-general-checkbox-panel)
    (letrec ([add-general-checkbox-panel
              (λ ()
                (set! add-general-checkbox-panel void)
                (add-checkbox-panel 
                 (list (string-constant general-prefs-panel-label))
                 (λ (editor-panel)
                   (make-recent-items-slider editor-panel)
                   (add-check editor-panel
                              'framework:autosaving-on? 
                              (string-constant auto-save-files))
                   (add-check editor-panel 'framework:backup-files? (string-constant backup-files))
                   (add-check editor-panel 'framework:show-status-line (string-constant show-status-line))
                   ;; does this not belong here?
                   ;; (add-check editor-panel 'drracket:show-line-numbers (string-constant show-line-numbers)
                   (add-check editor-panel 'framework:col-offsets (string-constant count-columns-from-one))
                   (add-check editor-panel 
                              'framework:display-line-numbers
                              (string-constant display-line-numbers))
                   (define print-rb (new radio-box% 
                                         [label (string-constant printing-mode)]
                                         [parent editor-panel]
                                         [choices (list (string-constant print-using-platform-specific-mode)
                                                        (string-constant print-to-ps)
                                                        (string-constant print-to-pdf))]
                                         [callback
                                          (λ (rb evt)
                                            (preferences:set 'framework:print-output-mode
                                                             (case (send print-rb get-selection)
                                                               [(0) 'standard]
                                                               [(1) 'postscript]
                                                               [(2) 'pdf])))]))
                   (define (update-print-rb what)
                     (send print-rb set-selection (case what
                                                    [(standard) 0]
                                                    [(postscript) 1]
                                                    [(pdf) 2])))
                   (update-print-rb (preferences:get 'framework:print-output-mode))
                   (preferences:add-callback 'framework:print-output-mode (λ (p v) (update-print-rb v)))
                   (general-panel-procs editor-panel))))])
      (add-general-checkbox-panel)))
  
  (define (add-warnings-checkbox-panel)
    (letrec ([add-warnings-checkbox-panel
              (λ ()
                (set! add-warnings-checkbox-panel void)
                (add-checkbox-panel
                 (string-constant warnings-prefs-panel-label)
                 (λ (warnings-panel)
                   (add-check warnings-panel 
                              'framework:verify-change-format 
                              (string-constant ask-before-changing-format))
                   (add-check warnings-panel 
                              'framework:verify-exit
                              (string-constant verify-exit))
                   (add-check warnings-panel
                              'framework:ask-about-paste-normalization
                              (string-constant ask-about-normalizing-strings))
                   (warnings-panel-procs warnings-panel))))])
      (add-warnings-checkbox-panel)))
  
  (define (local-add-font-panel)
    (let* ([font-families-name/const
            (list (list "Default" 'default)
                  (list "Decorative" 'decorative)
                  (list "Modern" 'modern)
                  (list "Roman" 'roman)
                  (list "Script" 'script)
                  (list "Swiss" 'swiss))]
           
           [font-families (map car font-families-name/const)]
           
           [font-size-entry "defaultFontSize"]
           [font-default-string "Default Value"]
           [font-default-size (case (system-type)
                                [(windows) 10]
                                [(macosx) 13]
                                [else 12])]
           [font-section "mred"]
           [build-font-entry (λ (x) (string-append "Screen" x "__"))]
           [build-font-preference-symbol
            (λ (family)
              (string->symbol (string-append "framework:" family)))]
           
           [set-default
            (λ (build-font-entry default pred)
              (λ (family)
                (let ([name (build-font-preference-symbol family)]
                      [font-entry (build-font-entry family)])
                  (preferences:set-default
                   name
                   default
                   (cond
                     [(string? default) string?]
                     [(number? default) number?]
                     [else (error 'internal-error.set-default "unrecognized default: ~a\n" default)])))))])
      
      (for-each (set-default build-font-entry font-default-string string?)
                font-families)
      ((set-default (λ (x) x)
                    font-default-size
                    number?)
       font-size-entry)
      (add-panel
       (string-constant default-fonts)
       (λ (parent)
         (letrec ([font-size-pref-sym (build-font-preference-symbol font-size-entry)]
                  [ex-string (string-constant font-example-string)]
                  [main (make-object vertical-panel% parent)]
                  [fonts (cons font-default-string (get-face-list))]
                  [make-family-panel
                   (λ (name)
                     (let* ([pref-sym (build-font-preference-symbol name)]
                            [family-const-pair (assoc name font-families-name/const)]
                            
                            [edit (make-object text%)]
                            [_ (send edit insert ex-string)]
                            [set-edit-font
                             (λ (size)
                               (let ([delta (make-object style-delta% 'change-size size)]
                                     [face (preferences:get pref-sym)])
                                 (if (and (string=? face font-default-string)
                                          family-const-pair)
                                     (send delta set-family (cadr family-const-pair))
                                     (send delta set-delta-face (preferences:get pref-sym)))
                                 
                                 (send edit change-style delta 0 (send edit last-position))))]
                            
                            [horiz (make-object horizontal-panel% main '(border))]
                            [label (make-object message% name horiz)]
                            
                            [message (make-object message%
                                       (let ([b (box "")])
                                         font-default-string)
                                       horiz)]
                            [button 
                             (make-object button%
                               (string-constant change-font-button-label)
                               horiz
                               (λ (button evt)
                                 (let ([new-value
                                        (get-choices-from-user
                                         (string-constant fonts)
                                         (format (string-constant choose-a-new-font)
                                                 name)
                                         fonts)])
                                   (when new-value
                                     (preferences:set pref-sym (list-ref fonts (car new-value))) 
                                     (set-edit-font (preferences:get font-size-pref-sym))))))]
                            [canvas (make-object editor-canvas% horiz
                                      edit
                                      (list 'hide-hscroll
                                            'hide-vscroll))])
                       (set-edit-font (preferences:get font-size-pref-sym))
                       (preferences:add-callback
                        pref-sym
                        (λ (p new-value)
                          (send horiz change-children
                                (λ (l)
                                  (let ([new-message (make-object message%
                                                       new-value
                                                       horiz)])
                                    (set! message new-message)
                                    (update-message-sizes font-message-get-widths 
                                                          font-message-user-min-sizes)
                                    (list label 
                                          new-message
                                          button
                                          canvas))))))
                       (send canvas set-line-count 1)
                       (vector set-edit-font
                               (λ () (send message get-width))
                               (λ (width) (send message min-width width))
                               (λ () (send label get-width))
                               (λ (width) (send label min-width width)))))]
                  [set-edit-fonts/messages (map make-family-panel font-families)]
                  [collect (λ (n) (map (λ (x) (vector-ref x n))
                                       set-edit-fonts/messages))]
                  [set-edit-fonts (collect 0)]
                  [font-message-get-widths (collect 1)]
                  [font-message-user-min-sizes (collect 2)]
                  [category-message-get-widths (collect 3)]
                  [category-message-user-min-sizes (collect 4)]
                  [update-message-sizes
                   (λ (gets sets)
                     (let ([width (foldl (λ (x l) (max l (x))) 0 gets)])
                       (for-each (λ (set) (set width)) sets)))]
                  [size-panel (make-object horizontal-panel% main '(border))]
                  [initial-font-size
                   (let ([b (box 0)])
                     font-default-size)]
                  [size-slider
                   (make-object slider%
                     (string-constant font-size-slider-label)
                     1 127
                     size-panel
                     (λ (slider evt)
                       (preferences:set font-size-pref-sym (send slider get-value)))
                     initial-font-size)])
           (update-message-sizes font-message-get-widths font-message-user-min-sizes)
           (update-message-sizes category-message-get-widths category-message-user-min-sizes)
           (preferences:add-callback
            font-size-pref-sym
            (λ (p value)
              (for-each (λ (f) (f value)) set-edit-fonts)
              (unless (= value (send size-slider get-value))
                (send size-slider set-value value))
              #t))
           (for-each (λ (f) (f initial-font-size)) set-edit-fonts)
           (make-object message% (string-constant restart-to-see-font-changes) main)
           main))))
    (set! local-add-font-panel void))
  
  (define (add-font-panel) (local-add-font-panel))
