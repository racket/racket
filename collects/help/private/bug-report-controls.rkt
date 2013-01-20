#lang racket/base
(require racket/gui/base
         racket/class
         racket/contract
         racket/pretty
         string-constants/string-constant
         setup/dirs
         setup/link
         framework
         (prefix-in planet2: planet2)
         (for-syntax racket/base
                     racket/list)
         "buginfo.rkt"
         "save-bug-report.rkt")

(provide/contract
 [add-bug-report-controls
  (-> (is-a?/c area-container<%>) 
      saved-report?
      (-> any)
      (-> any)
      (-> any)
      any)])

(define (add-bug-report-controls compose-panel init-bug-report ok cancel close-and-save)
  (define top-panel (make-object vertical-panel% compose-panel))
  
  (define lps null)
  
  ; build/label : ((union string (list-of string))
  ;                (area-container<%> -> item<%>) 
  ;                boolean
  ;                area-container<%>
  ;             -> item<%>)
  ; constructs and arranges the gui objects for the bug report form
  ; effect: updates lps with the new label panel, for future alignment
  (define (build/label text make-item top? #:stretch? [stretch? #f] #:top-panel [top-panel top-panel] #:vertical? [vertical? #f])
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
    item)
  
  (define (align-labels)
    (send synthesized-dialog reflow-container)
    (send compose-panel reflow-container)
    (let ([width (apply max (map (lambda (x) (send (car (send x get-children)) get-width))
                                 lps))])
      (for ([x (in-list lps)])
        (send x min-width width)))) 
  
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
          (define tf (new text-field%
                          [label #f]
                          [parent panel]
                          [callback (λ (a b) (bug-report-out-of-date))]))
          (send tf set-value (saved-report-lookup init-bug-report 'subject))
          tf)))
     #f))
  
  (define severity
    (build/label 
     (string-constant bug-report-field-severity) 
     (lambda (panel)
       (define choice
         (make-object choice% 
           #f
           bug-severities
           panel
           (λ (a b) (bug-report-out-of-date))))
       (send choice set-string-selection (saved-report-lookup init-bug-report 'severity))
       choice)
     #f))
  
  (define bug-class
    (build/label
     (string-constant bug-report-field-class)
     (lambda (panel)
       (define choice (make-object choice%
                        #f
                        (map car bug-classes)
                        panel
                        (λ (a b) (bug-report-out-of-date))))
       (send choice set-string-selection 
             (saved-report-lookup init-bug-report 'class))
       choice)
     #f))
  
  (define save-text%
    (class text:basic%
      (define initialized? #f)
      (define/public (initialized) (set! initialized? #t))
      (define/augment (after-insert a b)
        (when initialized?
          (bug-report-out-of-date))
        (inner (void) after-insert a b))
      (define/augment (after-delete a b)
        (when initialized?
          (bug-report-out-of-date))
        (inner (void) after-delete a b))
      (super-new)))
  
  (define (make-big-text label #:key [key #f] #:stretch? [stretch? #f] #:top-panel [top-panel top-panel] #:vertical? [vertical? #f])
    (let ([canvas 
           (build/label 
            label 
            (lambda (panel)
              (let* ([text (new (editor:standard-style-list-mixin
                                 (editor:keymap-mixin
                                  (if key
                                      save-text%
                                      text:basic%))))]
                     [canvas (new canvas:basic% 
                                  (style '(hide-hscroll))
                                  (parent panel) 
                                  (editor text))])
                (send text set-paste-text-only #t)
                (send text auto-wrap #t)
                (send text set-max-undo-history 'forever)
                (send text set-styles-fixed #t)
                (when key 
                  (send text insert (saved-report-lookup init-bug-report key))
                  (send text set-position 0 0)
                  (send text initialized))
                canvas))
            #t
            #:stretch? stretch?
            #:top-panel top-panel
            #:vertical? vertical?)])
      (send canvas min-width 500)
      (send canvas min-height 130)
      (send canvas get-editor)
      (send canvas allow-tab-exit #t)
      canvas))
  
  (define description (make-big-text (string-constant bug-report-field-description)
                                     #:key 'description
                                     #:stretch? #t))
  (define reproduce (make-big-text (list (string-constant bug-report-field-reproduce1)
                                         (string-constant bug-report-field-reproduce2))
                                   #:key 'how-to-repeat
                                   #:stretch? #t))
 
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
  
  (define version-tf
    (build/label
     (string-constant bug-report-field-version)
     (lambda (panel)
       (keymap:call/text-keymap-initializer
        (lambda ()
          (make-object text-field% #f panel void ""))))
     #f
     #:top-panel synthesized-panel))
  (define environment
    (build/label
     (string-constant bug-report-field-environment)
     (lambda (panel)
       (keymap:call/text-keymap-initializer
        (lambda ()
          (make-object text-field% #f panel void ""))))
     #f
     #:top-panel synthesized-panel))
  
  (define human-language
    (build/label 
     (string-constant bug-report-field-human-language)
     (lambda (panel)
       (keymap:call/text-keymap-initializer
        (lambda ()
          (make-object text-field% #f panel void ""))))
     #f
     #:top-panel synthesized-panel))
  
  (define memory-use
    (build/label 
     (string-constant bug-report-field-memory-use)
     (lambda (panel)
       (keymap:call/text-keymap-initializer
        (lambda ()
          (make-object text-field% #f panel void ""))))
     #f
     #:top-panel synthesized-panel))
  
  (define links-ctrl
    (build/label
     (string-constant bug-report-field-links)
     (lambda (panel)
       (keymap:call/text-keymap-initializer
        (lambda ()
          (make-object text-field% #f panel void ""))))
     #f
     #:top-panel synthesized-panel))
  
  (define planet2-info
    (make-big-text
     (string-constant bug-report-field-planet2)
     #:stretch? #t
     #:top-panel synthesized-panel))
  
  (define collections
    (make-big-text
     (string-constant bug-report-field-collections)
     #:stretch? #t
     #:top-panel synthesized-panel))
  
  (send synthesized-dialog reflow-container) ;; help out the editor by resizing the container to a reasonable width (and thus make word-wrapping easier)
  
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
               #:top-panel synthesized-panel))))
         (get-bug-report-infos)))
  
  (define still-save? #t)
  (define (no-more-saving) (set! still-save? #f))
  
  (define (save-this-bug-report)
    (when still-save?
      (save-bug-report 
       (saved-report-id init-bug-report)
       #:severity (send severity get-string-selection)
       #:class (send bug-class get-string-selection)
       #:subject (send summary get-value)
       #:description (get-content description)
       #:how-to-repeat (get-content reproduce))))
  
  (define timer 
    (new timer%
         [notify-callback save-this-bug-report]
         [just-once? #t]))
  (define (bug-report-out-of-date)
    (send timer stop)
    (send timer start 200 #t))
  
  (define (get-query)
    (append (list (cons 'help-desk "true")
                  (cons 'replyto (preferences:get 'drracket:email))
                  (cons 'originator (preferences:get 'drracket:full-name))
                  (cons 'subject (send summary get-value))
                  (cons 'severity (send severity get-string-selection))
                  (cons 'class (translate-class (send bug-class get-string-selection)))
                  (cons 'release (send version-tf get-value))
                  (cons 'description (get-content description))
                  (cons 'how-to-repeat (get-content reproduce))
                  (cons 'platform (get-environment)))
            (map (λ (bri) (cons (string->symbol (format "~a" (bri-label bri)))
                                (bri-value bri)))
                 (get-bug-report-infos))))
  
  (define (get-environment)
    (string-append (send environment get-value)
                   "\n"
                   (format "Human Language: ~a\n" (send human-language get-value))
                   (format "(current-memory-use) ~a\n" (send memory-use get-value))
                   (format "Links: ~a\n" (send links-ctrl get-value))
                   (format "Planet2 (show):\n~a\n" (send (send planet2-info get-editor) get-text))
                   "\n"
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
  
  (define (get-content canvas) 
    (define t (send canvas get-editor))
    (send t get-text 0 (send t last-position)))
  
  (define (set-content canvas str)
    (define t (send canvas get-editor))
    (send t begin-edit-sequence)
    (send t erase)
    (send t insert str)
    (send t end-edit-sequence))
  
  (define (compose-view-focus)
    (send (if (string=? "" (preferences:get 'drracket:full-name))
              name
              summary)
          focus))
  
  (define button-panel
    (new horizontal-panel% [parent compose-panel]
         [alignment '(right center)] [stretchable-height #f]))
  (define synthesized-button 
    (make-object button%
      (string-constant bug-report-show-synthesized-info)
      button-panel (lambda x (show-synthesized-info))))
  (new button% 
       [parent button-panel]
       [label (string-constant close-and-save)]
       [callback (λ (a b) (close-and-save))])
  (new horizontal-pane% (parent button-panel))
  (gui-utils:ok/cancel-buttons button-panel
                               (λ (a b) (ok))
                               (λ (a b) (cancel))
                               (string-constant bug-report-submit)
                               #:confirm-style '())
  (new grow-box-spacer-pane% [parent button-panel])
  
  
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
  
  (send version-tf set-value (format "~a" (version:version)))
  
  (send environment set-value
        (format "~a ~s (~a) (get-display-depth) = ~a"
                (system-type)
                (system-type 'machine)
                (system-library-subpath)
                (get-display-depth)))
  
  (send (send collections get-editor)
        insert
        (apply
         string-append
         (for/list ([d (get-collects-search-dirs)])
           (format "(~s\n ~s)\n"
                   (path->string d)
                   (if (directory-exists? d)
                       (map path->string (directory-list d))
                       '(non-existent-path))))))
  
  (define-syntax (links-calls stx)
    (syntax-case stx ()
      [(_ calls ...)
       (let ([str 
              (apply 
               string-append
               (add-between (map (λ (x) "~s = ~s")
                                 (syntax->list #'(calls ...)))
                            "; "))])
         #`(format #,str #,@(apply append (map (λ (x) (list #`'#,x x))
                                               (syntax->list #'(calls ...))))))]))
  (send (send links-ctrl get-editor)
        insert
        (links-calls (links)
                     (links #:user? #f)
                     (links #:root? #t)
                     (links #:user? #f #:root? #t)))
  
  (define planet2-info-sp (open-output-string))
  (parameterize ([current-output-port planet2-info-sp])
    (with-handlers ([exn:fail? (lambda (exn)
                                 (printf "ERROR:\n~a" (exn-message exn)))])
      (planet2:show)))
  (send (send planet2-info get-editor)
        insert
        (get-output-string planet2-info-sp))
  
  (send human-language set-value (format "~a" (this-language)))
  (send memory-use set-value (format "~a" (current-memory-use)))
  
  (send (send collections get-editor) auto-wrap #t)
  (align-labels)

  (define (empty-bug-report?)
    (define (empty-editor? c) 
      (define t (send c get-editor))
      (zero? (send t last-position)))
    (and (empty-editor? reproduce)
         (empty-editor? description)
         (empty-editor? summary)
         (equal? (send severity get-selection) default-severity)
         (equal? (send bug-class get-selection) default-class)))
  
  (values compose-view-focus
          get-query
          sanity-checking
          no-more-saving
          empty-bug-report?))
