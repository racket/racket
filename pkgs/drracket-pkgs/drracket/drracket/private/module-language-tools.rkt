#lang racket/base
(provide module-language-tools@)
(require mrlib/switchable-button 
         mrlib/bitmap-label
         racket/contract
         racket/place
         framework
         racket/unit
         racket/class
         racket/gui/base
         drracket/private/drsig
         "local-member-names.rkt"
         framework/private/logging-timer)

(define op (current-output-port))
(define (oprintf . args) (apply fprintf op args))

(define-unit module-language-tools@
  (import [prefix drracket:unit: drracket:unit^]
          [prefix drracket:module-language: drracket:module-language/int^]
          [prefix drracket:language: drracket:language^]
          [prefix drracket:language-configuration: drracket:language-configuration^]
          [prefix drracket: drracket:interface^])
  (export drracket:module-language-tools/int^)

  (define-struct opt-out-toolbar-button (make-button id number) #:transparent)
  (define opt-out-toolbar-buttons '())
  
  (define (add-opt-out-toolbar-button make-button id #:number [number #f]) 
    (set! opt-out-toolbar-buttons
          (cons (make-opt-out-toolbar-button make-button id number)
                opt-out-toolbar-buttons)))
    
  (define-local-member-name
    set-lang-toolbar-buttons
    get-lang-toolbar-buttons
    get-online-expansion-monitor-pcs
    with-language-specific-default-extensions-and-filters)
    
  (define tab-mixin
    (mixin (drracket:unit:tab<%>) (drracket:module-language-tools:tab<%>)
      (inherit get-frame)
      (define toolbar-buttons '())
      (define/public (get-lang-toolbar-buttons) toolbar-buttons)
      (define/public (set-lang-toolbar-buttons bs ns)
        (for-each
         (λ (old-button) (send (get-frame) remove-toolbar-button old-button))
         toolbar-buttons)
        (set! toolbar-buttons bs)
        (send (get-frame) register-toolbar-buttons toolbar-buttons #:numbers ns)
        (send (get-frame) when-initialized
              (λ ()
                (send (send (get-frame) get-toolbar-button-panel) change-children
                      (λ (l) toolbar-buttons))))
        (send (get-frame) sort-toolbar-buttons-panel))
      (super-new)))
  
  (define frame-mixin
    (mixin (drracket:unit:frame<%>) (drracket:module-language-tools:frame<%>)
      (inherit unregister-toolbar-button 
               get-definitions-text
               sort-toolbar-buttons-panel)
      
      (define toolbar-button-panel #f)
      (define/public (when-initialized thunk) 
        (cond
          [toolbar-button-panel
           (thunk)]
          [else
           (set! after-initialized 
                 (let ([old-after-initialized after-initialized])
                   (λ () 
                     (old-after-initialized) 
                     (thunk))))]))
      (define after-initialized void)
      (define/public (get-toolbar-button-panel) toolbar-button-panel)
      (define/public (remove-toolbar-button button)
        (send toolbar-button-panel change-children (λ (l) (remq button l)))
        (unregister-toolbar-button button)
        (sort-toolbar-buttons-panel))
      (define/augment (on-tab-change old-tab new-tab)
        (inner (void) on-tab-change old-tab new-tab)
        (when toolbar-button-panel
          (send toolbar-button-panel change-children
                (λ (l) (send new-tab get-lang-toolbar-buttons)))
          (sort-toolbar-buttons-panel)))
      
      (define/override (file-menu:open-callback menu evt)
        (send (get-definitions-text) with-language-specific-default-extensions-and-filters
              (λ ()
                (super file-menu:open-callback menu evt))))
      
      (super-new)
      (inherit get-button-panel)
      (set! toolbar-button-panel (new panel:horizontal-discrete-sizes% 
                                      [parent (get-button-panel)]
                                      [alignment '(right center)]
                                      [stretchable-width #t]))
      (after-initialized)
      (set! after-initialized void)
      
      (define/public (initialize-module-language)
        (let ([defs (get-definitions-text)])
          (when (send defs get-in-module-language?)
            (send defs move-to-new-language))))))
  
  (define-logger drracket-language)
  
  (define definitions-text-mixin
    (mixin (text:basic<%> 
            drracket:unit:definitions-text<%>
            drracket:module-language:big-defs/ints-label<%>)
           (drracket:module-language-tools:definitions-text<%>)
      (inherit get-next-settings
               get-filename
               set-lang-wants-big-defs/ints-labels?
               get-tab)
      (define in-module-language? #f)      ;; true when we are in the module language
      (define hash-lang-last-location #f)  ;; non-false when we know where the hash-lang line ended
      (define hash-lang-language #f)       ;; non-false is the string that was parsed for the language
      (define/public (get-in-module-language?) in-module-language?)
      (define/augment (after-insert start len)
        (inner (void) after-insert start len)
        (modification-at start))
      (define/augment (after-delete start len)
        (inner (void) after-delete start len)
        (modification-at start))
      
      (define last-filename #f)
      (define/augment (after-save-file success?)
        (inner (void) after-save-file success?)
        (define this-filename (get-filename))
        (unless (equal? last-filename this-filename)
          (set! last-filename this-filename)
          (modification-at #f)))
      
      (define timer #f)
      
      ;; modification-at : (or/c #f number) -> void
      ;; checks to see if the lang line has changed when start
      ;; is in the region of the lang line, or when start is #f, or
      ;; when there is no #lang line known.
      (define/private (modification-at start)
        (send (send (get-tab) get-frame) when-initialized
              (λ ()
                (when in-module-language?
                  (when (or (not start)
                            (not hash-lang-last-location)
                            (<= start hash-lang-last-location))
                    
                    (unless timer
                      (set! timer (new logging-timer% 
                                       [notify-callback
                                        (λ () 
                                          (when in-module-language?
                                            (move-to-new-language)))]
                                       [just-once? #t])))
                    (send timer stop)
                    (send timer start 200 #t))))))

      (define/private (update-in-module-language? new-one)
        (unless (equal? new-one in-module-language?)
          (set! in-module-language? new-one)
          (cond
            [in-module-language? 
             (move-to-new-language)]
            [else
             (set! hash-lang-language #f)
             (set! hash-lang-last-location #f)
             (clear-things-out)])))
      
      (define/public (move-to-new-language)
        (define port (open-input-text-editor this))
        
        (define (fallback)
          ;; fall back to whatever #lang racket does if
          ;; we don't have a #lang line present in the file
          (log-drracket-language-debug "falling back to using #lang racket's read-language for ~a"
                                       (or (send this get-filename) "<<unsaved file>>"))
          (vector (read-language (open-input-string "#lang racket"))))
        
        ;; info-result : 
        ;;  (or/c #f   [#lang without a known language]
        ;;        (vector <get-info-proc>) 
        ;;                [no #lang line, so we use the '#lang racket' info proc]
        ;;        <get-info-proc>) [the get-info proc for the program in the definitions]
        (define info-result 
          (with-handlers ([exn:fail? 
                           (λ (x)
                             (log-drracket-language-debug
                              (format
                               "DrRacket: error duing call to read-language for ~a:\n  ~a"
                               (or (send this get-filename) "<<unsaved file>>")
                               (regexp-replace* #rx"\n(.)" (exn-message x) "\n\\1  ")))
                             #f)])
            (or (read-language port fallback)
                (fallback))))
        
        ; sometimes I get eof here, but I don't know why and can't seem to 
        ;; make it happen outside of DrRacket
        (when (eof-object? info-result)
          (eprintf "file ~s produces eof from read-language\n"
                   (send this get-filename))
          (eprintf "  port-next-location ~s\n"
                   (call-with-values (λ () (port-next-location port)) list))
          (eprintf "  str ~s\n"
                   (let ([s (send this get-text)])
                     (substring s 0 (min 100 (string-length s)))))
          (set! info-result #f))
        (define-values (line col pos) (port-next-location port))
        (unless (equal? (get-text 0 pos) hash-lang-language)
          (set! hash-lang-language (get-text 0 pos))
          (set! hash-lang-last-location pos)
          (clear-things-out)
          (define info-proc 
            (if (vector? info-result)
                (vector-ref info-result 0)
                info-result))
          (define (ctc-on-info-proc-result ctc res)
            (contract ctc 
                      res
                      (if (vector? info-result)
                          'hash-lang-racket
                          (get-lang-name pos))
                      'drracket/private/module-language-tools))
          
          (define lang-wants-big-defs/ints-labels?
            (and info-proc (info-proc 'drracket:show-big-defs/ints-labels #f)))
          (set-lang-wants-big-defs/ints-labels? lang-wants-big-defs/ints-labels?)
          (send (send (get-tab) get-ints) set-lang-wants-big-defs/ints-labels?
                lang-wants-big-defs/ints-labels?)
          
          (set! extra-default-filters
                (if info-result
                    (or (ctc-on-info-proc-result
                         (or/c #f (listof (list/c string? string?)))
                         (info-proc 'drracket:default-filters #f))
                        '())
                    '()))
          
          (set! default-extension
                (if info-result
                    (or (ctc-on-info-proc-result
                         (or/c #f (and/c string? (not/c #rx"[.]")))
                         (info-proc 'drracket:default-extension #f))
                        "")
                    ""))
          
          (when info-result
            (register-new-buttons
             (ctc-on-info-proc-result 
              (or/c #f (listof (or/c (list/c string?
                                             (is-a?/c bitmap%)
                                             (-> (is-a?/c drracket:unit:frame<%>) any))
                                     (list/c string?
                                             (is-a?/c bitmap%)
                                             (-> (is-a?/c drracket:unit:frame<%>) any)
                                             (or/c real? #f)))))
              (or (info-proc 'drracket:toolbar-buttons #f)
                  (info-proc 'drscheme:toolbar-buttons #f)))
             (ctc-on-info-proc-result 
              (or/c #f (listof symbol?))
              (or (info-proc 'drracket:opt-out-toolbar-buttons '())
                  (info-proc 'drscheme:opt-out-toolbar-buttons '())))))))
      
      
      (define/private (register-new-buttons buttons opt-out-ids)
        ;; cleaned-up-buttons : (listof (list/c string?
        ;;                                      (is-a?/c bitmap%) 
        ;;                                      (-> (is-a?/c drracket:unit:frame<%>) any) 
        ;;                                      (or/c real? #f)))
        (define cleaned-up-buttons
          (cond
            [(not buttons) '()]
            [else
             (for/list ([button (in-list buttons)])
               (if (= 3 (length button))
                   (append button (list #f))
                   button))]))
        (define tab (get-tab))
        (define frame (send tab get-frame))
        (send frame when-initialized
              (λ ()
                (send frame begin-container-sequence)
                
                ;; avoid any time with both sets of buttons in the
                ;; panel so the window doesn't get too wide
                (send (send frame get-toolbar-button-panel) change-children (λ (prev) '()))
                
                (define directly-specified-buttons
                  (map (λ (button-spec)
                         (new switchable-button%
                              [label (list-ref button-spec 0)]
                              [bitmap (list-ref button-spec 1)]
                              [parent (send frame get-toolbar-button-panel)]
                              [callback
                               (lambda (button)
                                 ((list-ref button-spec 2) frame))]))
                       cleaned-up-buttons))
                (define directly-specified-button-numbers 
                  (map (λ (button-spec) (list-ref button-spec 3)) 
                       cleaned-up-buttons))
                (define opt-out-buttons+numbers
                  (cond
                    [(eq? opt-out-ids #f) '()]
                    [else
                     (for/list ([opt-out-toolbar-button (in-list opt-out-toolbar-buttons)]
                                #:unless (member 
                                          (opt-out-toolbar-button-id opt-out-toolbar-button) 
                                          opt-out-ids))
                       (list ((opt-out-toolbar-button-make-button opt-out-toolbar-button) 
                              frame
                              (send frame get-toolbar-button-panel))
                             (opt-out-toolbar-button-number opt-out-toolbar-button)))]))
                (send tab set-lang-toolbar-buttons
                      (append directly-specified-buttons
                              (map (λ (x) (list-ref x 0)) opt-out-buttons+numbers))
                      (append directly-specified-button-numbers
                              (map (λ (x) (list-ref x 1)) opt-out-buttons+numbers)))
                (send frame end-container-sequence))))
      
      (inherit get-text)
      (define/private (get-lang-name pos)
        (cond
          [(zero? pos) '<<unknown>>]
          [else
           (let ([str (get-text 0 pos)])
             (if (char-whitespace? (string-ref str (- (string-length str) 1)))
                 (substring str 0 (- (string-length str) 1))
                 str))]))

      ;; removes language-specific customizations
      (define/private (clear-things-out)
        (send (get-tab) set-lang-toolbar-buttons '() '()))
      
      
      ;; online-expansion-monitor-table : hash[(cons mod-path id) -o> (cons/c local-pc remote-pc)]
      (define online-expansion-monitor-table (make-hash))
      (define/public (get-online-expansion-monitor-pcs an-online-expansion-handler)
        (define key (cons (online-expansion-handler-mod-path an-online-expansion-handler)
                          (online-expansion-handler-id an-online-expansion-handler)))
        (define old (hash-ref online-expansion-monitor-table key #f))
        (cond
          [old
           (values (car old) (cdr old))]
          [else
           (define-values (local-pc remote-pc) (place-channel))
           (hash-set! key (cons local-pc remote-pc))
           (values local-pc remote-pc)]))
      
      (define/augment (after-set-next-settings settings)
        (update-in-module-language?
         (is-a? (drracket:language-configuration:language-settings-language settings)
                drracket:module-language:module-language<%>))
        (inner (void) after-set-next-settings settings))
      
      (define/override (put-file dir default-name)
        (with-language-specific-default-extensions-and-filters
         (λ ()
           (super put-file dir default-name))))
      (define/override (get-file dir)
        (with-language-specific-default-extensions-and-filters
         (λ ()
           (super get-file dir))))
      
      (define extra-default-filters '())
      (define default-extension "")
      (define/public (with-language-specific-default-extensions-and-filters t)
        (parameterize ([finder:default-extension default-extension]
                       [finder:default-filters 
                        (append extra-default-filters (finder:default-filters))])
          (t)))
        
      
      (super-new)
      (set! in-module-language? 
            (is-a? (drracket:language-configuration:language-settings-language (get-next-settings))
                   drracket:module-language:module-language<%>))))
  
  
  (define no-more-online-expansion-handlers? #f)
  (define (no-more-online-expansion-handlers) (set! no-more-online-expansion-handlers? #t))
  (define-values (done done?)
    (let ()
      (struct done ())
      (values (done) done?)))
  (define-values (start start?)
    (let ()
      (struct start ())
      (values (start) start?)))
  ;; mod-path : module-path?
  ;; id : symbol?
  ;; local-handler : ... -> ...
  (struct online-expansion-handler (mod-path id local-handler monitor?))
  (define online-expansion-handlers '())
  (define (get-online-expansion-handlers)
    (cond
      [no-more-online-expansion-handlers?
       online-expansion-handlers]
      [else
       (error 'get-online-expansion-handlers 
              "online-expansion-handlers can still be registered")]))
  (define (add-online-expansion-handler mod-path id local-handler)
    (check-bad-registration 'add-online-expansion-handler mod-path id local-handler)
    (set! online-expansion-handlers
          (cons (online-expansion-handler mod-path id local-handler #f)
                online-expansion-handlers)))
  
  (define (add-online-expansion-monitor mod-path id local-handler)
    (check-bad-registration 'add-online-expansion-monitor mod-path id local-handler)
    (set! online-expansion-handlers
          (cons (online-expansion-handler mod-path id local-handler #t)
                online-expansion-handlers)))
      
  (define (check-bad-registration who mod-path id local-handler)
    (when no-more-online-expansion-handlers?
      (error who 
             "no more online-expansion-handlers can be registered; got ~e ~e ~e"
             mod-path id local-handler))
    (for ([handler (in-list online-expansion-handlers)])
      (when (and (equal? (online-expansion-handler-mod-path handler) mod-path)
                 (equal? (online-expansion-handler-id handler) id))
        (error who
               (string-append
                "already registered a handler with the same mod-path and id\n"
                " mod-path: ~e\n"
                " id: ~e")
               mod-path
               id))))
  
  (define online-expansion-pref-funcs '())
  (define (get-online-expansion-pref-funcs) online-expansion-pref-funcs)
    (define online-expansion-prefs '())
  (define (register-online-expansion-pref func)
    (set! online-expansion-pref-funcs (cons func online-expansion-pref-funcs))))
