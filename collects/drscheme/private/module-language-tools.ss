#lang scheme/base
(provide module-language-tools@)
(require mrlib/switchable-button 
         mrlib/bitmap-label
         scheme/contract
         framework
         scheme/unit
         scheme/class
         scheme/gui/base
         "drsig.ss")

(define-unit module-language-tools@
  (import [prefix drscheme:unit: drscheme:unit^]
          [prefix drscheme:module-language: drscheme:module-language^]
          [prefix drscheme:language: drscheme:language^]
          [prefix drscheme:language-configuration: drscheme:language-configuration^])
  (export drscheme:module-language-tools^)

  (define-local-member-name initialized? move-to-new-language)
  
  (define tab<%> (interface ()))
  
  (define tab-mixin
    (mixin (drscheme:unit:tab<%>) (tab<%>)
      (inherit get-frame)
      (define toolbar-buttons '())
      (define/public (get-lang-toolbar-buttons) toolbar-buttons)
      (define/public (set-lang-toolbar-buttons bs)
        (for-each
         (λ (old-button) (send (get-frame) remove-toolbar-button old-button))
         toolbar-buttons)
        (set! toolbar-buttons bs)
        (send (get-frame) register-toolbar-buttons toolbar-buttons))
      (super-new)))
  
  (define frame<%> (interface ()))
  (define frame-mixin
    (mixin (drscheme:unit:frame<%>) (frame<%>)
      (inherit unregister-toolbar-button get-definitions-text)
  
      (define toolbar-button-panel #f)
      (define/public (initialized?) (and toolbar-button-panel #t))
      (define/public (get-toolbar-button-panel) toolbar-button-panel)
      (define/public (remove-toolbar-button button)
        (send toolbar-button-panel change-children (λ (l) (remq button l)))
        (unregister-toolbar-button button))
      (define/augment (on-tab-change old-tab new-tab)
        (inner (void) on-tab-change old-tab new-tab)
        (when toolbar-button-panel
          (send toolbar-button-panel change-children
                (λ (l) (send new-tab get-lang-toolbar-buttons)))))
      (super-new)
      (inherit get-button-panel)
      (set! toolbar-button-panel (new horizontal-panel% 
                                      [parent (get-button-panel)]
                                      [stretchable-width #f]))
      ;; move button panel to the front of the list
      (send (get-button-panel) change-children 
            (λ (l) (cons toolbar-button-panel (remq toolbar-button-panel l))))
      (send (get-definitions-text) move-to-new-language)))
  
  (define definitions-text<%> (interface ()))
  (define definitions-text-mixin
    (mixin (text:basic<%> drscheme:unit:definitions-text<%>) (definitions-text<%>)
      (inherit get-next-settings)
      (define in-module-language? #f)      ;; true when we are in the module language
      (define hash-lang-last-location #f)  ;; non-false when we know where the hash-lang line ended
      (define hash-lang-language #f)       ;; non-false is the string that was parsed for the language
      (define/augment (after-insert start len)
        (inner (void) after-insert start len)
        (modification-at start))
      (define/augment (after-delete start len)
        (inner (void) after-delete start len)
        (modification-at start))
      (define/private (modification-at start)
        (when (send (send (get-tab) get-frame) initialized?) 
          (when in-module-language?
            (when (or (not hash-lang-last-location)
                      (<= start hash-lang-last-location))
              (move-to-new-language)))))
      
      (define/private (update-in-module-language? new-one)
        (unless (equal? new-one in-module-language?)
          (set! in-module-language? new-one)
          (cond
            [in-module-language? 
             (move-to-new-language)]
            [else
             (clear-things-out)])))
      
      (define/public (move-to-new-language)
        (let* ([port (open-input-text-editor this)]
               [info-result (with-handlers ((exn:fail? (λ (x) #f)))
                              (read-language port (λ () #f)))])
          (let-values ([(line col pos) (port-next-location port)])
            (unless (equal? (get-text 0 pos) hash-lang-language)
              (set! hash-lang-language (get-text 0 pos))
              (set! hash-lang-last-location pos)
              (clear-things-out)
              (when info-result
                (register-new-buttons
                 (contract (or/c #f (listof (list/c string?
                                                    (is-a?/c bitmap%)
                                                    (-> (is-a?/c drscheme:unit:frame<%>) any))))
                           (info-result 'drscheme:toolbar-buttons)
                           (get-lang-name pos)
                           'drscheme/private/module-language-tools)))))))

      (inherit get-tab)
      (define/private (register-new-buttons buttons)
        (when buttons
          (let* ([tab (get-tab)]
                 [frame (send tab get-frame)])
            (send tab set-lang-toolbar-buttons
                  (map (λ (button-spec)
                         (new switchable-button%
                              [label (list-ref button-spec 0)]
                              [bitmap (list-ref button-spec 1)]
                              [parent (send frame get-toolbar-button-panel)]
                              [callback
                               (lambda (button)
                                 ((list-ref button-spec 2) frame))]))
                       buttons)))))
      
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
        (send (get-tab) set-lang-toolbar-buttons '()))
      
      (define/augment (after-set-next-settings settings)
        (update-in-module-language?
         (is-a? (drscheme:language-configuration:language-settings-language settings)
                drscheme:module-language:module-language<%>))
        (inner (void) after-set-next-settings settings))
      (super-new)
      (set! in-module-language? 
            (is-a? (drscheme:language-configuration:language-settings-language (get-next-settings))
                   drscheme:module-language:module-language<%>)))))



#|

                                                                     
                                                                     
                                                                     
                                             
#lang scheme/gui

(require mrlib/switchable-button 
         mrlib/bitmap-label
         drscheme/tool
         scheme/system
         setup/xref)

(provide tool@)

(define-namespace-anchor anchor)

(define scribble-bm (make-object bitmap% 1 1))

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)

    (define phase1 void)
    (define phase2 void)

    (define (make-new-unit-frame% super%)
      (class super%
        (inherit get-button-panel
                 get-definitions-text)
        (super-instantiate ())

        (define client-panel
          (new horizontal-pane% (parent (get-button-panel))))

        (define (make-render-button label mode suffix extra-cmdline)
          (new switchable-button%
               [label label]
               [bitmap scribble-bm]
               [parent client-panel]
               [callback
                (lambda (button)
                  (let* ([t (get-definitions-text)]
                         [fn (send t get-filename)])
                    (if fn
                        (begin
                          (send t save-file)
                          (parameterize ([current-namespace (make-base-namespace)]
                                         [current-command-line-arguments
                                          (list->vector 
                                           (append
                                            extra-cmdline
                                            (list mode (if (path? fn) (path->string fn) fn))))])
                            (namespace-attach-module (namespace-anchor->empty-namespace anchor) 'setup/xref)
                            (dynamic-require 'scribble/run #f)
                            (let-values ([(base name dir?) (split-path fn)])
                              (system (format "open ~a" (path-replace-suffix name suffix))))))
                        (message-box "Not Named" "Cannot render unsaved file"))))]))

        (inherit register-toolbar-button)
        (define pdf-button (make-render-button "PDF" "--pdf" #".pdf" null))
        (register-toolbar-button pdf-button)
        (define html-button (make-render-button "HTML" "--html" #".html" '("++xref-in" "setup/xref" "load-collections-xref")))
        (register-toolbar-button html-button)

        (send (get-button-panel) change-children
              (lambda (l) (cons client-panel (remq client-panel l))))))

    (drscheme:get/extend:extend-unit-frame make-new-unit-frame% #f)))

|#