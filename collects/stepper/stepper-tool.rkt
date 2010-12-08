#lang racket/unit

(require scheme/class
         drscheme/tool
         mred
         mzlib/pconvert
         string-constants
         (prefix-in frame: framework) 
         mrlib/switchable-button
         (file "private/my-macros.ss")
         (prefix-in x: "private/mred-extensions.ss")
         "private/shared.ss"
         lang/stepper-language-interface
         scheme/pretty
         "xml-sig.ss"
         "drracket-button.ss") ;; get the stepper-button-callback private-member-name

(import drscheme:tool^ xml^ view-controller^)
(export drscheme:tool-exports^ stepper-frame^)

  ;; tool magic here:
(define (phase1)
  ;; experiment with extending the language... parameter-like fields for stepper parameters
  (drscheme:language:extend-language-interface
   stepper-language<%>
   (lambda (superclass)
     (class* superclass (stepper-language<%>)
       (public stepper:supported?)
       (define (stepper:supported?) #f)
       
       (public stepper:enable-let-lifting?)
       (define (stepper:enable-let-lifting?) #f)
       
       (public stepper:show-lambdas-as-lambdas?)
       (define (stepper:show-lambdas-as-lambdas?) #t)
       
       (public stepper:show-inexactness?)
       (define (stepper:show-inexactness?) #t)

       (public stepper:show-consumed-and/or-clauses?)
       (define (stepper:show-consumed-and/or-clauses?) #t)
       
       (public stepper:render-to-sexp)
       (define (stepper:render-to-sexp val settings language-level)
         (parameterize ([pretty-print-show-inexactness (stepper:show-inexactness?)]
                        [current-print-convert-hook stepper-print-convert-hook])
           (set-print-settings
            language-level
            settings
            (lambda ()
              (simple-module-based-language-convert-value
               val
               settings)))))
       
       (super-instantiate ())))))

(define (phase2) (void))

;; this should be a preference:
(define stepper-initial-width 500)
(define stepper-initial-height 500)

(define (extract-language-level definitions-text)
  (settings->language-level (definitions-text->settings definitions-text)))

(define (definitions-text->settings definitions-text)
  (send definitions-text get-next-settings))

(define (settings->language-level settings)
  (drscheme:language-configuration:language-settings-language settings))

(define (stepper-works-for? language-level)
  (or (send language-level stepper:supported?)
      (getenv "PLTSTEPPERUNSAFE")))
  
  ;; the stepper's frame:
  
(define stepper-frame%
  (class (drscheme:frame:basics-mixin
          (frame:frame:standard-menus-mixin frame:frame:basic%))
    
    (init-field drscheme-frame)
    
    ;; PRINTING-PROC
    ;; I frankly don't think that printing (i.e., to a printer) works
    ;; correctly. 2005-07-01, JBC
    (public set-printing-proc)
    
    (define (set-printing-proc proc)
      (set! printing-proc proc))
    
    (define (printing-proc item evt)
      (message-box "error?" "shouldn't be called"))
    
    (define/private (file-menu:print a b) (printing-proc a b))
    
    ;; MENUS
    
    (define/override (edit-menu:between-find-and-preferences edit-menu)
      (void))
    (define/override (edit-menu:between-select-all-and-find edit-menu)
      (void))
    (define/override (file-menu:between-save-as-and-print file-menu)
      (void))
    
    ;; CUSTODIANS
    ;; The custodian is used to halt the stepped computation when the
    ;; stepper window closes.  The custodian is captured when the stepped
    ;; computation starts.
    
    (define custodian #f)
    (define/public (set-custodian! cust)
      (set! custodian cust))
    (define/augment (on-close)
      (when custodian
        (custodian-shutdown-all custodian))
      (send drscheme-frame on-stepper-close)
      (inner (void) on-close))
    
    ;; WARNING BOXES:
    
    (define program-changed-warning-str
      (string-constant stepper-program-has-changed))
    (define window-closed-warning-str
      (string-constant stepper-program-window-closed))
    
    (define warning-message-visible-already #f)
    (define/private (add-warning-message warning-str)
      (let ([warning-msg (new x:stepper-warning%
                              [warning-str warning-str]
                              [parent (get-area-container)])])
        (send (get-area-container)
              change-children
              (if warning-message-visible-already
                  (lambda (l)
                    (list (car l) warning-msg (caddr l)))
                  (lambda (l)
                    (list (car l) warning-msg (cadr l)))))
        (set! warning-message-visible-already #t)))
    
    (inherit get-area-container)
    (define program-change-already-warned? #f)
    (define/public (original-program-changed)
      (unless program-change-already-warned?
        (set! program-change-already-warned? #t)
        (add-warning-message program-changed-warning-str)))
    
    (define/public (original-program-gone)
      (add-warning-message window-closed-warning-str))
    
    (super-new [label "Stepper"] [parent #f]
               [width stepper-initial-width]
               [height stepper-initial-height])))
  

  ;; stepper-unit-frame<%> : the interface that the extended drscheme frame
  ;; fulfils
  (define stepper-unit-frame<%>
    (interface ()
      get-stepper-frame
      on-stepper-close))
  
  ;; stepper-unit-frame-mixin : the mixin that is applied to the drscheme
  ;; frame to interact with a possible stepper window
  (define (stepper-unit-frame-mixin super%)
    (class* super% (stepper-unit-frame<%>)
      
      (inherit get-button-panel register-toolbar-button get-interactions-text get-definitions-text)
      
      (define stepper-frame #f)
      (define/public (on-stepper-close)
        (set! stepper-frame #f))
      (define/public (get-stepper-frame) stepper-frame)
      
      (super-new)
      
      ;; program-expander : produces expanded expressions from the
      ;; definitions window one at a time and calls 'iter' on each one
      (define (program-expander init iter)
        (let* ([lang-settings
                (send (get-definitions-text) get-next-settings)]
               [lang (drscheme:language-configuration:language-settings-language lang-settings)]
               [settings (drscheme:language-configuration:language-settings-settings lang-settings)])
          (drscheme:eval:expand-program
           (drscheme:language:make-text/pos
            (get-definitions-text)
            0
            (send (get-definitions-text) last-position))
           lang-settings
           #f
           (lambda ()
             (init)
             (error-value->string-handler
              (lambda (val len)
                (let ([sp (open-output-string)])
                  (send lang render-value val settings sp)
                  (let ([str (get-output-string sp)])
                    (if ((string-length str) . <= . len)
                        str
                        (string-append (substring str 0 (max 0 (- len 3)))
                                       "..."))))))
             (current-print void))
           void ; kill
           iter)))
      
      ;; STEPPER BUTTON
      
      (define/public (get-stepper-button) stepper-button)
      
      (define stepper-button-parent-panel 
        (new horizontal-panel%
             [parent (get-button-panel)]
             [stretchable-width #f]
             [stretchable-height #f]))
      
      ;; called from drracket-button.rkt, installed via the #lang htdp/bsl (& co) reader into drscheme
      (define/public (stepper-button-callback)
        (if stepper-frame
            (send stepper-frame show #t)
            (let* ([language-level
                    (extract-language-level (get-definitions-text))]
                   [language-level-name (language-level->name language-level)])
              (if (or (stepper-works-for? language-level)
                      (is-a? language-level drscheme:module-language:module-language<%>))
                  (set! stepper-frame
                        (go this 
                            program-expander 
                            (+ 1 (send (get-definitions-text) get-start-position))
                            (+ 1 (send (get-definitions-text) get-end-position))))
                  (message-box
                   (string-constant stepper-name)
                   (format (string-constant stepper-language-level-message)
                           language-level-name))))))
      
      (define stepper-button
        (new switchable-button% 
             [parent stepper-button-parent-panel]
             [label (string-constant stepper-button-label)]
             [bitmap x:foot-img/horizontal]
             [alternate-bitmap x:foot-img/vertical]
             [callback (lambda (dont-care) (stepper-button-callback))]))
      
      (register-toolbar-button stepper-button)
      
      (define/augment (enable-evaluation)
        (send stepper-button enable #t)
        (inner (void) enable-evaluation))
      
      (define/augment (disable-evaluation)
        (send stepper-button enable #f)
        (inner (void) disable-evaluation))
      
      (define/augment (on-close)
        (when stepper-frame
          (send stepper-frame original-program-gone))
        (inner (void) on-close))
      
      (define/augment (on-tab-change old new)
        (check-current-language-for-stepper)
        (inner (void) on-tab-change old new))
      
      (define/public (check-current-language-for-stepper)
        (if (stepper-works-for?
             (extract-language-level (get-definitions-text)))
            (unless (send stepper-button is-shown?)
              (send (send stepper-button get-parent)
                    add-child stepper-button))
            (when (send stepper-button is-shown?)
              (send (send stepper-button get-parent)
                    delete-child stepper-button))))
      
      ;; add the stepper button to the button panel:
      (send (get-button-panel) change-children 
            (lx (cons stepper-button-parent-panel
                      (remq stepper-button-parent-panel _))))
      
      ;; hide stepper button if it's not supported for the initial language:
      (check-current-language-for-stepper)))
  
  ;; stepper-definitions-text-mixin : a mixin for the definitions text that
  ;; alerts thet stepper when the definitions text is altered or destroyed
  (define (stepper-definitions-text-mixin %)
    (class %
      
      (inherit get-top-level-window)
      (define/private (notify-stepper-frame-of-change)
        (let ([win (get-top-level-window)])
          ;; should only be #f when win is #f
          (when (is-a? win stepper-unit-frame<%>)
            (let ([stepper-window (send win get-stepper-frame)])
              (when stepper-window
                (send stepper-window original-program-changed))))))
      
      (define/augment (on-insert x y)
        (unless metadata-changing-now?
          (notify-stepper-frame-of-change))
        (inner (void) on-insert x y))
      
      (define/augment (on-delete x y)
        (unless metadata-changing-now?
          (notify-stepper-frame-of-change))
        (inner (void) on-delete x y))
      
      (define/augment (after-set-next-settings s)
        (let ([tlw (get-top-level-window)])
          (when tlw
            (send tlw check-current-language-for-stepper)))
        (inner (void) after-set-next-settings s))
      
      (define metadata-changing-now? #f)
      
      ;; don't pay attention to changes that occur on metadata.
      ;; this assumes that metadata changes cannot be nested.
      (define/augment (begin-metadata-changes)
        (set! metadata-changing-now? #t)
        (inner (void) begin-metadata-changes))
      
      (define/augment (end-metadata-changes)
        (set! metadata-changing-now? #f)
        (inner (void) end-metadata-changes))
      
      (super-new)))
  
  ;; apply the mixins dynamically to the drscheme unit frame and
  ;; definitions text:
  (drscheme:get/extend:extend-unit-frame stepper-unit-frame-mixin)
  (drscheme:get/extend:extend-definitions-text stepper-definitions-text-mixin)
  
  ;; COPIED FROM drscheme/private/language.ss
;; simple-module-based-language-convert-value : TST STYLE boolean -> TST
(define (simple-module-based-language-convert-value value settings)
  (case (drscheme:language:simple-settings-printing-style settings)
    [(write) value]
    [(constructor)
     (parameterize
         ([constructor-style-printing #t]
          [show-sharing (drscheme:language:simple-settings-show-sharing settings)]
          [current-print-convert-hook
           (leave-snips-alone-hook (current-print-convert-hook))])
       (stepper-print-convert value))]
    [(quasiquote)
     (parameterize
         ([constructor-style-printing #f]
          [show-sharing (drscheme:language:simple-settings-show-sharing settings)]
          [current-print-convert-hook
           (leave-snips-alone-hook (current-print-convert-hook))])
       (stepper-print-convert value))]
    [else (error "Internal stepper error: time to resync with simple-module-based-language-convert-value")]))

(define ((leave-snips-alone-hook sh) expr basic-convert sub-convert)
  (if (or (is-a? expr snip%)
          ;; FIXME: internal in language.ss (to-snip-value? expr)
          )
      expr
      (sh expr basic-convert sub-convert)))
  
;; mflatt: MINOR HACK - work around temporary
;;         print-convert problems
(define (stepper-print-convert v)
  (or (and (procedure? v) (object-name v))
      (print-convert v)))
  

;; set-print-settings ; settings ( -> TST) -> TST
(define (set-print-settings language simple-settings thunk)
  (if (method-in-interface? 'set-printing-parameters (object-interface language))
      (send language set-printing-parameters simple-settings thunk)
      ;; assume that the current print-convert context is fine
      ;; (error 'stepper-tool "language object does not contain set-printing-parameters method")
      ;; 2009-09-11, JBC : Gee Whiz, why the heck is it okay to assume that !?
      (thunk)))

;; WE REALLY WANT TO GET RID OF THIS STUFF (2005-07-01, JBC)

;; stepper-convert-hook:
;;   (TST (TST -> TST) (TST -> TST) -> TST)
;; this code copied from various locations in language.ss and rep.ss
(define (stepper-print-convert-hook exp basic-convert sub-convert)
  (cond
    [(is-a? exp snip%)
     (send exp copy)]
    #;
    [((drscheme:rep:use-number-snip) exp)
     (let ([number-snip-type
            (drscheme:language:simple-settings-fraction-style
             simple-settings)])
       (cond
         [(eq? number-snip-type 'repeating-decimal)
          (drscheme:number-snip:make-repeating-decimal-snip exp #f)]
         [(eq? number-snip-type 'repeating-decimal-e)
          (drscheme:number-snip:make-repeating-decimal-snip exp #t)]
         [(eq? number-snip-type 'mixed-fraction)
          (drscheme:number-snip:make-fraction-snip exp #f)]
         [(eq? number-snip-type 'mixed-fraction-e)
          (drscheme:number-snip:make-fraction-snip exp #t)]
         [else
          (error 'which-number-snip
                 "expected either 'repeating-decimal, 'repeating-decimal-e, 'mixed-fraction, or 'mixed-fraction-e got : ~e"
                 number-snip-type)]))]
    [else (basic-convert exp)]))

