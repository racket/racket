#lang racket/base
(provide module-language-tools@)
(require mrlib/switchable-button 
         mrlib/bitmap-label
         racket/contract
         framework
         scheme/unit
         racket/class
         racket/gui/base
         "drsig.ss")

(define op (current-output-port))
(define (oprintf . args) (apply fprintf op args))

(define-unit module-language-tools@
  (import [prefix drscheme:unit: drscheme:unit^]
          [prefix drscheme:module-language: drscheme:module-language^]
          [prefix drscheme:language: drscheme:language^]
          [prefix drscheme:language-configuration: drscheme:language-configuration^])
  (export drscheme:module-language-tools^)

  (define-local-member-name initialized? move-to-new-language)

  (define-struct opt-out-toolbar-button (make-button id) #:transparent)
  (define opt-out-toolbar-buttons '())
  
  (define (add-opt-out-toolbar-button make-button id) 
    (set! opt-out-toolbar-buttons
          (cons (make-opt-out-toolbar-button make-button id)
                opt-out-toolbar-buttons)))
    
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
               ;; info-result : (or/c #f   [#lang without a known language]
               ;;                     (vector <get-info-proc>) [no #lang line, so we use the '#lang racket' info proc]
               ;;                     <get-info-proc>  [the get-info proc for the program in the definitions]
               [info-result (with-handlers ((exn:fail? (λ (x) #f)))
                              (read-language 
                               port
                               (lambda () 
                                 ;; fall back to whatever #lang racket does if
                                 ;; we don't have a #lang line present in the file
                                 (vector (read-language (open-input-string "#lang racket"))))))])
          
          ; sometimes I get eof here, but I don't know why and can't seem to 
          ;; make it happen outside of DrScheme
          (when (eof-object? info-result)
            (fprintf (current-error-port) "file ~s produces eof from read-language\n"
                     (send this get-filename))
            (fprintf (current-error-port) "  port-next-location ~s\n" (call-with-values (λ () (port-next-location port)) list))
            (fprintf (current-error-port) "  str ~s\n" (let ([s (send this get-text)])
                                                         (substring s 0 (min 100 (string-length s)))))
            (set! info-result #f))
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
                           ((if (vector? info-result)
                                (vector-ref info-result 0)
                                info-result)
                            'drscheme:toolbar-buttons #f)
                           (if (vector? info-result)
                               'hash-lang-racket
                               (get-lang-name pos))
                           'drscheme/private/module-language-tools)
                 ((if (vector? info-result)
                      (vector-ref info-result 0)
                      info-result)
                  'drscheme:opt-out-toolbar-buttons '())))))))

      (inherit get-tab)
      
      (define/private (register-new-buttons buttons opt-out-ids)
        (let* ([tab (get-tab)]
               [frame (send tab get-frame)])
          (when (send frame initialized?)
            (send frame begin-container-sequence)
            (let ([directly-specified-buttons
                   (map (λ (button-spec)
                          (new switchable-button%
                               [label (list-ref button-spec 0)]
                               [bitmap (list-ref button-spec 1)]
                               [parent (send frame get-toolbar-button-panel)]
                               [callback
                                (lambda (button)
                                  ((list-ref button-spec 2) frame))]))
                        (or buttons '()))]
                  [opt-out-buttons
                   (if (eq? opt-out-ids #f)
                       '()
                       (map
                        (λ (opt-out-toolbar-button)
                          ((opt-out-toolbar-button-make-button opt-out-toolbar-button) 
                           frame
                           (send frame get-toolbar-button-panel)))
                        (filter (λ (opt-out-toolbar-button)
                                  (not (member (opt-out-toolbar-button-id opt-out-toolbar-button) 
                                               opt-out-ids)))
                                opt-out-toolbar-buttons)))])
              (send tab set-lang-toolbar-buttons
                    (sort
                     (append directly-specified-buttons
                             opt-out-buttons)
                     string<=?
                     #:key (λ (x) (send x get-label)))))
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
