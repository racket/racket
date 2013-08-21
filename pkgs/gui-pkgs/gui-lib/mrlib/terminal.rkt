#lang racket/base
(require racket/gui/base
         racket/class
         racket/contract
         framework
         string-constants)

(provide 
 (contract-out
  [on-terminal-run (parameter/c (-> void?))]
  [in-terminal (->* ((-> eventspace? (or/c #f (is-a?/c top-level-window<%>)) void?))
                    (#:container (is-a?/c area-container<%>)
                     #:title string?
                     #:abort-label string?
                     #:aborted-message string?
                     #:cleanup-thunk (-> void?)
                     #:canvas-min-width (or/c #f exact-nonnegative-integer?)
                     #:canvas-min-height (or/c #f exact-nonnegative-integer?)
                     #:close-button? boolean?
                     #:close-label string?)
                    (is-a?/c terminal<%>))])
 terminal<%>)

(define terminal<%>
  (interface ()
    is-closed?
    close
    can-close?
    can-close-evt))

(define on-terminal-run (make-parameter void))

;; creates a frame and sets up the current error and output ports
;; before calling `do-install'. 
;; runs the installer in a separate thread and returns immediately,
;; before the installation is complete. The cleanup thunk is called when installation completes
(define (in-terminal do-install
                     #:container [container #f]
                     #:title [title "mrlib/terminal"]
                     #:abort-label [abort-label (string-constant plt-installer-abort-installation)]
                     #:aborted-message [aborted-message (string-constant plt-installer-aborted)]
                     #:cleanup-thunk [cleanup-thunk void]
                     #:canvas-min-width [canvas-min-width #f]
                     #:canvas-min-height [canvas-min-height #f]
                     #:close-button? [close-button? #t]
                     #:close-label [close-button-label (string-constant close)])
  (define orig-eventspace (current-eventspace))
  (define orig-custodian (current-custodian))
  (define inst-eventspace (if container
                              (send (send container get-top-level-window) get-eventspace)
                              (make-eventspace)))

  (define on-terminal-run-proc (on-terminal-run))
  
  (define frame #f)
  (define sub-container #f)
  (define text #f)
  (define close-button #f)
  (define kill-button #f)
  (define setup-sema (make-semaphore 0))
  (define can-close-sema (make-semaphore))
  (define currently-can-close? #t)
  
  (define (close)
    (if frame
        (send frame show #f)
        (send container delete-child sub-container))
    (close-callback))
  (define (close-callback)
    (custodian-shutdown-all installer-cust))
  
  (define saved-button-panel #f)

  (parameterize ([current-eventspace inst-eventspace])
    (queue-callback
     (λ ()
        (unless container
          (set! frame
                (new (class frame%
                       (define/augment (can-close?) currently-can-close?)
                       (define/augment (on-close) (close-callback))
                       (super-new [label title]
                                  [width 600]
                                  [height 300]))))
          (set! container frame)
          (define mb (new menu-bar% [parent frame]))
          (define edit-menu (new menu% 
                                 [label (string-constant edit-menu)]
                                 [parent mb]))
          (define copy-menu-item
            (new menu-item%
                 [parent edit-menu]
                 [label (string-constant copy-menu-item)]
                 [shortcut #\c]
                 [demand-callback
                  (λ (item)
                     (send copy-menu-item enable
                           (not (= (send text get-start-position)
                                   (send text get-end-position)))))]
                 [callback
                  (λ (item evt)
                     (send text copy))]))
          (new menu-item%
               [parent edit-menu]
               [label (string-constant select-all-menu-item)]
               [shortcut #\a]
               [callback
                (λ (item evt)
                   (send text set-position 0 (send text last-position)))]))
        
        (when container
          (send container begin-container-sequence))

        (set! sub-container
              (or frame
                  (new (class vertical-panel%
                         (super-new)
                         (define/override (on-superwindow-show on?)
                           (unless on? (close-callback))))
                       [parent container])))
        
        (set! text (new (text:hide-caret/selection-mixin text:standard-style-list%)))
        (define canvas (new editor-canvas%
                            [parent sub-container]
                            [editor text]
                            [min-width canvas-min-width]
                            [min-height canvas-min-height]))
        (define button-panel (new horizontal-panel%
                                  [parent sub-container]
                                  [stretchable-height #f]
                                  [alignment '(center center)]))
        (set! kill-button (new button%
                               [label abort-label]
                               [parent button-panel]
                               [callback (λ (b e) (kill-callback))]))
        (when close-button?
          (set! close-button (new button% 
                                  [label close-button-label]
                                  [parent button-panel]
                                  [callback (λ (b e) (close))])))

        (define (close)
          (if frame
              (send frame show #f)
              (send container delete-child sub-container))
          (close-callback))

        (define (kill-callback)
          (custodian-shutdown-all installer-cust)
          (fprintf output-port "\n~a\n" aborted-message))
        (set! currently-can-close? #f)
        (when close-button (send close-button enable #f))
        (send canvas allow-tab-exit #t)
        ((current-text-keymap-initializer) (send text get-keymap))
        (send text set-styles-sticky #f)
        (send text lock #t)
        (send text hide-caret #t)
        (set! saved-button-panel button-panel)
        (semaphore-post setup-sema)
        (when container
          (send container end-container-sequence))
        (when frame
          (send frame show #t)))))
  
  (if (equal? inst-eventspace (current-eventspace))
      (yield setup-sema)
      (semaphore-wait setup-sema))
  
  (define (mk-port style)
    (make-output-port
     #f
     always-evt
     (lambda (bytes start end flush? enable-break?)
       (define str (bytes->string/utf-8 (subbytes bytes start end)))
       (parameterize ([current-eventspace inst-eventspace])
         (queue-callback
          (lambda ()
            (define lp (send text last-position))
            (send text begin-edit-sequence)
            (send text lock #f)
            (send text insert
                  str
                  (send text last-position)
                  'same 
                  ; Scroll on newlines only:
                  (regexp-match? #rx"\n" str))
            (send text change-style style lp (send text last-position))
            (send text lock #t)
            (send text end-edit-sequence))))
       (- end start))
     void))
  
  (define plain-style (send (editor:get-standard-style-list) find-named-style "Standard"))
  (define red-delta (make-object style-delta% 'change-italic))
  (send red-delta set-delta-foreground "red")
  (define error-style (send (editor:get-standard-style-list) find-or-create-style 
                            plain-style
                            red-delta))
  
  (define output-port (mk-port plain-style))
  (define error-port (mk-port error-style))
  
  (define completed-successfully? #f)
  
  (define installer-cust (make-custodian))
    
  (parameterize ([current-custodian installer-cust])
    (parameterize ([current-eventspace (make-eventspace)])
      (queue-callback
       (lambda ()
         
         (let ([installer-thread (current-thread)])
           (parameterize ([current-custodian orig-custodian])
             (thread
              (lambda ()
                (thread-wait installer-thread)
                (parameterize ([current-eventspace inst-eventspace])
                  (queue-callback
                   (λ () 
                     (send kill-button enable #f)
                     (when close-button (send close-button enable #t))
                     (set! currently-can-close? #t)
                     (semaphore-post can-close-sema))))
                (unless completed-successfully? 
                  (parameterize ([current-eventspace orig-eventspace])
                    (queue-callback
                     (lambda ()
                       (cleanup-thunk)))))))))
         
         (let/ec k
           (parameterize ([current-output-port output-port]
                          [current-error-port error-port]
                          [error-escape-handler (λ () (k (void)))]
                          [exit-handler
                           (λ (x)
                             (unless (equal? x 0)
                               (eprintf "exited with code: ~s\n" x))
                             (custodian-shutdown-all installer-cust))])
             (do-install inst-eventspace frame)))
         (parameterize ([current-eventspace orig-eventspace])
           (queue-callback
            (lambda ()
              (set! completed-successfully? #t)
              (parameterize ([current-output-port output-port]
                             [current-error-port error-port])
                (on-terminal-run-proc))
              (cleanup-thunk)
              (custodian-shutdown-all installer-cust))))))))
  
  (new (class* object% (terminal<%>)
         (super-new)
         (define/public (get-button-panel)
           saved-button-panel)
         (define/public (can-close-evt)
           (semaphore-peek-evt can-close-sema))
         (define/public (is-closed?)
           (not (send sub-container is-shown?)))
         (define/public (can-close?) 
           currently-can-close?)
         (define/public (close)
           (unless (is-closed?)
             (if frame
                 (send frame show #f)
                 (send container delete-child sub-container)))))))
