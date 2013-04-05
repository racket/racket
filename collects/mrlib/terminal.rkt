#lang racket/base
(require racket/gui/base
         racket/class
         racket/contract
         framework
         string-constants)

(provide 
 (contract-out
  [on-terminal-run (parameter/c (-> void?))]
  [in-terminal (->* ((-> eventspace? (is-a?/c top-level-window<%>) void?))
                    (#:title string?
                             #:abort-label string?
                             #:aborted-message string?
                             #:cleanup-thunk (-> void?))
                    void?)]))

(define on-terminal-run (make-parameter void))

;; creates a frame and sets up the current error and output ports
;; before calling `do-install'. 
;; runs the installer in a separate thread and returns immediately,
;; before the installation is complete. The cleanup thunk is called when installation completes
(define (in-terminal do-install
                     #:title [title "mrlib/terminal"]
                     #:abort-label [abort-label (string-constant plt-installer-abort-installation)]
                     #:aborted-message [aborted-message (string-constant plt-installer-aborted)]
                     #:cleanup-thunk [cleanup-thunk void])
  (define orig-eventspace (current-eventspace))
  (define orig-custodian (current-custodian))
  (define inst-eventspace (make-eventspace))

  (define on-terminal-run-proc (on-terminal-run))
  
  (define frame #f)
  (define text #f)
  (define close-button #f)
  (define kill-button #f)
  (define setup-sema (make-semaphore 0))
  
  (parameterize ([current-eventspace inst-eventspace])
    (queue-callback
     (λ ()
       (set! frame (new (class frame%
                          (define/augment (can-close?) (send close-button is-enabled?))
                          (define/augment (on-close) (close-callback))
                          (super-new [label title]
                                     [width 600]
                                     [height 300]))))
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
       (define select-all-item 
         (new menu-item%
              [parent edit-menu]
              [label (string-constant select-all-menu-item)]
              [shortcut #\a]
              [callback
               (λ (item evt)
                 (send text set-position 0 (send text last-position)))]))
                                    
       (set! text (new (text:hide-caret/selection-mixin text:standard-style-list%)))
       (define canvas (new editor-canvas%
                           [parent frame]
                           [editor text]))
       (define button-panel (new horizontal-panel%
                                 [parent frame]
                                 [stretchable-height #f]
                                 [alignment '(center center)]))
       (set! kill-button (new button%
                              [label abort-label]
                              [parent button-panel]
                              [callback (λ (b e) (kill-callback))]))
       (set! close-button (new button% 
                              [label (string-constant close)]
                              [parent button-panel]
                              [callback (λ (b e) (close-callback))]))
       (define (kill-callback)
         (custodian-shutdown-all installer-cust)
         (fprintf output-port "\n~a\n" aborted-message))
       (define (close-callback)
         (send frame show #f)
         (custodian-shutdown-all installer-cust))
       (send close-button enable #f)
       (send canvas allow-tab-exit #t)
       ((current-text-keymap-initializer) (send text get-keymap))
       (send text set-styles-sticky #f)
       (send text lock #t)
       (send text hide-caret #t)
       (semaphore-post setup-sema)
       (send frame show #t))))
  
  (semaphore-wait setup-sema)
  
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
                     (send close-button enable #t))))
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
              (custodian-shutdown-all installer-cust)))))))))
