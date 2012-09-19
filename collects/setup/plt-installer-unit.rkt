(module plt-installer-unit mzscheme
  (require mzlib/unit
           mred/mred-sig
           mzlib/class
           mzlib/etc
           "plt-installer-sig.rkt"
           (prefix single: "plt-single-installer.rkt")
           string-constants)

  (provide plt-installer@)
  (define-unit plt-installer@
    (import mred^)
    (export setup:plt-installer^)
      
      (define on-installer-run
        (make-parameter void))
      
      ;; with-installer-window : ((union (instanceof dialog%) (instanceof frame%)) -> void) (-> void) -> void
      ;; creates a frame and sets up the current error and output ports
      ;; before calling `do-install'. 
      ;; runs the installer in a separate thread and returns immediately,
      ;; before the installation is complete. The cleanup thunk is called when installation completes
      (define (with-installer-window do-install cleanup-thunk)
        (let ([orig-eventspace (current-eventspace)]
              [orig-custodian (current-custodian)]
              [inst-eventspace (make-eventspace)])
          (parameterize ([current-eventspace inst-eventspace])
            (letrec ([dlg (make-object (class dialog% ()
                                         (define/augment can-close? (lambda () (send done is-enabled?)))
                                         (define/augment on-close (lambda () (done-callback)))
                                         (super-make-object
                                          (string-constant plt-installer-progress-window-title)
                                          #f 600 300 #f #f '(resize-border))))]
                     [text (make-object text%)]
                     [canvas (make-object editor-canvas% dlg text)]
                     [button-panel (instantiate horizontal-panel% ()
                                     (parent dlg)
                                     (stretchable-height #f)
                                     (alignment '(center center)))]
                     [kill-button (make-object button%
                                    (string-constant plt-installer-abort-installation)
                                    button-panel
                                    (lambda (b e) (kill)))]
                     [done (make-object button% (string-constant close) button-panel (lambda (b e) (done-callback)))]
                     [output (make-output-port
                              #f
                              always-evt
                              (lambda (bytes start end flush? enable-break?)
                                (parameterize ([current-eventspace inst-eventspace])
                                  (queue-callback
                                   (lambda ()
                                     (let ([str (bytes->string/utf-8 (subbytes bytes start end))])
                                       (send text lock #f)
                                       (send text insert
                                             str
                                             (send text last-position)
                                             'same 
                                             ; Scroll on newlines only:
                                             (regexp-match #rx"\n" str))
                                       (send text lock #t)))))
                                (- end start))
                              void)]
                     [kill
                      (lambda ()
                        (custodian-shutdown-all installer-cust)
                        (fprintf output "\n~a\n" (string-constant plt-installer-aborted))
                        (send done enable #t))]
                     [completed-successfully? #f]
                     [done-callback
                      (lambda ()
                        (send dlg show #f)
                        (custodian-shutdown-all installer-cust))]
                     [installer-cust (make-custodian)])
              (send done enable #f)
              (send canvas allow-tab-exit #t)
              ((current-text-keymap-initializer) (send text get-keymap))
              (send text lock #t)

              ;; still do this even tho we aren't in the eventspace main thread
              (thread (lambda () (send dlg show #t)))

              (parameterize ([current-custodian installer-cust])
                (parameterize ([current-eventspace (make-eventspace)])
                  (queue-callback
                   (lambda ()
                     
                     (let ([installer-thread (current-thread)])
                       (parameterize ([current-custodian orig-custodian])
                         (thread
                          (lambda ()
                            (thread-wait installer-thread)
                            (send kill-button enable #f)
                            (unless completed-successfully? 
                              (parameterize ([current-eventspace orig-eventspace])
                                (queue-callback
                                 (lambda ()
                                   (cleanup-thunk)))))))))
                    
                     (parameterize ([current-output-port output]
                                    [current-error-port output])
                       (do-install dlg))
                     (parameterize ([current-eventspace orig-eventspace])
                       (queue-callback
                        (lambda ()
                          (fprintf output "\nInstallation complete.\n")
                          (set! completed-successfully? #t)
                          ((on-installer-run))
                          (cleanup-thunk)
                          (custodian-shutdown-all installer-cust))))
                     
                     (send done enable #t)))))))))
      
      (define run-single-installer single:run-single-installer)
      
      (define run-installer 
        (opt-lambda (file [cleanup-thunk void])
          (with-installer-window 
           (lambda (frame)
             (run-single-installer 
              file
              (lambda ()
                (sleep 0.2) ; kludge to allow f to appear first
                (end-busy-cursor)
                ;; do these strings ever appear? (should move to string-constants, if so)
                (let ([d (get-directory 
                          "Select the destination for unpacking"
                          frame)])
                  (unless d
                    (printf ">>> Cancelled <<<\n"))
                  (begin-busy-cursor)
                  d))))
           cleanup-thunk)))))
