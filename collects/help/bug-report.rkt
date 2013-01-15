#lang racket/base
(require string-constants
         net/head
         racket/gui/base
         framework
         racket/class
         racket/port
         net/url
         net/uri-codec
         browser/htmltext
         "private/bug-report-controls.rkt"
         "private/buginfo.rkt"
         "private/save-bug-report.rkt")

(provide help-desk:report-bug
         (struct-out brinfo)
         saved-bug-report-titles/ids
         discard-all-saved-bug-reports)

(define (bug-server-url path)
  (string->url (string-append "http://bugs.racket-lang.org/" path)))
(define bug-report-url   (bug-server-url "bug-report.cgi"))
(define captcha-text-url (bug-server-url "captcha-text"))
(define (get-captcha-text)
  (let* ([s (port->string (get-pure-port captcha-text-url))]
         [s (regexp-replace #px"^\\s+" s "")]
         [s (regexp-replace #px"\\s+$" s "")])
    (and ((string-length s) . > . 0) s)))

(preferences:set-default 'drracket:email "" string? #:aliases '(drscheme:email))
(preferences:set-default 'drracket:full-name "" string? #:aliases '(drscheme:full-name))

(define open-frames '())

(define (discard-all-saved-bug-reports)
  (discard-all-except 
   (λ (id) (ormap (λ (frame) (equal? (send frame get-bug-id) id))
                  open-frames))))

(define (help-desk:report-bug [this-bug-id #f] #:frame-mixin [frame-mixin values])
  (cond
    [this-bug-id
     (let loop ([open-frames open-frames])
       (cond
         [(null? open-frames)
          (report-bug/new-frame this-bug-id frame-mixin)]
         [else
          (let ([open-frame (car open-frames)])
            (if (= (send open-frame get-bug-id) this-bug-id)
                (send open-frame show #t)
                (loop (cdr open-frames))))]))]
    [else
     (report-bug/new-frame this-bug-id frame-mixin)]))

(define (report-bug/new-frame this-bug-id frame-mixin)
  (define bug-frame%
    (class (frame-mixin (frame:standard-menus-mixin frame:basic%))
      (init title)
      (init-field bug-id)
      (define/public (get-bug-id) (and editing? bug-id))
      
      (define editing? #t)
      (define/public (no-longer-editing) (set! editing? #f))
      (define close-box-clicked? #t)
      (define/public (set-close-box-not-clicked) (set! close-box-clicked? #f))
      (define/augment (can-close?)
        (cond
          [close-box-clicked?
           (cond
             [(eq? (send single active-child) finished-panel)
              #t]
             [(empty-bug-report?)
              (no-more-saving)
              (unsave-bug-report bug-id)
              (set! editing? #f)]
             [else
              (define user-choice
                (message-box/custom (string-constant cancel-bug-report?)
                                    (string-constant do-you-want-to-discard-or-save-this-bug-report)
                                    (string-constant save)
                                    (string-constant cancel)
                                    (string-constant discard)
                                    this
                                    '(default=1)
                                    1))
              (case user-choice
                [(1) #t] ;; saving happens automatically
                [(2) #f]
                [(3) 
                 (no-more-saving)
                 (unsave-bug-report bug-id)
                 (set! editing? #f)
                 #t])])]
          [else #t]))
      (define/augment (on-close)
        (inner (void) on-close)
        (set! open-frames (remq this open-frames)))
      (super-make-object title)
      (set! open-frames (cons this open-frames))
      
      ;; a bunch of stuff we don't want
      (define/override (file-menu:between-print-and-close menu) (void))
      (define/override (edit-menu:between-find-and-preferences menu) (void))
      (define/override (file-menu:create-open?)        #f)
      (define/override (file-menu:create-open-recent?) #f)
      (define/override (file-menu:create-new?)         #f)
      (define/override (file-menu:create-save?)        #f)
      (define/override (file-menu:create-revert?)      #f)))

  (define init-bug-report (if this-bug-id
                              (lookup-bug-report this-bug-id)
                              (register-new-bug-id)))
  (define bug-frame (new bug-frame% 
                         [bug-id (saved-report-id init-bug-report)]
                         [title (string-constant bug-report-form)]))
  (define single (new panel:single% (parent (send bug-frame get-area-container))))
  (define compose-panel (make-object vertical-panel% single))
  
  (define cancel-kill-cust #f)
  
  (define-values (compose-view-focus get-query sanity-checking no-more-saving empty-bug-report?)
    (add-bug-report-controls compose-panel
                             init-bug-report
                             (λ () (ok))
                             (λ () (cancel))
                             (λ () (close-and-save))))
  
  (define pending-panel (new vertical-panel% (parent single)))
  (define pending-text (new html-text% (auto-wrap #t)))
  (define (reset-pending-text)
    (with-pending-text
     (λ ()
       (send pending-text erase)
       (render-html-to-text ; hack to get nice text in
        (open-input-string
         "&nbsp;<br><br><br><br><br><div align=\"center\"><h2><b>Submitting bug report...</b></h2></div>")
        pending-text #t #f))))
  (define (with-pending-text t)
    (send pending-text begin-edit-sequence)
    (send pending-text lock #f)
    (t)
    (send pending-text lock #t)
    (send pending-text end-edit-sequence))

  (define pending-ec (new editor-canvas% [parent pending-panel] [editor pending-text]))
  (send pending-ec allow-tab-exit #t)

  (define pending-button-panel (new horizontal-panel%
                                     [stretchable-height #f]
                                     [parent pending-panel]
                                     [alignment '(right center)]))
  (define pending-back (new button%
                            [parent pending-button-panel]
                            [callback (λ (x y) (switch-to-compose-view))]
                            [label (string-constant dialog-back)]))
  (define pending-abort (new button%
                             [parent pending-button-panel]
                             [callback (lambda (x y) (custodian-shutdown-all cancel-kill-cust))]
                             [label (string-constant abort)]))
  (new grow-box-spacer-pane% [parent pending-button-panel])

  (define finished-panel (new vertical-panel% [parent single]))
  (define finished-ec (new editor-canvas% (parent finished-panel)))
  (send finished-ec allow-tab-exit #t)
  (define finished-button-panel (new horizontal-panel% 
				     [stretchable-height #f]
				     [parent finished-panel]
				     [alignment '(right center)]))
  (define finished-close (new button%
			      [parent finished-button-panel]
			      [enabled #t]
			      [label (string-constant close)]
                              [callback
			       (lambda (x y)
                                 (send bug-frame set-close-box-not-clicked)
                                 (send bug-frame close))]))
  (new grow-box-spacer-pane% [parent finished-button-panel])
  
  (define (init-pending-view)
    (reset-pending-text)
    (send pending-back enable #f)
    (send pending-abort enable #t)
    (send single active-child pending-panel))

  (define (switch-to-compose-view)
    (send single active-child compose-panel)
    (compose-view-focus))
  
  ;; important that you cannot go back from this view, 
  ;; or else that might trigger saving the bug report in the preferences 
  ;; (but when you're here the bug report should be successfully submitted)
  (define (switch-to-finished-view finished-text)
    (send finished-ec set-editor finished-text)
    (unsave-bug-report (saved-report-id init-bug-report))
    (send single active-child finished-panel)
    (send finished-close focus))

  ;; send-bug-report : (-> void)
  ;; initiates sending the bug report and switches the GUI's mode
  (define (send-bug-report)
    (set! cancel-kill-cust (make-custodian))
    (define response-chan (make-channel))
    (define exn-chan (make-channel))
    (define starter-query (get-query))
    (define worker-thread
      (parameterize ([current-custodian cancel-kill-cust]
                     [current-alist-separator-mode 'amp])
        (thread
         (λ ()
           (with-handlers ([exn:fail? (λ (x) (channel-put exn-chan x))])
             ;; Note that this UI is not great: every submission asks for a
             ;; captcha and nothing is kept.  This is fine since this is only in
             ;; case it needs to be used in the future -- if/when that happens,
             ;; the code can be improved to remember some of it, and the server
             ;; can have some better policy to send the same captcha to the same
             ;; client.  So the only case where you'd suffer the bad UI is if a
             ;; captcha is added *and* you have this version of the code (which
             ;; will be outdated by that time).
             (define captcha-question (get-captcha-text))
             (define captcha-answer
               (and captcha-question
                    (get-text-from-user
                     "Are you human?" ; FIXME: use string-constant
                     captcha-question bug-frame)))
             (define post-data
               (let* ([q (if captcha-answer
                             `([captcha . ,captcha-answer]
                               ;; send back the question too: if things get really
                               ;; bad, then the server can make up random captchas
                               ;; and check the reply against the challenge that
                               ;; was used
                               [captcha-question . ,captcha-question]
                               ,@starter-query)
                             starter-query)])
                 (string->bytes/utf-8 (alist->form-urlencoded q))))

             (call/input-url
              bug-report-url
              (lambda (x) (post-impure-port x post-data))
              (lambda (port)
                (define error?
                  (cond [(regexp-match #rx"^HTTP/[0-9.]+ +([0-9]+) *(.*)$"
                                       (read-line port 'any))
                         => (lambda (m)
                              ;; ignore the status text -- the reply should
                              ;; have a better indication of what went wrong
                              ((string->number (cadr m)) . >= . 400))]
                        [else #f]))
                ;; skip HTTP headers
                (regexp-match-positions #rx"\r?\n\r?\n" port)
                (if error?
                  ;; error status => show as error
                  (begin (with-pending-text
                          (λ ()
                            (send pending-text erase)
                            (render-html-to-text port pending-text #t #f)))
                         (channel-put exn-chan #f)) ; #f = "already rendered"
                  ;; (hopefully) a good result
                  (let ([response-text (new html-text%)])
                    (render-html-to-text port response-text #t #f)
                    (send response-text auto-wrap #t)
                    (send response-text lock #t)
                    (channel-put response-chan response-text))))))))))
    (define (render-error to-render)
      (cond
        [(string? to-render)
         (let ([str (string-append "<pre>\n\nERROR:\n"to-render"\n</pre>\n")])
           (render-error (open-input-string str)))]
        [(exn? to-render) (render-error (exn-message to-render))]
        [(or (input-port? to-render) (not to-render))
         (queue-callback
          (λ ()
            (when to-render
              (with-pending-text
               (λ () (render-html-to-text to-render pending-text #t #f))))
            (send pending-back enable #t)
            (send pending-abort enable #f)))]
        [else (error 'render-error "internal error")]))

    (thread
     (λ ()
       (sync (handle-evt exn-chan render-error)
             (handle-evt (thread-dead-evt worker-thread)
                         (λ (_) (render-error "reporting process killed")))
             (handle-evt response-chan
                         (λ (finished-text)
                           (queue-callback
                            (λ ()
                              (switch-to-finished-view finished-text))))))))

    (init-pending-view))

  (define (ok)
    (when (sanity-checking)
      (send-bug-report)))
  
  (define (cancel)
    (when (or (empty-bug-report?)
              (ask-yes-or-no (string-constant cancel-bug-report?)
                             (string-constant are-you-sure-cancel-bug-report?)
                             bug-frame))
      (unsave-bug-report (saved-report-id init-bug-report))
      (send bug-frame set-close-box-not-clicked)
      (send bug-frame close)))
  
  (define (close-and-save)
    (send bug-frame set-close-box-not-clicked)
    (send bug-frame close))
  
  ;; Currently, the help-menu is left empty
  (frame:remove-empty-menus bug-frame)
  
  (switch-to-compose-view)
  
  (send bug-frame show #t))

(define html-text% (text:hide-caret/selection-mixin (html-text-mixin text:basic%)))

(define (ask-yes-or-no title msg parent)
  (gui-utils:get-choice msg 
                        (string-constant yes)
                        (string-constant no)
                        title
                        #f
                        parent))
