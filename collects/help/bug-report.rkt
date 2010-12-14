#lang racket/base
(require string-constants
         net/head
         racket/gui/base
         framework
         racket/class
         net/url
         net/uri-codec
         browser/htmltext
         "private/bug-report-controls.rkt"
         "private/buginfo.ss"
         "private/save-bug-report.rkt")

(provide help-desk:report-bug
         (struct-out brinfo)
         saved-bug-report-titles/ids
         discard-all-saved-bug-reports)

(define bug-www-server "bugs.racket-lang.org")
(define bug-www-server-port 80)

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
  
  (define-values (compose-view-focus get-query sanity-checking)
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
  ;; (but when you're here the bug report should be succesfully submitted)
  (define (switch-to-finished-view finished-text)
    (send finished-ec set-editor finished-text)
    (unsave-bug-report (saved-report-id init-bug-report))
    (send single active-child finished-panel))
  
  ; send-bug-report : (-> void)
  ;; initiates sending the bug report and switches the GUI's mode
  (define (send-bug-report)
    (define query (get-query))
    (define url
      (string->url (format "http://~a:~a/cgi-bin/bug-report"
                           bug-www-server
                           bug-www-server-port)))
    (define post-data
      (parameterize ([current-alist-separator-mode 'amp])
        (string->bytes/utf-8 (alist->form-urlencoded query))))
    (set! cancel-kill-cust (make-custodian))
    (define response-chan (make-channel))
    (define exn-chan (make-channel))
    (define worker-thread
      (parameterize ([current-custodian cancel-kill-cust])
        (thread
         (λ ()
           (with-handlers ([exn:fail? (λ (x) (channel-put exn-chan x))])
             (parameterize ([current-alist-separator-mode 'amp])
               (call/input-url 
                url
                (case-lambda
                  [(x) (post-pure-port x post-data)]
                  [(x y) (post-pure-port x post-data y)])
                (lambda (port)
                  (define response-text (new html-text%))
                  (render-html-to-text port response-text #t #f)
                  (send response-text auto-wrap #t)
                  (send response-text lock #t)
                  (channel-put response-chan response-text)))))))))
    
    (thread
     (λ ()
       (sync
        (handle-evt
         exn-chan
         (λ (exn)
           (queue-callback
            (λ ()
              (define sp (open-output-string))
              (define-values (in out) (make-pipe))
              (thread
               (λ ()
                 (fprintf out "<pre>\n")
                 (display (exn-message exn) out)
                 (fprintf out "\n</pre>\n")
                 (close-output-port out)))
              (with-pending-text
               (λ () (render-html-to-text in pending-text #t #f)))
              (send pending-back enable #t)
              (send pending-abort enable #f)))))
        (handle-evt
         (thread-dead-evt worker-thread)
         (λ (_)
           (queue-callback
            (λ ()
              (with-pending-text
               (λ () 
                 (define p (send pending-text last-position))
                 (send pending-text insert "Killed." p p)))
              (send pending-back enable #t)
              (send pending-abort enable #f)))))
        (handle-evt
         response-chan
         (λ (finished-text)
           (queue-callback
            (lambda ()
              (switch-to-finished-view finished-text))))))))
    
    (init-pending-view))

  (define (ok)
    (when (sanity-checking)
      (send-bug-report)))
  
  (define (cancel)
    (when (ask-yes-or-no (string-constant cancel-bug-report?)
                         (string-constant are-you-sure-cancel-bug-report?)
                         bug-frame)
      (unsave-bug-report (saved-report-id init-bug-report))
      (send bug-frame close)))
  
  (define (close-and-save)
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
