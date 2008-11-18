#lang scheme/base

(require scheme/class
         scheme/file
         mred
         framework
         string-constants
         "test-info.scm"
         "test-engine.scm")

(define test-display%
  (class* object% ()

    (init-field (current-rep #f))

    (define test-info #f)
    (define/pubment (install-info t) 
      (set! test-info t)
      (inner (void) install-info t))

    (define current-tab #f)
    (define drscheme-frame #f)
    (define src-editor #f)
    (define/public (display-settings df ct ed)
      (set! current-tab ct)
      (set! drscheme-frame df)
      (set! src-editor ed))

    (define (docked?)
      (and drscheme-frame
           (get-preference 'test:test-window:docked? 
                           (lambda () (put-preferences '(test:test-window:docked?) '(#f)) #f))))
    
    (define/public (report-success)
      (when current-rep
        (unless current-tab
          (set! current-tab (send (send current-rep get-definitions-text) get-tab)))
        (unless drscheme-frame
          (set! drscheme-frame (send current-rep get-top-level-window)))
        (let ([curr-win (and current-tab (send current-tab get-test-window))]
              [content (make-object (editor:standard-style-list-mixin text%))])
          (send this insert-test-results content test-info src-editor)
          (send content lock #t)
          (when curr-win (send curr-win update-editor content))
          (when current-tab (send current-tab current-test-editor content))
          (when (and curr-win (docked?))
            (send drscheme-frame display-test-panel content)
            #;(send curr-win show #f))
          )))
    
    (define/public (display-results)
      (let* ([curr-win (and current-tab (send current-tab get-test-window))]
             [window (or curr-win (make-object test-window%))]
             [content (make-object (editor:standard-style-list-mixin text%))])
        
        (send this insert-test-results content test-info src-editor)
        (send content lock #t)
        (send window update-editor content)
        (when current-tab
          (send current-tab current-test-editor content)
          (unless curr-win
            (send current-tab current-test-window window)
            (send drscheme-frame register-test-window window)
            (send window update-switch
                  (lambda () (send drscheme-frame dock-tests)))
            (send window update-disable
                  (lambda () (send current-tab update-test-preference #f)))
            (send window update-closer
                  (lambda()
                    (send drscheme-frame deregister-test-window window)
                    (send current-tab current-test-window #f)
                    (send current-tab current-test-editor #f)))))
        (if (docked?)
            (send drscheme-frame display-test-panel content)
            (send window show #t))))

    (define/pubment (insert-test-results editor test-info src-editor)
      (let* ([style (send test-info test-style)]
             [total-tests (send test-info tests-run)]
             [failed-tests (send test-info tests-failed)]
             [total-checks (send test-info checks-run)]
             [failed-checks (send test-info checks-failed)]
             [test-outcomes
              (lambda (zero-message)
                (send editor insert
                      (cond
                        [(zero? total-tests) zero-message]
                        [(= 1 total-tests) "Ran 1 test.\n"]
                        [else (format "Ran ~a tests.\n" total-tests)]))
                (when (> total-tests 0)
                  (send editor insert
                        (cond
                          [(and (zero? failed-tests) (= 1 total-tests))
                           "Test passed!\n\n"]
                          [(zero? failed-tests) "All tests passed!\n\n"]
                          [(= failed-tests total-tests) "0 tests passed.\n"]
                          [else (format "~a of the ~a tests failed.\n\n" failed-tests total-tests)]))))]
             [check-outcomes
              (lambda (zero-message ck)
                (send editor insert
                      (cond
                        [(zero? total-checks) zero-message]
                        [(= 1 total-checks) (format "Ran 1 ~a.\n" ck)]
                        [else (format "Ran ~a ~as.\n" total-checks ck)]))
                (when (> total-checks 0)
                  (send editor insert
                        (cond
                          [(and (zero? failed-checks) (= 1 total-checks))
                           (format "The ~a passed!\n\n" ck)]
                          [(zero? failed-checks) (format "All ~as passed!\n\n" ck)]
                          [(= failed-checks total-checks) (format "0 ~as passed.\n" ck)]
                          [else (format "~a of the ~a ~as failed.\n\n"
                                        failed-checks total-checks ck)]))))])
        (case style
          [(test-require)
           (test-outcomes "This program must be tested!\n")
           (check-outcomes "This program is unchecked!\n" "check")]
          [(check-require)
           (check-outcomes "This program is unchecked!\n" "check")]
          [(test-basic)
           (test-outcomes "")
           (check-outcomes "" "check")]
          [(test-check)
           (check-outcomes "This program must be tested.\n" "test")]
          [else (check-outcomes "" "check")])

        (unless (and (zero? total-checks) (zero? total-tests))
          (inner (display-check-failures (send test-info failed-checks) 
                                         editor test-info src-editor)
                 insert-test-results editor test-info src-editor))))

    (define/public (display-check-failures checks editor test-info src-editor)
      (for ([failed-check (reverse checks)])
        (send editor insert "\t")
        (if (failed-check-exn? failed-check)
            (make-error-link editor
                             (failed-check-msg failed-check)
                             (failed-check-exn? failed-check)
                             (failed-check-src failed-check)
                             src-editor)
            (make-link editor
                       (failed-check-msg failed-check)
                       (failed-check-src failed-check)
                       src-editor))
        (send editor insert "\n")))

    ;next-line: editor% -> void
    ;Inserts a newline and a tab into editor
    (define/public (next-line editor) (send editor insert "\n\t"))

    ;; make-link: text% (listof (U string snip%)) src editor -> void
    (define (make-link text msg dest src-editor)
      (for ([m msg])
        (when (is-a? m snip%)
          (send m set-style (send (send text get-style-list)
                                  find-named-style "Standard")))
        (send text insert m))
      (let ((start (send text get-end-position)))
        (send text insert (format-src dest))
        (when (and src-editor current-rep)
          (send text set-clickback
                start (send text get-end-position)
                (lambda (t s e) (highlight-check-error dest src-editor))
                #f #f)
          (let ([end (send text get-end-position)]
                [c (new style-delta%)])
            (send text insert " ")
            (send text change-style
                  (make-object style-delta% 'change-underline #t)
                  start end #f)
            (send c set-delta-foreground "royalblue")
            (send text change-style c start end #f)))))
    
    ;; make-error-link: text% (listof (U string snip%)) exn src editor -> void
    (define (make-error-link text msg exn dest src-editor)
      (make-link text msg dest src-editor)
      (let ((start (send text get-end-position)))
        (send text insert "Trace error ")
        (when (and src-editor current-rep)
          (send text set-clickback
                start (send text get-end-position)
                (lambda (t s e) ((error-handler) exn))
                #f #f)
          (let ([end (send text get-end-position)]
                [c (new style-delta%)])
            (send text insert " ")
            (send text change-style
                  (make-object style-delta% 'change-underline #t)
                  start end #f)
            (send c set-delta-foreground "red")
            (send text change-style c start end #f)))))

    ;format-src: src -> string
    (define (format-src src)
      (let ([src-file car]
            [src-line cadr]
            [src-col caddr])
        (string-append
         (cond
           [(symbol? (src-file src)) (string-append " At ")]
           [(path? (src-file src)) (string-append " In " (path->string (src-file src)) " at ")]
           [(is-a? (src-file src) editor<%>) " At "])
         "line " (cond [(src-line src) => number->string]
                       [else "(unknown)"])
         " column " (cond [(src-col src) => number->string]
                          [else "(unknown)"]))))

    (define (highlight-check-error srcloc src-editor)
      (let* ([src-pos cadddr]
             [src-span (lambda (l) (car (cddddr l)))]
             [position (src-pos srcloc)]
             [span (src-span srcloc)])
        (when (and current-rep src-editor)
          (cond
            [(is-a? src-editor text:basic<%>)
             (let ((highlight
                    (lambda ()
                      (send current-rep highlight-errors
                            (list (make-srcloc src-editor
                                               (cadr srcloc)
                                               (caddr srcloc)
                                               position span)) #f))))
               (queue-callback highlight))]))))

    (super-instantiate ())))

(define test-window%
  (class* frame% ()

    (super-instantiate
     ((string-constant test-engine-window-title) #f 400 350))

    (define switch-func void)
    (define disable-func void)
    (define close-cleanup void)

    (define content
      (make-object editor-canvas% this #f '(auto-vscroll)))

    (define button-panel
      (make-object horizontal-panel% this
                   '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))

    (define buttons
      (list (make-object button%
                         (string-constant close)
                         button-panel
                         (lambda (b c)
                           (when (eq? 'button (send c get-event-type))
                             (close-cleanup)
                             (send this show #f))))
            (make-object button%
                         (string-constant dock)
                         button-panel
                         (lambda (b c)
                           (when (eq? 'button (send c get-event-type))
                             (send this show #f)
                             (put-preferences '(test:test-window:docked?)
                                              '(#t))
                             (switch-func))))
            (make-object grow-box-spacer-pane% button-panel)))

    (define/public (update-editor e)
      (send content set-editor e))

    (define/public (update-switch thunk)
      (set! switch-func thunk))
    (define/public (update-closer thunk)
      (set! close-cleanup thunk))
    (define/public (update-disable thunk)
      (set! disable-func thunk))))

(define test-panel%
  (class* vertical-panel% ()

    (inherit get-parent)

    (super-instantiate ())

    (define content (make-object editor-canvas% this #f '()))
    (define button-panel (make-object horizontal-panel% this
                                      '() #t 0 0 0 0 '(right bottom) 0 0 #t #f))
    (define (hide)
      (let ([current-tab (send frame get-current-tab)])
        (send frame deregister-test-window 
              (send current-tab get-test-window))
        (send current-tab current-test-window #f)
        (send current-tab current-test-editor #f))
      (remove))

    (make-object button%
                 (string-constant hide)
                 button-panel
                 (lambda (b c)
                   (when (eq? 'button (send c get-event-type))
                     (hide))))
    #;(make-object button%
                 (string-constant profj-test-results-hide-and-disable)
                 button-panel
                 (lambda (b c)
                   (when (eq? 'button (send c get-event-type))
                     (hide)
                     (send (send frame get-current-tab)
                           update-test-preference #f))))
    (make-object button%
                 (string-constant undock)
                 button-panel
                 (lambda (b c)
                   (when (eq? 'button (send c get-event-type))
                     (put-preferences '(test:test-window:docked?) '(#f))
                     (send frame undock-tests))))

    (define/public (update-editor e)
      (send content set-editor e))

    (define frame #f)
    (define/public (update-frame f)
      (set! frame f))

    (define/public (remove)
      (let ([parent (get-parent)])
        (put-preferences '(test:test-dock-size)
                         (list (send parent get-percentages)))
        (send parent delete-child this)))))

(provide test-panel% test-window% test-display%)
