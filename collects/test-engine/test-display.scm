#lang scheme/base

(require scheme/class
         scheme/file
         mred
         framework
         string-constants
         "test-info.scm"
         "test-engine.scm"
	 "print.ss")

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
            #;(send curr-win show #f)))))

    (define/public (display-success-summary port count)
      (unless (test-silence)
	(display (case count
		   [(0) (string-constant test-engine-0-tests-passed)]
		   [(1) (string-constant test-engine-1-test-passed)]
		   [(2) (string-constant test-engine-both-tests-passed)]
		   [else (format (string-constant test-engine-all-n-tests-passed)
				 count)])
		 port)))

    (define/public (display-untested-summary port)
      (unless (test-silence)
        (display (string-constant test-engine-should-be-tested) port)
	(display "\n" port)))

    (define/public (display-disabled-summary port)
      (display (string-constant test-engine-tests-disabled) port)
      (display "\n" port))
    
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
             [outcomes
              (lambda (total failed zero-message ck?)
                (send editor insert
                      (cond
                        [(zero? total) zero-message]
                        [(= 1 total)
			 (string-append 
			  (if ck?
			      (string-constant test-engine-ran-1-check)
			      (string-constant test-engine-ran-1-test))
			  "\n")]
                        [else 
			 (format (string-append
				  (if ck?
				      (string-constant test-engine-ran-n-checks)
				      (string-constant test-engine-ran-n-tests))
				  "\n")
				 total)]))
                (when (> total 0)
                  (send editor insert
                        (cond
                          [(and (zero? failed) (= 1 total))
			   (string-append (if ck?
					      (string-constant test-engine-1-check-passed)
					      (string-constant test-engine-1-test-passed))
					  "\n\n")]
                          [(zero? failed)
			   (string-append (if ck?
					      (string-constant test-engine-all-checks-passed)
					      (string-constant test-engine-all-tests-passed))
					  "\n\n")]
                          [(= failed total)
			   (string-append (if ck?
					      (string-constant test-engine-0-checks-passed)
					      (string-constant test-engine-0-tests-passed))
					  "\n")]
                          [else 
			   (format
			    (string-append (if ck?
					       (string-constant test-engine-m-of-n-checks-failed)
					       (string-constant test-engine-m-of-n-tests-failed))
					   "\n\n")
			    failed total)]))))]
	     [check-outcomes/check
	      (lambda (zero-message)
		(outcomes total-checks failed-checks
			  zero-message #t))]
	     [check-outcomes/test
	      (lambda (zero-message)
		(outcomes total-checks failed-checks
			  zero-message #f))]
	     [test-outcomes 
	      (lambda (zero-message)
		(outcomes total-tests failed-tests
			  zero-message #f))])
        (case style
          [(test-require)
           (test-outcomes
	    (string-append (string-constant test-engine-must-be-tested) "\n"))
           (check-outcomes/check
	    (string-append (string-constant test-engine-is-unchecked) "\n"))]
          [(check-require)
           (check-outcomes/check
	    (string-append (string-constant test-engine-is-unchecked) "\n"))]
          [(test-basic)
           (test-outcomes "")
           (check-outcomes/check "")]
          [(test-check)
           (check-outcomes/test
	    (string-append (string-constant test-engine-must-be-tested)
			   "\n"))]
          [else (check-outcomes/check "")])

        (unless (and (zero? total-checks) (zero? total-tests))
          (inner (display-check-failures (send test-info failed-checks) 
                                         editor test-info src-editor)
                 insert-test-results editor test-info src-editor))))

    (define/public (display-check-failures checks editor test-info src-editor)
      (for ([failed-check (reverse checks)])
        (send editor insert "\t")
        (if (failed-check-exn? failed-check)
            (make-error-link editor
                             (failed-check-reason failed-check)
                             (failed-check-exn? failed-check)
                             (check-fail-src (failed-check-reason failed-check))
                             src-editor)
            (make-link editor
                       (failed-check-reason failed-check)
                       (check-fail-src (failed-check-reason failed-check))
                       src-editor))
        (send editor insert "\n")))

    ;next-line: editor% -> void
    ;Inserts a newline and a tab into editor
    (define/public (next-line editor) (send editor insert "\n\t"))

    ;; make-link: text% check-fail src editor -> void
    (define (make-link text reason dest src-editor)
      (display-reason text reason)
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

    (define (display-reason text fail)
      #;(write (list 'display-reason fail (check-fail? fail) (message-error? fail))
	     (current-error-port))
      #;(newline (current-error-port))
		   
      (let* ((print-string
	      (lambda (m)
		(send text insert m)))
	     (print-formatted
	      (lambda (m)
		(when (is-a? m snip%)
		  (send m set-style (send (send text get-style-list)
					  find-named-style "Standard")))
		(send text insert m)))
	     (print
	      (lambda (fstring . vals)
		(apply print-with-values fstring print-string print-formatted vals)))
	     (formatter (check-fail-format fail)))
	(cond
	 [(unexpected-error? fail)
	  (print (string-constant test-engine-check-encountered-error)
		 (formatter (unexpected-error-expected fail))
		 (unexpected-error-message fail))]
	 [(unequal? fail)
	  (print (string-constant test-engine-actual-value-differs-error)
		 (formatter (unequal-test fail))
		 (formatter (unequal-actual fail)))]
	 [(outofrange? fail)
	  (print (string-constant test-engine-actual-value-not-within-error)
		 (formatter (outofrange-test fail))
		 (outofrange-range fail)
		 (formatter (outofrange-actual fail)))]
	 [(incorrect-error? fail)
	  (print (string-constant test-engine-encountered-error-error)
		 (incorrect-error-expected fail)
		 (incorrect-error-message fail))]
	 [(expected-error? fail)
	  (print (string-constant test-engine-expected-error-error)
		 (formatter (expected-error-value fail))
		 (expected-error-message fail))]
	 [(message-error? fail)
	  (for-each print-formatted (message-error-strings fail))]
         [(not-mem? fail)
          (print (string-constant test-engine-not-mem-error)
                 (formatter (not-mem-test fail)))
          (for-each (lambda (a) (print " ~F" (formatter a))) (not-mem-set fail))
          (print ".")]
         [(not-range? fail)
          (print (string-constant test-engine-not-range-error)
                 (formatter (not-range-test fail))
                 (formatter (not-range-min fail))
                 (formatter (not-range-max fail)))]
         )
	(print-string "\n")))
    
    ;; make-error-link: text% check-fail exn src editor -> void
    (define (make-error-link text reason exn dest src-editor)
      (make-link text reason dest src-editor)
      #;(let ((start (send text get-end-position)))
        (send text insert (string-constant test-engine-trace-error))
	(send text insert " ")
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
	(let ([line (cond [(src-line src) => number->string]
			  [else 
			   (string-constant test-engine-unknown)])]
	      [col
	       (cond [(src-col src) => number->string]
		     [else (string-constant test-engine-unknown)])])  
	  (string-append
	   " "
	   (cond
	    [(or (symbol? (src-file src))
		 (is-a? (src-file src) editor<%>))
	     (format (string-constant test-engine-at-line-column) line col)]
	    [(path? (src-file src)) 
	     (format (string-constant test-engine-in-at-line-column)
		     (path->string (src-file src))
		     line col)])))))

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
  (class* frame:standard-menus% ()

    (super-instantiate
     ((string-constant test-engine-window-title) #f 400 350))

    (define switch-func void)
    (define disable-func void)
    (define close-cleanup void)

    (inherit get-area-container)
    
    (define content
      (make-object editor-canvas% (get-area-container) #f '(auto-vscroll)))

    (define button-panel
      (make-object horizontal-panel% (get-area-container)
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

    (define/override (edit-menu:between-select-all-and-find menu) (void))
    
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
