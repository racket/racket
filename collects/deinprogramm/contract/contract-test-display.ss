#lang scheme/base

; DeinProgramm version of collects/test-engine/test-display.ss
; synched with SVN rev 11385

(provide contract-test-display%)

(require scheme/class
         scheme/file
         mred
         framework
         string-constants
         (lib "test-engine/test-info.scm")
         (lib "test-engine/test-engine.scm")
	 deinprogramm/contract/contract
	 deinprogramm/contract/contract-test-engine)

(define contract-test-display%
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
        (let ([curr-win (and current-tab (send current-tab get-test-window))])
	  (when curr-win
	    (let ([content (make-object (editor:standard-style-list-mixin text%))])
	      (send content lock #t)
	      (when curr-win (send curr-win update-editor content))
	      (when current-tab (send current-tab current-test-editor content))
	      (when (docked?)
		(send drscheme-frame display-test-panel content)
		(send curr-win show #f)))))))

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
             [total-checks (send test-info checks-run)]
             [failed-checks (send test-info checks-failed)]
	     [violated-contracts (send test-info failed-contracts)]
             [check-outcomes
              (lambda (zero-message)
                (send editor insert
                      (cond
                        [(zero? total-checks) zero-message]
                        [(= 1 total-checks) "Ran 1 check.\n"]
                        [else (format "Ran ~a checks.\n" total-checks)]))
                (when (> total-checks 0)
                  (send editor insert
                        (cond
                          [(and (zero? failed-checks) (= 1 total-checks))
                           "Check passed!\n\n"]
                          [(zero? failed-checks) "All checks passed!\n\n"]
                          [(= failed-checks total-checks) "0 checks passed.\n"]
                          [else (format "~a of the ~a checks failed.\n\n"
                                        failed-checks total-checks)])))
		(send editor insert
		      (cond
		       ((null? violated-contracts)
			"No contract violations!\n\n")
		       (else
			(format "~a contract violations.\n\n"
				(length violated-contracts)))))
		)])
        (case style
          [(check-require)
           (check-outcomes "This program is unchecked!\n")]
          [else (check-outcomes "")])

        (unless (and (zero? total-checks)
		     (null? violated-contracts))
          (inner (begin
		   (display-check-failures (send test-info failed-checks) 
					   editor test-info src-editor)
		   (send editor insert "\n")
		   (display-contract-violations violated-contracts
						editor test-info src-editor))
                 insert-test-results editor test-info src-editor))))

    (define/public (display-check-failures checks editor test-info src-editor)
      (when (pair? checks)
	(send editor insert "Check failures:\n"))
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

    (define/public (display-contract-violations violations editor test-info src-editor)
      (when (pair? violations)
	(send editor insert "Contract violations:\n"))
      (for-each (lambda (violation)
		  (send editor insert "\t")
		  (make-contract-link editor violation src-editor)
		  (send editor insert "\n"))
		violations))

    ;next-line: editor% -> void
    ;Inserts a newline and a tab into editor
    (define/public (next-line editor) (send editor insert "\n\t"))

    ;; make-link: text% (listof (U string snip%)) src editor -> void
    (define (make-link text msg dest src-editor)
      (insert-messages text msg)
      (let ((start (send text get-end-position)))
        (send text insert (format-src dest))
        (when (and src-editor current-rep)
          (send text set-clickback
                start (send text get-end-position)
                (lambda (t s e) (highlight-check-error dest src-editor))
                #f #f)
	  (set-clickback-style text start "royalblue"))))
    
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
	  (set-clickback-style text start "red"))))

    (define (insert-messages text msgs)
      (for ([m msgs])
        (when (is-a? m snip%)
          (send m set-style (send (send text get-style-list)
                                  find-named-style "Standard")))
        (send text insert m)))

    (define (make-contract-link text violation src-editor)
      (let* ((contract (contract-violation-contract violation))
	     (stx (contract-syntax contract))
	     (srcloc (contract-violation-srcloc violation)))
	(insert-messages text (contract-violation-messages violation))
	(when srcloc
	  (send text insert " ")
	  (let ((source (srcloc-source srcloc))
		(line (srcloc-line srcloc))
		(column (srcloc-column srcloc))
		(pos (srcloc-position srcloc))
		(span (srcloc-span srcloc))
		(start (send text get-end-position)))
	    (send text insert (format-position source line column))
	    (send text set-clickback
		  start (send text get-end-position)
		  (lambda (t s e)
		    (highlight-error line column pos span src-editor))
		  #f #f)
	    (set-clickback-style text start "blue")))
	(send text insert ", contract ")
	(format-clickable-syntax-src text stx src-editor)
	(cond
	 ((contract-violation-blame violation)
	  => (lambda (blame)
	       (next-line text)
	       (send text insert "to blame: procedure ")
	       (format-clickable-syntax-src text blame src-editor))))))
	
    (define (format-clickable-syntax-src text stx src-editor)
      (let ((start (send text get-end-position)))
	(send text insert (format-syntax-src stx))
	(send text set-clickback
	      start (send text get-end-position)
	      (lambda (t s e)
		(highlight-error/syntax stx src-editor))
	      #f #f)
	(set-clickback-style text start "blue")))

    (define (set-clickback-style text start color)
      (let ([end (send text get-end-position)]
	    [c (new style-delta%)])
	(send text insert " ")
	(send text change-style
	      (make-object style-delta% 'change-underline #t)
	      start end #f)
	(send c set-delta-foreground color)
	(send text change-style c start end #f)))

    (define (format-syntax-src stx)
      (format-position (syntax-source stx) 
		       (syntax-line stx) (syntax-column stx)))

    ;format-src: src -> string
    (define (format-src src)
      (format-position (car src) (cadr src) (caddr src)))

    (define (format-position file line column)
      (string-append
       (if (path? file)
	   (let-values (((base name must-be-dir?)
			 (split-path file)))
	     (if (path? name)
		 (string-append " in " (path->string name) " at ")
		 ""))
	   "")
       "at line " (cond [line => number->string]
		     [else "(unknown)"])
       " column " (cond [column => number->string]
			[else "(unknown)"])))

    (define (highlight-error line column position span src-editor)
      (when (and current-rep src-editor)
	(cond
	 [(is-a? src-editor text:basic<%>)
	  (let ((highlight
		 (lambda ()
		   (send current-rep highlight-errors
			 (list (make-srcloc src-editor
					    line
					    column
					    position span)) #f))))
	    (queue-callback highlight))])))

    (define (highlight-check-error srcloc src-editor)
      (let* ([src-pos cadddr]
             [src-span (lambda (l) (car (cddddr l)))]
             [position (src-pos srcloc)]
             [span (src-span srcloc)])
	(highlight-error (cadr srcloc) (caddr srcloc)
			 position span
			 src-editor)))

    (define (highlight-error/syntax stx src-editor)
      (highlight-error (syntax-line stx) (syntax-column stx)
		       (syntax-position stx) (syntax-span stx)
		       src-editor))

    (super-instantiate ())))

(define test-window%
  (class* frame% ()

    (super-instantiate
     ((string-constant test-engine-window-title) #f 400 350))

    ;; (define editor #f)
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
      ;;(set! editor e)
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

