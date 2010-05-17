#lang scheme/base

(require (prefix-in fw: framework)
         mrlib/hierlist
         scheme/gui/base
         scheme/class
         scheme/contract
         tests/utils/gui)

  (provide/contract 
   [use-get/put-dialog (-> (-> any) path? void?)]
   [set-module-language! (->* () (boolean?) void?)])
  
  (provide fire-up-drscheme-and-run-tests
           save-drscheme-window-as
           do-execute
           test-util-error
           poll-until
           wait-for-computation
           wait-for-drscheme-frame
           wait-for-new-frame
           clear-definitions
           type-in-definitions
           type-in-interactions
           insert-in-definitions
           insert-in-interactions
           type-string
           wait
           wait-pending
           get-sub-panel
           get-text-pos
           wait-for-button
           push-button-and-wait
           set-language-level!
           repl-in-edit-sequence?
           fetch-output
           has-error?
           run-one/sync
           alt-return-in-interactions)
  
  ;; save-drscheme-window-as : string -> void
  ;; use the "save as" dialog in drscheme to save the definitions
  ;; window to a file.
  (define (save-drscheme-window-as filename)
    (use-get/put-dialog
     (lambda ()
       (fw:test:menu-select "File" "Save Definitions As..."))
     filename))

  ;; open-dialog is a thunk that should open the dialog
  ;; filename is a string naming a file that should be typed into the dialog
  (define (use-get/put-dialog open-dialog filename)
    (let ([drs (wait-for-drscheme-frame)])
      (with-handlers ([(lambda (x) #t)
		       (lambda (x)
			 (fw:preferences:set 'framework:file-dialogs 'std)
			 (raise x))])
	(fw:preferences:set 'framework:file-dialogs 'common)
	(open-dialog)
	(let ([dlg (wait-for-new-frame drs)])
	  (send (find-labelled-window "Filename:") focus)
	  (fw:test:keystroke #\a (list (case (system-type)
					 [(windows) 'control]
					 [(macosx macos) 'meta]
					 [(unix) 'meta]
                                         [else (error 'use-get/put-dialog "unknown platform: ~s\n"
                                                      (system-type))])))
	  (for-each fw:test:keystroke (string->list (path->string filename)))
	  (fw:test:button-push "OK")
	  (wait-for-new-frame dlg))
	(fw:preferences:set 'framework:file-dialogs 'std))))

  (define (test-util-error fmt . args)
    (raise (make-exn (apply fmt args) (current-continuation-marks))))
  
  ;; poll-until : (-> alpha) number (-> alpha) -> alpha
  ;; waits until pred return a true value and returns that.
  ;; if that doesn't happen by `secs', calls fail and returns that.
  (define poll-until
    (lambda (pred [secs 10] [fail (lambda ()
                                    (error 'poll-until 
                                           "timeout after ~e secs, ~e never returned a true value"
                                           secs pred))])
      (let ([step 1/20])
        (let loop ([counter secs])
          (if (<= counter 0)
              (fail)
              (let ([result (pred)])
                (or result
                    (begin
                      (sleep step)
                      (loop (- counter step))))))))))
  
  (define (drscheme-frame? frame)
    (method-in-interface? 'get-execute-button (object-interface frame)))
  
  (define (wait-for-drscheme-frame [print-message? #f])
    (let ([wait-for-drscheme-frame-pred
           (lambda ()
             (let ([active (get-top-level-focus-window)])
               (if (and active
                        (drscheme-frame? active))
                   active
                   #f)))])
      (or (wait-for-drscheme-frame-pred)
          (begin
            (when print-message?
              (printf "Select DrRacket frame~n"))
            (poll-until wait-for-drscheme-frame-pred)))))
  
  ;; wait-for-new-frame : frame [(listof eventspace) = null] -> frame
  ;; returns the newly opened frame, waiting until old-frame
  ;; is no longer frontmost. Optionally checks other eventspaces
  ;; waits until the new frame has a focus'd window, too. 
  (define wait-for-new-frame
    (case-lambda
     [(old-frame) (wait-for-new-frame old-frame null)]
     [(old-frame extra-eventspaces)
      (wait-for-new-frame old-frame extra-eventspaces 10)]
     [(old-frame extra-eventspaces timeout)
      (let ([wait-for-new-frame-pred
	     (lambda ()
	       (let ([active (or (get-top-level-focus-window)
				 (ormap
				  (lambda (eventspace)
				    (parameterize ([current-eventspace eventspace])
				      (get-top-level-focus-window)))
				  extra-eventspaces))])
		 (if (and active
                          (send active get-focus-window)
			  (not (eq? active old-frame)))
		     active
		     #f)))])
	(poll-until wait-for-new-frame-pred timeout))]))

  ;; wait-for-computation : frame -> void
  ;; waits until the drscheme frame finishes some computation.
  ;; uses the state of the execute button to indicate when the
  ;; computations is finished. That is, waits for the execute
  ;; button to dim, indicating a computation is running. Then,
  ;; waits for it to be re-enabled, indicating that the computation
  ;; is complete.
  (define (wait-for-computation frame)
    (verify-drscheme-frame-frontmost 'wait-for-computation frame)
    (let* ([wait-for-computation-to-start
	    (lambda ()
	      (fw:test:reraise-error)
	      (not (send (send frame get-execute-button) is-enabled?)))]
           [wait-for-computation-to-finish
	    (lambda ()
	      (fw:test:reraise-error)
	      (send (send frame get-execute-button) is-enabled?))])
      ;(poll-until wait-for-computation-to-start 60) ;; hm.
      (poll-until wait-for-computation-to-finish 60)))

  (define do-execute 
    (case-lambda
     [(frame)
      (do-execute frame #t)]
     [(frame wait-for-finish?)
      (verify-drscheme-frame-frontmost 'do-execute frame)
      (let ([button (send frame get-execute-button)])
	(fw:test:run-one (lambda () (send button command)))
	(when wait-for-finish?
	  (wait-for-computation frame)))]))
  
  (define (verify-drscheme-frame-frontmost function-name frame)
    (let ([tl (get-top-level-focus-window)])
      (unless (and (eq? frame tl)
                   (drscheme-frame? tl))
        (error function-name "drscheme frame not frontmost: ~e (found ~e)" frame tl))))
  
  (define (clear-definitions frame)
    (verify-drscheme-frame-frontmost 'clear-definitions frame)
    (fw:test:new-window (send frame get-definitions-canvas))
    (let ([window (send frame get-focus-window)])
      (let-values ([(cw ch) (send window get-client-size)]
		   [(w h) (send window get-size)])
	(fw:test:mouse-click 'left
			     (inexact->exact (+ cw (floor (/ (- w cw) 2))))
			     (inexact->exact (+ ch (floor (/ (- h ch) 2)))))))
    (fw:test:menu-select "Edit" "Select All")
    (fw:test:menu-select "Edit" (if (eq? (system-type) 'macos)
				    "Clear"
				    "Delete")))

  (define (type-in-definitions frame str)
    (put-in-frame (lambda (x) (send x get-definitions-canvas)) frame str #f 'type-in-definitions))
  (define (type-in-interactions frame str)
    (put-in-frame (lambda (x) (send x get-interactions-canvas)) frame str #f 'type-in-interactions))
  (define (insert-in-definitions frame str)
    (put-in-frame (lambda (x) (send x get-definitions-canvas)) frame str #t 'insert-in-definitions))
  (define (insert-in-interactions frame str)
    (put-in-frame (lambda (x) (send x get-interactions-canvas)) frame str #t 'insert-in-interactions))

  (define (put-in-frame get-canvas frame str/sexp just-insert? who)
    (unless (and (object? frame) (is-a? frame top-level-window<%>))
      (error who "expected a frame or a dialog as the first argument, got ~e" frame))
    (let ([str (if (string? str/sexp)
		   str/sexp
		   (let ([port (open-output-string)])
		     (parameterize ([current-output-port port])
		       (write str/sexp port))
		     (get-output-string port)))])
      (verify-drscheme-frame-frontmost who frame)
      (let ([canvas (get-canvas frame)])
	(fw:test:new-window canvas)
	(let ([editor (send canvas get-editor)])
          (cond
            [just-insert? 
             (let ([s (make-semaphore 0)])
               (queue-callback
                (λ () 
                  (send editor set-caret-owner #f)
                  (send editor insert str)
                  (semaphore-post s)))
               (unless (sync/timeout 3 s)
                 (error who "callback didn't run for 3 seconds; trying to insert ~s" str/sexp)))]
            [else 
             (send editor set-caret-owner #f)
             (type-string str)])))))
  
  (define (alt-return-in-interactions frame)
    (verify-drscheme-frame-frontmost 'alt-return-in-interactions frame)
    (let ([canvas (send frame get-interactions-canvas)])
      (fw:test:new-window canvas)
      (let ([editor (send canvas get-editor)])
        (send editor set-caret-owner #f)
        (fw:test:keystroke #\return '(alt)))))
        
  ;; type-string : string -> void
  ;; to call test:keystroke repeatedly with the characters
  (define (type-string str)
    (let ([len (string-length str)])
      (let loop ([i 0])
        (unless (>= i len)
          (let ([c (string-ref str i)])
            (fw:test:keystroke
             (if (char=? c #\newline)
                 #\return
                 c)))
          (loop (+ i 1))))))
  
  (define wait
    (case-lambda 
     [(test desc-string) (wait test desc-string 5)]
     [(test desc-string time)
      (let ([int 1/2])
	(let loop ([sofar 0])
	  (cond
	    [(> sofar time) (error 'wait desc-string)]
	    [(test) (void)]
	    [else (sleep int)
		  (loop (+ sofar int))])))]))
  
  (define (wait-pending)
    (wait (lambda () (= 0 (fw:test:number-pending-actions)))
	  "Pending actions didn't terminate")
    (fw:test:reraise-error))
  
  
;;; get-sub-panel takes 
;;;    a list of integers describing the path from a frame to the desired panel
;;;    the frame
;;;    based on code by Mark Krentel
  
;;;    Examples:
;;;    (get-sub-panel '() frame) gets the top-panel in frame
;;;    (get-sub-panel '(2) frame) gets the 2nd child of the top-panel 
;;;    (get-sub-panel '(2 0) frame) gets the 0th child of the 2nd child of the top-panel 
  
  (define (get-sub-panel path frame)
    (letrec ([loop 
	      (lambda (path panel)
		(if (null? path)
		    (if (is-a? panel panel%)
			panel
			(test-util-error "not a panel")) 
		    (loop
		     (cdr path)
		     (list-ref (send panel get-children) (car path)))))])
      (loop path frame)))
  
;;; get-text-pos returns the offset in an text buffer of the beginning
;;; of the last line
  
  (define (get-text-pos text)
    (let* ([last-pos (send text last-position)]
	   [last-line (send text position-line last-pos)])
      (send text line-start-position last-line)))
  
  ; poll for enabled button
  
  (define (wait-for-button button)
    (poll-until
     (let ([wait-for-button-pred
	    (lambda ()
	      (send button is-enabled?))])
       wait-for-button-pred)))
  
  (define (push-button-and-wait button)
    (fw:test:button-push button)
    (poll-until
     (let ([button-push-and-wait-pred
	    (lambda ()
	      (fw:test:reraise-error)
	      (= 0 (fw:test:number-pending-actions)))])
       button-push-and-wait-pred))
    (wait-for-button button))
  
  ;; set-language-level! : (cons (union regexp string) (listof (union regexp string))) boolean -> void
  ;; set language level in the frontmost DrRacket frame (resets settings to defaults)
  ;; If `close-dialog?' it #t,
  (define set-language-level! 
    (lambda (in-language-spec [close-dialog? #t])
      (unless (and (pair? in-language-spec)
                   (list? in-language-spec)
                   (andmap (lambda (x) (or string? regexp?)) in-language-spec))
        (error 'set-language-level! "expected a non-empty list of regexps and strings for language, got: ~e" in-language-spec))
      (let ([drs-frame (get-top-level-focus-window)])
        (fw:test:menu-select "Language" "Choose Language...")
        (let* ([language-dialog (wait-for-new-frame drs-frame)]
               [language-choice (find-labelled-window #f hierarchical-list%)]
               [b1 (box 0)]
               [b2 (box 0)]
               [click-on-snip
                (lambda (snip)
                  (let* ([editor (send (send snip get-admin) get-editor)]
                         [between-threshold (send editor get-between-threshold)])
                    (send editor get-snip-location snip b1 b2)
                    (let-values ([(gx gy) (send editor editor-location-to-dc-location
                                                (unbox b1)
                                                (unbox b2))])
                      (let ([x (inexact->exact (+ gx between-threshold 1))]
                            [y (inexact->exact (+ gy between-threshold 1))])
                        (fw:test:mouse-click 'left x y)))))])
          (send language-choice focus)
          (let loop ([list-item language-choice]
                     [language-spec in-language-spec])
            (let* ([name (car language-spec)]
                   [which (filter (lambda (child)
                                    (let* ([text (send (send child get-editor) get-text)]
                                           [matches
                                            (or (and (regexp? name)
                                                     (regexp-match name text))
                                                (and (string? name)
                                                     (string=? name text)))])
                                      (and matches
                                           child)))
                                  (send list-item get-items))])
              (when (null? which)
                (error 'set-language-level! "couldn't find language: ~e, no match at ~e"
                       in-language-spec name))
              (unless (= 1 (length which))
                (error 'set-language-level! "couldn't find language: ~e, double match ~e"
                       in-language-spec name))
              (let ([next-item (car which)])
                (cond
                  [(null? (cdr language-spec))
                   (when (is-a? next-item hierarchical-list-compound-item<%>)
                     (error 'set-language-level! "expected no more languages after ~e, but still are, input ~e"
                            name in-language-spec))
                   (click-on-snip (send next-item get-clickable-snip))]
                  [else
                   (unless (is-a? next-item hierarchical-list-compound-item<%>)
                     (error 'set-language-level! "expected more languages after ~e, but got to end, input ~e"
                            name in-language-spec))
                   (unless (send next-item is-open?)
                     (click-on-snip (send next-item get-arrow-snip)))
                   (loop next-item (cdr language-spec))]))))
          
          (with-handlers ([exn:fail? (lambda (x) (void))])
            (fw:test:button-push "Show Details"))
          
          (fw:test:button-push "Revert to Language Defaults")
          
          (when close-dialog?
            (fw:test:button-push "OK")
            (let ([new-frame (wait-for-new-frame language-dialog)])
              (unless (eq? new-frame drs-frame)
                (error 'set-language-level! 
                       "didn't get drscheme frame back, got: ~s (drs-frame ~s)\n"
                       new-frame
                       drs-frame)))))))) 
  (define (set-module-language! [close-dialog? #t])
    (let ([drs-frame (get-top-level-focus-window)])
      (fw:test:menu-select "Language" "Choose Language...")
      (let* ([language-dialog (wait-for-new-frame drs-frame)])
        (fw:test:set-radio-box-item! "Use the language declared in the source")
        
        (with-handlers ([exn:fail? (lambda (x) (void))])
          (fw:test:button-push "Show Details"))
        
        (fw:test:button-push "Revert to Language Defaults")
        
        (when close-dialog?
          (fw:test:button-push "OK")
          (let ([new-frame (wait-for-new-frame language-dialog)])
            (unless (eq? new-frame drs-frame)
              (error 'set-language-level! 
                     "didn't get drscheme frame back, got: ~s (drs-frame ~s)\n"
                     new-frame
                     drs-frame)))))))
  
  (provide/contract [check-language-level ((or/c string? regexp?) . -> . void?)])
  ;; checks that the language in the drscheme window is set to the given one.
  ;; clears the definitions, clicks execute and checks the interactions window.
  (define (check-language-level lang-spec)
    (let* ([drs-frame (wait-for-drscheme-frame)]
           [interactions (send drs-frame get-interactions-text)]
           [definitions-canvas (send drs-frame get-definitions-canvas)])
      (fw:test:new-window definitions-canvas)
      (fw:test:menu-select "Edit" "Select All")
      (fw:test:menu-select "Edit" "Delete")
      (do-execute drs-frame)
      (let ([lang-line (send interactions get-text
                             (send interactions line-start-position 1)
                             (send interactions line-end-position 1))])
        (unless (regexp-match lang-spec lang-line)
          (error 'check-language-level "expected ~s to match ~s"
                 lang-line lang-spec)))))
  
  
  (define (repl-in-edit-sequence?)
    (send (send (wait-for-drscheme-frame) get-interactions-text) refresh-delayed?))
 
  ;; has-error? : frame -> (union #f string)
  ;; returns the error text of an error in the interactions window of the frame or #f if there is none.
  ;; ensures that frame is front most.
  (define (has-error? frame)
    (run-one/sync
     (lambda ()
       (verify-drscheme-frame-frontmost 'had-error? frame)
       (let* ([interactions-text (send frame get-interactions-text)]
              [last-para (send interactions-text last-paragraph)])
         (unless (>= last-para 2)
           (error 'has-error? "expected at least 2 paragraphs in interactions window, found ~a"
                  (+ last-para 1)))
         (let ([start (send interactions-text paragraph-start-position 2)]
               [end (send interactions-text paragraph-end-position
                          (- (send interactions-text last-paragraph) 1))])
           (send interactions-text split-snip start)
           (send interactions-text split-snip end)
           (let loop ([pos start])
             (cond
               [(<= end pos) #f]
               [else
                (let ([snip (send interactions-text find-snip pos 'after-or-none)])
                  (cond
                    [(not snip) #f]
                    [else
                     (let ([color (send (send snip get-style) get-foreground)])
                       (if (and (= 255 (send color red))
                                (= 0 (send color blue) (send color green)))
                           
                           ;; return the text of the entire line containing the red text
                           (let ([para (send interactions-text position-paragraph pos)])
                             (unless (exact-nonnegative-integer? para)
                               (error 'has-error? "got back a bad result from position-paragraph: ~s ~s\n" 
                                      para
                                      (list pos (send interactions-text last-position))))
                             (send interactions-text get-text
                                   (send interactions-text paragraph-start-position para)
                                   (send interactions-text paragraph-end-position para)))
                           
                           (loop (+ pos (send snip get-count)))))]))])))))))

  (define fetch-output
    (case-lambda
      [(frame) (fetch-output frame #f #f)]
      [(frame _start _end)
       (run-one/sync
        (lambda ()
          (verify-drscheme-frame-frontmost 'fetch-output frame)
          (let-values ([(start end)
                        (if (and _start _end)
                            (values _start _end)
                            (let* ([interactions-text (send frame get-interactions-text)]
                                   [last-para (send interactions-text last-paragraph)])
                              (unless (>= last-para 2)
                                (error 'fetch-output "expected at least 2 paragraphs in interactions window, found ~a"
                                       (+ last-para 1)))
                              (values (send interactions-text paragraph-start-position 2)
                                      (send interactions-text paragraph-end-position
                                            (- (send interactions-text last-paragraph) 1)))))])
            (let ([interactions-text (send frame get-interactions-text)])
              
              (send interactions-text split-snip start)
              (send interactions-text split-snip end)
              
              (let loop ([snip (send interactions-text find-snip end 'before)]
                         [strings null])
                (cond
                  [(< (send interactions-text get-snip-position snip) start)
                   (apply string-append strings)]
                  [else 
                   (cond
                     [(is-a? snip string-snip%)
                      (loop (send snip previous)
                            (cons (send snip get-text 0 (send snip get-count)) strings))]
                     [(is-a? snip editor-snip%)
                      (let ([editor (send snip get-editor)])
                        (cond
                          [(is-a? editor pasteboard%)
                           (loop (send snip previous)
                                 (cons "<pasteboard>" strings))]
                          [(is-a? editor text%)
                           (loop (send snip previous)
                                 (list* "{embedded \""
                                        (send editor get-text)
                                        "\"}"
                                        strings))]))]
                     [(is-a? snip image-snip%)
                      (loop (send snip previous)
                            (cons (with-handlers ([exn:fail? (lambda (x) "{image}")])
                                    (format "{~a}" (send snip get-image-name)))
                                  strings))]
                     
                     [;; this test is an approximation of
                      ;; (is-a? snip drscheme:snip:number-snip%)
                      (and (method-in-interface? 'get-fraction-view (object-interface snip)))
                      (loop (send snip previous)
                            (cons (format "{number ~s ~s ~s}"
                                          (send snip get-number)
                                          (send snip get-text 0 (send snip get-count))
                                          (send snip get-fraction-view))
                                  strings))]
                     
                     [else
                      (loop (send snip previous)
                            (cons (format "{unknown snip: ~e}~n" snip)
                                  strings))])]))))))]))
  
  ;; run-one/sync : (-> A) -> A
  ;; runs the thunk `f' as a test action, and
  ;; waits for it to complete. Also propogates
  ;; exceptions.
  (define (run-one/sync f)
    (let ([s (make-semaphore 0)]
          [raised-exn? #f]
          [exn #f]
          [anss #f])
      (fw:test:run-one
       (lambda ()
         (with-handlers ([exn:fail? (lambda (-exn)
                                      (set! raised-exn? #t)
                                      (set! exn -exn))])
           (call-with-values f (lambda x (set! anss x))))
         (semaphore-post s)))
      (semaphore-wait s)
      (if raised-exn?
          (raise exn)
          (apply values anss))))
  
  ;; this is assumed to not open an windows or anything like that
  ;; but just to print and return.
  (define orig-display-handler (error-display-handler))
  
  (define (fire-up-drscheme-and-run-tests run-test)
    (let ()
      ;; change the preferences system so that it doesn't write to 
      ;; a file; partly to avoid problems of concurrency in drdr
      ;; but also to make the test suite easier for everyone to run.
      (let ([prefs-table (make-hash)])
	(fw:preferences:low-level-put-preferences
	 (lambda (names vals)
	   (for-each (lambda (name val) (hash-set! prefs-table name val))
		     names vals)))
	(fw:preferences:low-level-get-preference 
	 (lambda (name [fail (lambda () #f)])
	   (hash-ref prefs-table name fail))))
	 
      (dynamic-require 'drscheme #f)

      ;; set all preferences to their defaults (some pref values may have
      ;; been read by this point, but hopefully that won't affect much
      ;; of the startup of drscheme)
      (fw:preferences:restore-defaults)

      (thread (λ () 
		 (let ([orig-display-handler (error-display-handler)])
		   (uncaught-exception-handler
		    (λ (x)
		       (if (exn? x)
			   (orig-display-handler (exn-message x) x)
			   (fprintf (current-error-port) "uncaught exception ~s\n" x))
		       (exit 1))))
		 (run-test)
		 (exit)))
      (yield (make-semaphore 0))))
