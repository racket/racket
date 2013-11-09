#lang scheme/base

(require (prefix-in fw: framework)
         mrlib/hierlist
         scheme/gui/base
         scheme/class
         scheme/contract
         "gui.rkt")

  (provide/contract 
   [use-get/put-dialog (-> (-> any) path? void?)]
   [set-module-language! (->* () (boolean?) void?)])
  
  (provide queue-callback/res
           fire-up-drracket-and-run-tests
           fire-up-separate-drracket-and-run-tests
           save-drracket-window-as
           do-execute
           test-util-error
           poll-until
           wait-for-computation
           wait-for-drracket-frame
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
  
  ;; save-drracket-window-as : string -> void
  ;; use the "save as" dialog in drracket to save the definitions
  ;; window to a file.
  (define (save-drracket-window-as filename)
    (not-on-eventspace-handler-thread 'save-drracket-window-as)
    (use-get/put-dialog
     (lambda ()
       (fw:test:menu-select "File" "Save Definitions As..."))
     filename))

  ;; open-dialog is a thunk that should open the dialog
  ;; filename is a string naming a file that should be typed into the dialog
  (define (use-get/put-dialog open-dialog filename)
    (not-on-eventspace-handler-thread 'use-get/put-dialog)
    (let ([drs (wait-for-drracket-frame)])
      (with-handlers ([(lambda (x) #t)
		       (lambda (x)
			 (fw:preferences:set 'framework:file-dialogs 'std)
			 (raise x))])
	(fw:preferences:set 'framework:file-dialogs 'common)
	(open-dialog)
	(let ([dlg (wait-for-new-frame drs)])
	  (send (find-labelled-window "Filename:" #f (fw:test:get-active-top-level-window)) focus)
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
  (define (poll-until pred
                      [secs 10]
                      [fail (lambda ()
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
                    (loop (- counter step)))))))))
  
  (define (drracket-frame? frame)
    (method-in-interface? 'get-execute-button (object-interface frame)))
  
  (define (wait-for-drracket-frame [print-message? #f])
    (define (wait-for-drracket-frame-pred)
      (define active (fw:test:get-active-top-level-window))
      (if (and active
               (drracket-frame? active))
          active
          #f))
    (define drr-fr
      (or (wait-for-drracket-frame-pred)
          (begin
            (when print-message?
              (printf "Select DrRacket frame\n"))
            (poll-until wait-for-drracket-frame-pred))))
    (when drr-fr
      (wait-for-events-in-frame-eventspace drr-fr))
    drr-fr)
  
  ;; wait-for-new-frame : frame [(listof eventspace) = null] -> frame
  ;; returns the newly opened frame, waiting until old-frame
  ;; is no longer frontmost. Optionally checks other eventspaces
  ;; waits until the new frame has a focus'd window, too. 
  (define (wait-for-new-frame/proc old-frame old-frame-id-name [extra-eventspaces '()] [timeout 10])
    (define (wait-for-new-frame-pred)
      (define active (or (fw:test:get-active-top-level-window)
                         (for/or ([eventspace (in-list extra-eventspaces)]) 
                           (parameterize ([current-eventspace eventspace])
                             (fw:test:get-active-top-level-window)))))
      (if (and active
               (not (eq? active old-frame)))
          active
          #f))
    (define lab (send old-frame get-label))
    (define fr (poll-until 
                (procedure-rename wait-for-new-frame-pred
                                  (string->symbol
                                   (format "wait-for-new-frame-pred; old: ~s id: ~a"
                                           lab old-frame-id-name)))
                timeout))
    (when fr (wait-for-events-in-frame-eventspace fr))
    (sleep 1)
    fr)

(define-syntax-rule 
  (wait-for-new-frame a b ...)
  (wait-for-new-frame/proc a 'a b ...))
  
  (define (wait-for-events-in-frame-eventspace fr)
    (define sema (make-semaphore 0))
    (parameterize ([current-eventspace (send fr get-eventspace)])
      (queue-callback
       (λ () (semaphore-post sema))
       #f))
    (semaphore-wait sema))

  ;; wait-for-computation : frame -> void
  ;; waits until the drracket frame finishes some computation.
  ;; uses the state of the execute button to indicate when the
  ;; computations is finished. That is, waits for the execute
  ;; button to dim, indicating a computation is running. Then,
  ;; waits for it to be re-enabled, indicating that the computation
  ;; is complete.
  (define (wait-for-computation frame)
    (not-on-eventspace-handler-thread 'wait-for-computation)
    (queue-callback/res (λ () (verify-drracket-frame-frontmost 'wait-for-computation frame)))
    (let* ([wait-for-computation-to-start
	    (lambda ()
	      (fw:test:reraise-error)
	      (not (send (send frame get-execute-button) is-enabled?)))]
           [wait-for-computation-to-finish
	    (lambda ()
	      (fw:test:reraise-error)
	      (send (send frame get-execute-button) is-enabled?))])
      ;(poll-until wait-for-computation-to-start 60) ;; hm.
      (poll-until wait-for-computation-to-finish 60)
      (sync (system-idle-evt))))

  (define do-execute 
    (case-lambda
     [(frame)
      (do-execute frame #t)]
     [(frame wait-for-finish?)
      (not-on-eventspace-handler-thread 'do-execute)
      (queue-callback/res (λ () (verify-drracket-frame-frontmost 'do-execute frame)))
      (let ([button (queue-callback/res (λ () (send frame get-execute-button)))])
	(fw:test:run-one (lambda () (send button command)))
	(when wait-for-finish?
          (wait-for-computation frame)))]))
  
  (define (verify-drracket-frame-frontmost function-name frame)
    (on-eventspace-handler-thread 'verify-drracket-frame-frontmost)
    (let ([tl (fw:test:get-active-top-level-window)])
      (unless (and (eq? frame tl)
                   (drracket-frame? tl))
        (error function-name "drracket frame not frontmost: ~e (found ~e)" frame tl))))
  
  (define (clear-definitions frame)
    (queue-callback/res (λ () (verify-drracket-frame-frontmost 'clear-definitions frame)))
    (fw:test:new-window (queue-callback/res (λ () (send frame get-definitions-canvas))))
    (let ([window (queue-callback/res (λ () (send frame get-edit-target-window)))])
      (let-values ([(cw ch) (queue-callback/res (λ () (send window get-client-size)))]
                   [(w h) (queue-callback/res (λ () (send window get-size)))])
        (fw:test:mouse-click 'left
			     (inexact->exact (floor (+ cw (/ (- w cw) 2))))
			     (inexact->exact (floor (+ ch (/ (- h ch) 2)))))))
    (fw:test:menu-select "Edit" "Select All")
    (fw:test:menu-select "Edit" (if (eq? (system-type) 'macos)
				    "Clear"
				    "Delete")))

  (define (type-in-definitions frame str)
    (not-on-eventspace-handler-thread 'type-in-definitions)
    (put-in-frame (lambda (x) (send x get-definitions-canvas)) frame str #f 'type-in-definitions))
  (define (type-in-interactions frame str)
    (not-on-eventspace-handler-thread 'type-in-interactions)
    (put-in-frame (lambda (x) (send x get-interactions-canvas)) frame str #f 'type-in-interactions))
  (define (insert-in-definitions frame str)
    (not-on-eventspace-handler-thread 'insert-in-definitions)
    (put-in-frame (lambda (x) (send x get-definitions-canvas)) frame str #t 'insert-in-definitions))
  (define (insert-in-interactions frame str)
    (not-on-eventspace-handler-thread 'insert-in-interactions)
    (put-in-frame (lambda (x) (send x get-interactions-canvas)) frame str #t 'insert-in-interactions))

  (define (put-in-frame get-canvas frame str/sexp just-insert? who)
    (not-on-eventspace-handler-thread 'put-in-frame)
    (unless (and (object? frame) (is-a? frame top-level-window<%>))
      (error who "expected a frame or a dialog as the first argument, got ~e" frame))
    (let ([str (if (string? str/sexp)
		   str/sexp
		   (let ([port (open-output-string)])
		     (parameterize ([current-output-port port])
		       (write str/sexp port))
		     (get-output-string port)))])
      (queue-callback/res (λ () (verify-drracket-frame-frontmost who frame)))
      (let ([canvas (queue-callback/res (λ () (get-canvas frame)))])
	(fw:test:new-window canvas)
	(let ([editor (queue-callback/res (λ () (send canvas get-editor)))])
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
             (queue-callback/res (λ () (send editor set-caret-owner #f)))
             (type-string str)])))))
  
  (define (queue-callback/res thunk)
    (not-on-eventspace-handler-thread 'queue-callback/res)
    (let ([c (make-channel)])
      (queue-callback (λ () (channel-put c (with-handlers ((exn:fail? values))
                                             (call-with-values thunk list))))
                      #f)
      (define res (channel-get c))
      (when (exn? res) (raise res))
      (apply values res)))

  (define (alt-return-in-interactions frame)
    (not-on-eventspace-handler-thread 'alt-return-in-interactions)
    (queue-callback/res (λ () (verify-drracket-frame-frontmost 'alt-return-in-interactions frame)))
    (let ([canvas (send frame get-interactions-canvas)])
      (fw:test:new-window canvas)
      (let ([editor (send canvas get-editor)])
        (send editor set-caret-owner #f)
        (fw:test:keystroke #\return '(alt)))))
        
  ;; type-string : string -> void
  ;; to call test:keystroke repeatedly with the characters
  (define (type-string str)
    (not-on-eventspace-handler-thread 'type-string)
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
    (on-eventspace-handler-thread 'get-sub-panel)
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
    (on-eventspace-handler-thread 'get-text-pos)
    (let* ([last-pos (send text last-position)]
	   [last-line (send text position-line last-pos)])
      (send text line-start-position last-line)))
  
  ; poll for enabled button
  
  (define (wait-for-button button)
    (not-on-eventspace-handler-thread 'wait-for-button)
    (poll-until
     (let ([wait-for-button-pred
	    (lambda ()
              (queue-callback/res (λ () (send button is-enabled?))))])
       wait-for-button-pred)))
  
  (define (push-button-and-wait button)
    (not-on-eventspace-handler-thread 'push-button-and-wait)
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
        (error 'set-language-level!
               "expected a non-empty list of regexps and strings for language, got: ~e"
               in-language-spec))
      (not-on-eventspace-handler-thread 'set-language-level!)
      (let ([drs-frame (fw:test:get-active-top-level-window)])
        (fw:test:menu-select "Language" "Choose Language...")
        (define language-dialog (wait-for-new-frame drs-frame))
        (fw:test:set-radio-box-item! #rx"Other Languages")
        (define language-choices (find-labelled-windows #f hierarchical-list%
                                                        (fw:test:get-active-top-level-window)))
        (define b1 (box 0))
        (define b2 (box 0))
        (define (click-on-snip snip)
          (let* ([editor (send (send snip get-admin) get-editor)]
                 [between-threshold (send editor get-between-threshold)])
            (send editor get-snip-location snip b1 b2)
            (let-values ([(gx gy) (send editor editor-location-to-dc-location
                                        (unbox b1)
                                        (unbox b2))])
              (let ([x (inexact->exact (floor (+ gx between-threshold 1)))]
                    [y (inexact->exact (floor (+ gy between-threshold 1)))])
                (fw:test:mouse-click 'left x y)))))
          
        (define found-language? #f)
          
        (for ([language-choice (in-list language-choices)])
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
              (unless (null? which)
                (unless (= 1 (length which))
                  (error 'set-language-level! "couldn't find language: ~e, double match ~e"
                         in-language-spec name))
                (let ([next-item (car which)])
                  (cond
                    [(null? (cdr language-spec))
                     (when (is-a? next-item hierarchical-list-compound-item<%>)
                       (error 'set-language-level!
                              "expected no more languages after ~e, but still are, input ~e"
                              name in-language-spec))
                     (set! found-language? #t)
                     (click-on-snip (send next-item get-clickable-snip))]
                    [else
                     (unless (is-a? next-item hierarchical-list-compound-item<%>)
                       (error 'set-language-level!
                              "expected more languages after ~e, but got to end, input ~e"
                              name in-language-spec))
                     (unless (send next-item is-open?)
                       (click-on-snip (send next-item get-arrow-snip)))
                     (loop next-item (cdr language-spec))]))))))
        
        (unless found-language?
          (error 'set-language-level! "couldn't find language: ~e" in-language-spec))
        
        (with-handlers ([exn:fail? (lambda (x) (void))])
          (fw:test:button-push "Show Details"))
        
        (fw:test:button-push "Revert to Language Defaults")
        
        (when close-dialog?
          (fw:test:button-push "OK")
          (let ([new-frame (wait-for-new-frame language-dialog)])
            (unless (eq? new-frame drs-frame)
              (error 'set-language-level! 
                     "didn't get drracket frame back, got: ~s (drs-frame ~s)\n"
                     new-frame
                     drs-frame)))))))
  
  (define (set-module-language! [close-dialog? #t])
    (not-on-eventspace-handler-thread 'set-module-language!)
    (let ([drs-frame (fw:test:get-active-top-level-window)])
      (fw:test:menu-select "Language" "Choose Language...")
      (let* ([language-dialog (wait-for-new-frame drs-frame)])
        (fw:test:set-radio-box-item! #rx"The Racket Language")
        
        (with-handlers ([exn:fail? (lambda (x) (void))])
          (fw:test:button-push "Show Details"))
        
        (fw:test:button-push "Revert to Language Defaults")
        
        (when close-dialog?
          (fw:test:button-push "OK")
          (let ([new-frame (wait-for-new-frame language-dialog)])
            (unless (eq? new-frame drs-frame)
              (error 'set-language-level! 
                     "didn't get drracket frame back, got: ~s (drs-frame ~s)\n"
                     new-frame
                     drs-frame)))))))
  
  (provide/contract [check-language-level ((or/c string? regexp?) . -> . void?)])
  ;; checks that the language in the drracket window is set to the given one.
  ;; clears the definitions, clicks execute and checks the interactions window.
  (define (check-language-level lang-spec)
    (not-on-eventspace-handler-thread 'check-language-level!)
    (let* ([drs-frame (wait-for-drracket-frame)]
           [interactions (send drs-frame get-interactions-text)]
           [definitions-canvas (send drs-frame get-definitions-canvas)])
      (fw:test:new-window definitions-canvas)
      (fw:test:menu-select "Edit" "Select All")
      (fw:test:menu-select "Edit" "Delete")
      (do-execute drs-frame)
      (let ([lang-line (queue-callback/res
                        (λ ()
                          (send interactions get-text
                                (send interactions line-start-position 1)
                                (send interactions line-end-position 1))))])
        (unless (regexp-match lang-spec lang-line)
          (error 'check-language-level "expected ~s to match ~s"
                 lang-line lang-spec)))))
  
  
  (define (repl-in-edit-sequence?)
    (not-on-eventspace-handler-thread 'repl-in-edit-sequence?)
    (let ([drr (wait-for-drracket-frame)])
      (queue-callback/res
       (λ ()
         (send (send drr get-interactions-text) refresh-delayed?)))))
 
  ;; has-error? : frame -> (union #f string)
  ;; returns the text of an error in the interactions window of the frame or #f if there is none.
  ;; ensures that frame is front most.
  (define (has-error? frame)
    (not-on-eventspace-handler-thread 'repl-in-edit-sequence?)
    (run-one/sync
     (lambda ()
       (verify-drracket-frame-frontmost 'had-error? frame)
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
                               (error 'has-error? 
                                      "got back a bad result from position-paragraph: ~s ~s\n" 
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
       (not-on-eventspace-handler-thread 'fetch-output)
       (run-one/sync
        (lambda ()
          (verify-drracket-frame-frontmost 'fetch-output frame)
          (define-values (start end)
            (if (and _start _end)
                (values _start _end)
                (let* ([interactions-text (send frame get-interactions-text)]
                       [last-para (send interactions-text last-paragraph)])
                  (unless (>= last-para 2)
                    (error 'fetch-output 
                           "expected at least 2 paragraphs in interactions window, found ~a"
                           (+ last-para 1)))
                  (values (send interactions-text paragraph-start-position 2)
                          (send interactions-text paragraph-end-position
                                (- (send interactions-text last-paragraph) 1))))))
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
                     [;; this test is an approximation of
                      ;; (is-a? snip pict-snip%) from drracket/private/pict-snip%
                      (let ([sc (send snip get-snipclass)])
                        (and sc (regexp-match #rx"pict-snip.rkt" (send sc get-classname))))
                      (loop (send snip previous)
                            (cons "{pict-snip}" strings))]
                     
                     [else
                      (loop (send snip previous)
                            (cons (format "{unknown snip: ~e}\n" snip)
                                  strings))])])))))]))
  
  ;; run-one/sync : (-> A) -> A
  ;; runs the thunk `f' as a test action, and
  ;; waits for it to complete. Also propagates
  ;; exceptions.
  (define (run-one/sync f)
    (not-on-eventspace-handler-thread 'repl-in-edit-sequence?)
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
  
  (define (fire-up-drracket-and-run-tests
           #:use-focus-table? [use-focus-table? #t] 
           #:prefs [prefs '()]
           run-test)
    (on-eventspace-handler-thread 'fire-up-drracket-and-run-tests)
    (let ()
      (use-hash-for-prefs fw:preferences:low-level-get-preference
                          fw:preferences:low-level-put-preferences
                          fw:preferences:restore-defaults
                          fw:preferences:set
                          fw:preferences:default-set?
                          prefs)
      
      (parameterize ([current-command-line-arguments #()])
        (dynamic-require 'drracket #f))
      
      (fw:test:use-focus-table use-focus-table?)
      
      (thread (λ () 
                (let ([orig-display-handler (error-display-handler)])
                  (uncaught-exception-handler
                   (λ (x)
                     (if (exn? x)
                         (orig-display-handler (exn-message x) x)
                         (eprintf "uncaught exception ~s\n" x))
                     (exit 1))))
                (run-test)
                (exit)))
      (yield (make-semaphore 0))))
  
  ;; fire-up-separate-drracket-and-run-tests : (-> any) -> any
  ;; creates a separate custodian, eventspace, namespace, etc to
  ;; start up a new DrRacket. This has the advantage over fire-up-drracket-and-run-tests
  ;; that a single test suite can start up DrRacket multiple times, but it has the 
  ;; disadvantage that there is little sharing between the test suite implementation code and
  ;; DrRacket, so writing the testing code is more painful
  ;;
  ;; the only things shared are mred/mred (and its dependencies).
  (define (fire-up-separate-drracket-and-run-tests run-test)
    (define c (make-custodian))
    (define s (make-semaphore 0))
    (define orig-ns (current-namespace))
    (parameterize ([current-custodian c])
      (parameterize ([exit-handler (λ (v) 
                                     (semaphore-post s)
                                     (custodian-shutdown-all c))]
                     [current-namespace (make-empty-namespace)]
                     [current-command-line-arguments #()])
        (parameterize ([current-eventspace (make-eventspace)])
          (namespace-attach-module orig-ns 'mred/mred)
          
          ;; do this now so that dynamically requiring framework
          ;; exports during the call to 'run-test' is safe
          (namespace-require 'framework)
          
          (queue-callback
           (λ ()
             (use-hash-for-prefs (dynamic-require 'framework 'preferences:low-level-get-preference)
                                 (dynamic-require 'framework 'preferences:low-level-put-preferences)
                                 (dynamic-require 'framework 'preferences:restore-defaults)
                                 (dynamic-require 'framework 'preferences:set)
                                 (dynamic-require 'framework 'preferences:default-set?)
                                 '())
             (dynamic-require 'drracket #f)
             (thread (λ ()
                       (run-test)
                       (exit)))
             (yield (make-semaphore 0)))))))
    (semaphore-wait s))

  (define (use-hash-for-prefs preferences:low-level-get-preference 
                              preferences:low-level-put-preferences
                              preferences:restore-defaults
                              preferences:set
                              preferences:default-set?
                              prefs)
    ;; change the preferences system so that it doesn't write to 
    ;; a file; partly to avoid problems of concurrency in drdr
    ;; but also to make the test suite easier for everyone to run.
    (let ([prefs-table (make-hash)])
      (preferences:low-level-put-preferences
       (λ (names vals)
         (for ([name (in-list names)]
               [val (in-list vals)])
           (hash-set! prefs-table name val))))
      (preferences:low-level-get-preference 
       (λ (name [fail (lambda () #f)])
         (hash-ref prefs-table name fail)))
      
      ;; set all preferences to their defaults (some pref values may have
      ;; been read by this point, but hopefully that won't affect the
      ;; startup of drracket)
      (preferences:restore-defaults)
      
      ;; initialize some preferences to simulate these
      ;; being saved already in the user's prefs file 
      ;; call preferences:set too since the prefs file
      ;; may have been "read" already at this point
      (for ([pref (in-list prefs)])
        (define pref-key (list-ref pref 0))
        (define pref-val (list-ref pref 1))
        (define m (regexp-match #rx"^plt:framework-pref:(.*)$" (symbol->string pref-key)))
        (cond
          [m
           (hash-set! prefs-table pref-key pref-val)
           (define fw-pref-key (string->symbol (list-ref m 1)))
           (when (preferences:default-set? fw-pref-key)
             (preferences:set fw-pref-key pref-val))]
          [else
           ;; this currently doesn't happen, and it is easy to forget
           ;; that prefix, so print a message here to remind 
           (printf "WARNING: setting a preference that isn't set via the framework: ~s\n" 
                   pref-key)]))))
  
(define (not-on-eventspace-handler-thread fn)
  (when (eq? (current-thread) (eventspace-handler-thread (current-eventspace)))
    (error fn "expected to be run on some thread other than the eventspace handler thread")))

(define (on-eventspace-handler-thread fn)
  (unless (eq? (current-thread) (eventspace-handler-thread (current-eventspace)))
    (error fn "expected to be run on the eventspace handler thread")))
