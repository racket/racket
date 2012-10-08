(module repl racket/base 
  (require racket/class
           (prefix-in wx: "kernel.rkt")
	   (prefix-in wx: racket/snip/private/style)
           "editor.rkt"
           "app.rkt"
           "mrtop.rkt"
           "mrcanvas.rkt"
           "mrmenu.rkt"
           "filedialog.rkt")

  (provide graphical-read-eval-print-loop
           textual-read-eval-print-loop)

  (define (-graphical-read-eval-print-loop user-esp override-ports?)
    ;; The REPL buffer class
    (define esq:text%
      (class text%
	(inherit insert last-position get-text erase change-style clear-undos set-max-undo-history)
	(rename-super [super-on-char on-char])
	(define prompt-pos 0)
        (define locked? #f)
	(augment*
	 [can-insert? (lambda (start end) (and (>= start prompt-pos) (not locked?)))]
	 [can-delete? (lambda (start end) (and (>= start prompt-pos) (not locked?)))])
	(override*
         [on-char (lambda (c)
                    (super-on-char c)
                    (when (and (memq (send c get-key-code) '(#\return #\newline #\003))
                               (not locked?))
                      (set! locked? #t)
                      (evaluate (get-text prompt-pos (last-position)))))])
	(public*
         [new-prompt (lambda ()
                       (output "> ")
                       (set! prompt-pos (last-position))
                       (set! locked? #f)
                       (clear-undos))]
         [output (lambda (str)
                   (let ([l? locked?])
                     (set! locked? #f)
                     (insert str)
                     (set! locked? l?)))]
         [reset (lambda ()
                  (set! locked? #f)
                  (set! prompt-pos 0)
                  (erase)
                  (new-prompt))])
        (super-new)
        (let ([s (last-position)]
              [m (regexp-match #rx"^(.*), (Copyright.*)$" (banner))])
          (insert (if m
                      (format "~a." (cadr m))
                      (let ([b (banner)])
                        (substring b 0 (sub1 (string-length b))))))
          (let ([e (last-position)])
            (insert #\newline)
            (change-style (send (make-object wx:style-delta% 'change-bold) set-delta-foreground "BLUE") s e))
          (when m
            (output (caddr m))))
        (insert "This is a simple window for evaluating Racket expressions.") (insert #\newline)
        (let ([s (last-position)])
          (insert "Quit now and run DrRacket to get a better window.")
          (let ([e (last-position)])
            (insert #\newline)
            (change-style
             (send (make-object wx:style-delta% 'change-italic) set-delta-foreground "RED")
             s e)))
        (insert "The current input port always returns eof.") (insert #\newline)
        (new-prompt)
        (set-max-undo-history 'forever)))

    ;; GUI creation
    (define frame (make-object (class frame%
                                 (init-rest args)
				 (inherit accept-drop-files)
				 (augment*
				  [on-close (lambda () 
					      (custodian-shutdown-all user-custodian)
					      (semaphore-post waiting))])
				 (override*
                                  [on-drop-file (lambda (f) (evaluate (format "(load ~s)" (path->string f))))])
                                 (apply super-make-object args) (accept-drop-files #t))
			       "GRacket REPL" #f 500 400))
    (define repl-buffer (make-object esq:text%))
    (define repl-display-canvas (new editor-canvas% [parent frame] [style '(no-border auto-hscroll resize-corner)]))

    (define esq-eventspace (wx:current-eventspace))
    (define (queue-output proc)
      (parameterize ((wx:current-eventspace esq-eventspace))
	(wx:queue-callback proc #f)))
    
    ;; User space initialization
    (define user-custodian (make-custodian))
    
    (define user-output-port
      (let ([leftover #""]
	    [cvt (bytes-open-converter "UTF-8-permissive" "UTF-8")])
	(make-output-port
	 'console
	 always-evt
	 (lambda (s start end flush? breakable?) 
	   (queue-output (lambda () 
			   ;; s might end in the middle of a UTF-8 encoding.
			   ;;  Get a complete prefix, and save the rest.
			   (let ([s (bytes-append leftover (subbytes s start end))])
			     (let-values ([(res used status) (bytes-convert cvt s)])
			       (send repl-buffer output (bytes->string/utf-8 res))
			       (set! leftover (subbytes s used))))))
	   (- end start))
	 void))) ; no close action
    
    (define user-eventspace
      (or user-esp
	  (parameterize ((current-custodian user-custodian))
	    (wx:make-eventspace))))

    ;; Evaluation
    
    (define (evaluate expr-str)
      (parameterize ((wx:current-eventspace user-eventspace))
	(wx:queue-callback
	 (lambda ()
	   (dynamic-wind
	       void
	       (lambda () 
		 (call-with-values
		     (lambda () (call-with-continuation-prompt
                                 (lambda () (eval (cons
                                                   '#%top-interaction
                                                   (read (open-input-string expr-str)))))))
		   (lambda results
		     (for-each 
		      (lambda (v) 
			(parameterize ([current-output-port user-output-port])
			  (print v) 
			  (newline)))
		      results))))
	       (lambda ()
		 (queue-output (lambda () (send repl-buffer new-prompt)))))))))

    (define waiting (make-semaphore 0))

    (let ([mb (make-object menu-bar% frame)])
      (let ([m (make-object menu% "&File" mb)])
	(make-object menu-item% "Load File..." m (lambda (i e) (let ([f (get-file #f frame)]) 
								 (and f 
								      (evaluate (format "(load ~s)" (path->string f)))))))
	(unless (current-eventspace-has-standard-menus?)
	  (make-object menu-item% 
		       (if (eq? (system-type) 'windows)
			   "E&xit"
			   "&Quit")
		       m (lambda (i e) (send frame on-close) (send frame show #f)) #\q)))
      (let ([m (make-object menu% "&Edit" mb)])
	(append-editor-operation-menu-items m #f)))

    ;; Just a few extra key bindings:
    ((current-text-keymap-initializer) (send repl-buffer get-keymap))
    (send repl-buffer auto-wrap #t)

    ;; Go
    (when override-ports?
      (parameterize ((wx:current-eventspace user-eventspace))
	(wx:queue-callback
	 (lambda ()
	   (current-output-port user-output-port)
	   (current-error-port user-output-port)
	   (current-input-port (open-input-bytes #"")))
	 #t)))

    (send repl-display-canvas set-editor repl-buffer)

    (send frame show #t)
    
    (send repl-display-canvas focus)

    (wx:yield waiting))

  (define graphical-read-eval-print-loop
    (case-lambda
     [() (-graphical-read-eval-print-loop #f #t)]
     [(esp)
      (graphical-read-eval-print-loop esp (not esp))]
     [(esp override-ports?)
      (unless (or (not esp) (wx:eventspace? esp))
	(raise-argument-error 'graphical-read-eval-print-loop "(or/c eventspace? #f)" esp))
      (-graphical-read-eval-print-loop esp override-ports?)]))

  (define (textual-read-eval-print-loop)
    (define user-custodian (make-custodian))
    (define user-eventspace
      (parameterize ((current-custodian user-custodian))
        (wx:make-eventspace)))
    (define ready-sema (make-semaphore))
    (define (evaluate expr)
      (parameterize ((wx:current-eventspace user-eventspace))
	(wx:queue-callback
	 (lambda ()
	   (dynamic-wind
	       void
	       (lambda () 
		 (call-with-values
		     (lambda () (call-with-continuation-prompt
                                 (lambda () (eval (cons
                                                   '#%top-interaction
                                                   expr)))))
		   (lambda results
		     (for-each 
		      (lambda (v) 
                        ((current-print) v))
		      results))))
	       (lambda ()
                 (semaphore-post ready-sema)))))))
    (parameterize-break 
     #f
     (let loop ()
       (let ([e (let read-loop ()
                  (call-with-continuation-prompt
                   ;; Enable break during reading:
                   (lambda ()
                     (parameterize-break 
                      #t
                      ((current-prompt-read))))
                   (default-continuation-prompt-tag)
                   (lambda args (read-loop))))])
         (unless (eof-object? e)
           (evaluate e)
           ;; While waiting, redirect breaks:
           (call-with-exception-handler
            (lambda (exn)
              (if (exn:break? exn)
                  (begin
                    (break-thread (eventspace-handler-thread user-eventspace))
                    ((exn:break-continuation exn) (void)))
                  exn))
            (lambda ()
              (parameterize-break
               #t
               (semaphore-wait ready-sema))))
           (loop)))))))
