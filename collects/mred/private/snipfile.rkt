(module snipfile racket/base
  (require racket/class
           racket/port
           syntax/moddep
           (prefix-in wx: "kernel.rkt")
           (prefix-in wx: racket/snip)
           "check.rkt"
           "editor.rkt")

  (provide open-input-text-editor
           open-input-graphical-file
           text-editor-load-handler
           open-output-text-editor )

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define empty-string (make-bytes 0))
  
  ;; open-input-text-editor : (instanceof text%) num num -> input-port
  ;; creates a user port whose input is taken from the text%,
  ;; starting at position `start-in'
  ;; and ending at position `end'.
  (define open-input-text-editor 
    (lambda (text [start 0] [end 'end] [snip-filter values] [port-name text] [expect-to-read-all? #f]
                  #:lock-while-reading? [lock-while-reading? #f])
      ;; Check arguments:
      (unless (text . is-a? . text%)
	(raise-type-error 'open-input-text-editor "text% object" text))
      (check-non-negative-integer 'open-input-text-editor start)
      (unless (or (eq? end 'end)
		  (and (integer? end) (exact? end) (not (negative? end))))
	(raise-type-error 'open-input-text-editor "non-negative exact integer or 'end" end))
      (let ([last (send text last-position)])
	(when (start . > . last)
	  (raise-mismatch-error 'open-input-text-editor
				(format "start index outside the range [0,~a]: " last)
				start))
	(unless (eq? end 'end)
	  (unless (<= start end last)
	    (raise-mismatch-error 'open-input-text-editor
				  (format "end index outside the range [~a,~a]: " start last)
				  end))))
      (let ([end (if (eq? end 'end) (send text last-position) end)]
	    [snip (send text find-snip start 'after-or-none)])
	;; If the region is small enough, and if the editor contains
	;; only string snips, then it's probably better to move
	;; all of the text into a string port:
	(if (or (not snip)
		(and (is-a? snip wx:string-snip%)
		     (let ([s (send text find-next-non-string-snip snip)])
		       (or (not s)
			   ((send text get-snip-position s) . >= . end)))))
	    (if (or expect-to-read-all?
		    ((- end start) . < . 4096))
		;; It's all text, and it's short enough: just read it into a string
		(open-input-string (send text get-text start end) port-name)
		;; It's all text, so the reading process is simple:
                (let ([start start])
                  (when lock-while-reading?
                    (send text begin-edit-sequence)
                    (send text lock #t))
                  (let-values ([(pipe-r pipe-w) (make-pipe)])
		    (make-input-port/read-to-peek
                     port-name
		     (lambda (s)
		       (let ([v (read-bytes-avail!* s pipe-r)])
			 (if (eq? v 0)
			     (let ([n (min 4096 (- end start))])
			       (if (zero? n)
				   (begin
                                     (close-output-port pipe-w)
				     (when lock-while-reading?
                                       (set! lock-while-reading? #f)
                                       (send text lock #f)
                                       (send text end-edit-sequence))
                                     eof)
				   (begin
				     (write-string (send text get-text start (+ start n)) pipe-w)
				     (set! start (+ start n))
				     (let ([ans (read-bytes-avail!* s pipe-r)])
                                       (when lock-while-reading?
                                         (when (eof-object? ans)
                                           (set! lock-while-reading? #f)
                                           (send text lock #f)
                                           (send text edit-edit-sequence)))
                                       ans))))
			     v)))
                     (lambda (s skip general-peek)
		       (let ([v (peek-bytes-avail!* s skip #f pipe-r)])
			 (if (eq? v 0)
			     (general-peek s skip)
			     v)))
		     void))))
	    ;; General case, which handles non-text context:
	    (with-method ([gsp (text get-snip-position)]
			  [grn (text get-revision-number)])
	      (let-values ([(pipe-r pipe-w) (make-pipe)])
		(let* ([get-text-generic (generic wx:snip% get-text)]
		       [get-count-generic (generic wx:snip% get-count)]
		       [next-generic (generic wx:snip% next)]
		       [revision (grn)]
		       [next? #f]
		       [update-str-to-snip
			(lambda (to-str)
			  (if snip
			      (let ([snip-start (gsp snip)])
				(cond
				 [(snip-start . >= . end)
				  (set! snip #f)
				  (set! next? #f)
				  0]
				 [(is-a? snip wx:string-snip%)
				  (set! next? #t)
				  (let ([c (min (send-generic snip get-count-generic) (- end snip-start))])
				    (write-string (send-generic snip get-text-generic 0 c) pipe-w)
				    (read-bytes-avail!* to-str pipe-r))]
				 [else
				  (set! next? #f)
				  0]))
			      (begin
				(set! next? #f)
				0)))]
		       [next-snip
			(lambda (to-str)
			  (unless (= revision (grn))
			    (raise-mismatch-error 
			     'text-input-port 
			     "editor has changed since port was opened: "
			     text))
			  (set! snip (send-generic snip next-generic))
			  (update-str-to-snip to-str))]
		       [read-chars (lambda (to-str)
				     (cond
				      [next?
				       (next-snip to-str)]
				      [snip
				       (let ([the-snip (snip-filter snip)])
					 (next-snip empty-string)
					 (lambda (file line col ppos)
					   (if (is-a? the-snip wx:snip%)
					       (if (is-a? the-snip wx:readable-snip<%>)
						   (send the-snip read-special file line col ppos)
						   (send the-snip copy))
					       the-snip)))]
				      [else eof]))]
		       [close (lambda () (void))]
		       [port (make-input-port/read-to-peek
			      port-name
			      (lambda (s)
                                (let* ([v (read-bytes-avail!* s pipe-r)]
                                       [res (if (eq? v 0) (read-chars s) v)])
                                  (when (eof-object? res)
                                    (when lock-while-reading? 
                                      (set! lock-while-reading? #f)
                                      (send text lock #f)
                                      (send text end-edit-sequence)))
                                  res))
                              (lambda (s skip general-peek)
				(let ([v (peek-bytes-avail!* s skip #f pipe-r)])
				  (if (eq? v 0)
				      (general-peek s skip)
				      v)))
			      close)])
		  (when lock-while-reading? 
                    (send text begin-edit-sequence)
                    (send text lock #t))
                  (if (is-a? snip wx:string-snip%)
		      ;; Special handling for initial snip string in
		      ;; case it starts too early:
		      (let* ([snip-start (gsp snip)]
			     [skip (- start snip-start)]
			     [c (min (- (send-generic snip get-count-generic) skip)
				     (- end snip-start))])
			(set! next? #t)
			(display (send-generic snip get-text-generic skip c) pipe-w))
		      (update-str-to-snip empty-string))
		  port)))))))

  (define (text-editor-load-handler filename expected-module)
    (unless (path? filename)
      (raise-type-error 'text-editor-load-handler "path" filename))
    (let-values ([(in-port src) (build-input-port filename)])
      (dynamic-wind
	  (lambda () (void))
	  (lambda ()
	    (parameterize ([read-accept-compiled #t]
                           [read-on-demand-source (and (load-on-demand-enabled)
                                                       (path->complete-path filename))])
	      (if expected-module
		  (with-module-reading-parameterization 
		   (lambda ()
		     (let* ([first (read-syntax src in-port)]
			    [module-ized-exp (check-module-form first expected-module filename)]
			    [second (read in-port)])
		       (unless (eof-object? second)
			 (raise-syntax-error
			  'text-editor-load-handler
			  (format "expected only a `module' declaration for `~s', but found an extra expression"
				  expected-module)
			  second))
		       (eval module-ized-exp))))
		  (let loop ([last-time-values (list (void))])
		    (let ([exp (read-syntax src in-port)])
		      (if (eof-object? exp)
			  (apply values last-time-values)
			  (call-with-values (lambda () (call-with-continuation-prompt
                                                        (lambda () (eval 
                                                                    (datum->syntax
                                                                     #f
                                                                     (cons '#%top-interaction exp)
                                                                     exp)))
                                                        (default-continuation-prompt-tag)
                                                        (lambda args
                                                          (apply
                                                           abort-current-continuation
                                                           (default-continuation-prompt-tag)
                                                           args))))
			    (lambda x (loop x)))))))))
	  (lambda ()
	    (close-input-port in-port)))))


  ;; build-input-port : string -> (values input any)
  ;; constructs an input port for the load handler. Also
  ;; returns a value representing the source of code read from the file.
  (define (build-input-port filename)
    (let ([p (open-input-file filename)])
      (port-count-lines! p)
      (let ([p (cond
		[(regexp-match-peek #rx#"^(?:#reader[(]lib\"read[.]ss\"\"wxme\"[)])?WXME01[0-9][0-9] ##[ \r\n]" p)
		 (let ([t (make-object text%)])
		   (send t insert-port p 'standard)
		   (close-input-port p)
		   (open-input-text-editor t 0 'end values filename))]
		[else p])])
	(port-count-lines! p) ; in case it's new
	(values p filename))))

  (define (open-input-graphical-file filename)
    (let-values ([(p name) (build-input-port filename)])
      p))

  (define open-output-text-editor 
    (lambda (text [start 'end] [special-filter values] [port-name text])
      (define pos (if (eq? start 'end)
		      (send text last-position)
		      (min start
			   (send text last-position))))
      (define-values (in out) (make-pipe))
      (define cvt (bytes-open-converter "UTF-8-permissive" "UTF-8"))
      (define raw-buffer (make-bytes 128))
      (define utf8-buffer (make-bytes 128))
      (define (show s)
	(send text insert s pos)
	(set! pos (+ (string-length s) pos)))
      (define (flush-text)
	(let ([cnt (peek-bytes-avail!* raw-buffer 0 #f in)])
	  (when (positive? cnt)
	    (let-values ([(got used status) (bytes-convert cvt raw-buffer 0 cnt utf8-buffer)])
	      (cond
	       [(positive? got) 
		(read-bytes-avail!* raw-buffer in 0 used)
		(show (bytes->string/utf-8 utf8-buffer #\? 0 got))
		(flush-text)]
	       [(eq? status 'error) 
		(read-byte in)
		(show "?")
		(flush-text)])))))
      (define (force-text)
	(when (byte-ready? in)
	  (show "?")
	  (read-byte in)
	  (flush-text)
	  (force-text)))
      (define port
	(make-output-port
	 text
	 always-evt
	 (lambda (s start end nonblock? breakable?)
	   ;; Put bytes into pipe:
	   (write-bytes s out start end)
	   ;; Extract as many string characters as are ready:
	   (flush-text)
	   (- end start))
	 (lambda ()
	   (force-text))
	 (lambda (special nonblock? breakable?)
	   (let ([special (special-filter special)])
	     (cond
	      [(special . is-a? . wx:snip%)
	       (force-text)
	       (send text insert special pos)
	       (set! pos (+ pos (send special get-count)))]
	      [else
	       (display special port)]))
	   #t)
	 #f #f
	 (lambda ()
	   (let ([line (send text position-line pos)])
	     (values (add1 line)
		     (- pos (send text line-start-position line))
		     (add1 pos))))
	 void
	 (add1 pos)))
      port)))
