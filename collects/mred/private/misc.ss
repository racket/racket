(module misc mzscheme
  (require (lib "class.ss")
	   (lib "file.ss")
	   (lib "process.ss")
	   "wxkernel.ss")

  (provide file-creator-and-type
	   hide-cursor-until-moved
	   sleep/yield
	   play-sound
	   timer%)

  ;; Currently only used for PS print and preview
  (wx:set-executer
   (let ([orig-err (current-error-port)])
     (lambda (prog . args)
       (let ([cmd (string-append
		   prog 
		   (let loop ([args args])
		     (if (null? args)
			 ""
			 (format " ~s~a" (car args) (loop (cdr args))))))])
	 (let-values ([(in out pid err x) (apply values (process cmd))])
	   (close-output-port out)
	   (let ([echo (lambda (p)
			 (thread (lambda ()
				   (dynamic-wind
				       void
				       (lambda ()
					 (let loop ()
					   (let ([l (read-line p)])
					     (unless (eof-object? l)
					       (fprintf orig-err "~a~n" l)
					       (loop)))))
				       (lambda () (close-input-port p))))))])
	     (echo in)
	     (echo err)
	     (void)))))))

  (define (sleep/yield secs)
    (unless (and (real? secs) (not (negative? secs)))
      (raise-type-error 'sleep/yield "non-negative real number" secs))
    (wx:yield (alarm-evt (+ (current-inexact-milliseconds)
			    (* secs 1000))))
    (void))

  (define file-creator-and-type
    (case-lambda
     [(fn) (wx:file-creator-and-type fn)]
     [(fn c t) (wx:file-creator-and-type fn c t)]))
  
  (define (hide-cursor-until-moved) 
    (wx:hide-cursor))

  (define (play-sound f async?)
    (unless (or (path? f) (string? f))
      (raise-type-error 'play-sound "string-or-path" f))
    (unless (file-exists? f)
      (error 'play-sound "file not found: ~e" f))
    (if (not (eq? (system-type) 'unix))
      (wx:play-sound f async?)
      (let* (;; check user-set preference first
             [cmd (get-preference '|MrEd:playcmd| (lambda () #f))]
             [cmd (if (string? cmd)
                    cmd
                    (let ([subpath (path->string (system-library-subpath))])
                      (cond [;; use play interface to sox
                             (regexp-match #rx"linux" subpath) "play ~a"]
                            [(regexp-match #rx"solaris" subpath) "audioplay ~a"]
                            [else (raise-mismatch-error
                                   'play-sound
                                   "not supported by default on this platform"
                                   subpath)])))])
        ((if async?
           (lambda (x)
             (process/ports
              (current-output-port) (current-input-port) (current-error-port) x)
             #t)
           system)
         (format cmd (string-append
                      "\""
                      (regexp-replace* #rx"([$\"\\])"
                                       (path->string (expand-path f))
                                       "\\\\\\1")
                      "\""))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Timers:

  (define timer%
    (class wx:timer%
      (init [notify-callback void]
	    [interval #f]
	    [just-once? #f])
      
      (inherit start)
      
      (define -notify-callback notify-callback)
      
      (define/override (notify) (-notify-callback))
      
      (super-make-object)

      (when interval
	(start interval just-once?)))))
