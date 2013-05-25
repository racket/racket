(module misc mzscheme
  (require mzlib/class
           mzlib/file
           mzlib/process
           (prefix wx: "kernel.rkt")
           racket/snip/private/prefs)

  (provide file-creator-and-type
           hide-cursor-until-moved
           sleep/yield
           play-sound
           timer%)

  ;; Formerly used for PS print and preview:
  #;
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
					       (fprintf orig-err "~a\n" l)
					       (loop)))))
				       (lambda () (close-input-port p))))))])
	     (echo in)
	     (echo err)
	     (void)))))))

  (define (sleep/yield secs)
    (unless (and (real? secs) (not (negative? secs)))
      (raise-argument-error 'sleep/yield "(>=/c 0.0)" secs))
    (let ([evt (alarm-evt (+ (current-inexact-milliseconds)
                             (* secs 1000)))])
      ;; First, allow at least some events to be handled even if 
      ;; the alarm is immediately ready. This makes `sleep/yield'
      ;; more like `sleep':
      (wx:yield) 
      ;; Now, really sleep:
      (wx:yield evt))
    (void))

  (define file-creator-and-type
    (case-lambda
     [(fn) (wx:file-creator-and-type fn)]
     [(fn c t) (wx:file-creator-and-type fn c t)]))

  (define (hide-cursor-until-moved)
    (wx:hide-cursor))

  (define unix-play-command
    (delay
      (let* (;; check user-set preference first
             ;;  (can be a string with `~a', or a name of an executable)
             [cmd (get-preference* '|GRacket:playcmd| (lambda () #f))]
             [cmd (cond [(not (string? cmd)) #f]
                        [(regexp-match? #rx"~[aA]" cmd) cmd]
                        [(find-executable-path cmd) => values]
                        ;; maybe there are some redundant spaces?
                        [(regexp-match #rx"^ *([^ ].*?) *$" cmd)
                         => (lambda (m) (find-executable-path (cadr m)))]
                        ;; bad setting: no ~a, and does not name an executable
                        [else #f])]
             ;; no setting => search for some known commands
             [cmd (or cmd
                      (ormap find-executable-path
                             '("aplay" "play" "esdplay" "sndfile-play"
                               "audioplay"))
                      (error 'play-sound
                             "not supported on this machine ~a"
                             "(no default, and no known command found)"))]
             [>null (open-output-file "/dev/null" 'append)]
             [<null (open-input-file "/dev/null")]
             [bufsize 500]) ; maximum number of chars from stderr that we show
        (lambda (f async?)
          (define file (path->string (expand-path f)))
          ;; if find-executable-path was used, then cmd is a path, otherwise
          ;; it's a string with `~a'.
          (define cmd+args
            (if (path? cmd)
              (list cmd file)
              (list "/bin/sh" "-c"
                    (format cmd (string-append "\""
                                               (regexp-replace*
                                                #rx"([$\"\\])" file "\\\\\\1")
                                               "\"")))))
          (define-values (p pout pin perr)
            (apply subprocess >null <null #f cmd+args))
          (define buf (make-bytes bufsize))
          (define (follow)
            ;; buf holds the tail (`bufsize' chars) of the error output
            (let loop ([i 0] [full? #f])
              (let ([n (read-bytes! buf perr i)])
                (cond
                  [(eof-object? n)
                   (let ([c (begin (subprocess-wait p) (subprocess-status p))])
                     (if (zero? c)
                       #t ; success => don't show error output
                       (let ([err (current-error-port)])
                         (cond [full?
                                (display "...snip...\n")
                                (write-bytes buf err i)
                                (write-bytes buf err 0 i)]
                               [(> i 0)
                                (write-bytes buf err 0 i)])
                         (unless async? ; no point in an async error
                           (error 'play-sound
                                  "running ~a returned an error code"
                                  cmd+args)))))]
                  [(= (+ n i) bufsize) (loop 0 #t)]
                  [else (loop (+ n i) full?)]))))
          (if async?
            (begin (thread follow) #t)
            (follow))))))

  (define (play-sound f async?)
    (unless (path-string? f)
      (raise-argument-error 'play-sound "path-string?" f))
    (unless (file-exists? f)
      (error 'play-sound "file not found: ~e" f))
    ((if (eq? (system-type) 'unix) (force unix-play-command) wx:play-sound)
     f async?))

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
