(module sendurl mzscheme
  (require (lib "process.ss")
           (lib "file.ss")
           (lib "etc.ss")
           (lib "sendevent.ss"))
  
  (provide send-url unix-browser-list browser-preference? external-browser)
  
  (define separate-by-default?
    (get-preference 'new-browser-for-urls (lambda () #t)))

  ; : any -> bool
  (define (browser-preference? x)
    (or (not x) (memq x unix-browser-list) (custom-browser? x)
        (procedure? x)))

  (define external-browser
    (make-parameter
     (get-preference 'external-browser (lambda () #f))
     (lambda (x)
       (if (browser-preference? x)
           x
           (error 'external-browser "~a is not a valid browser preference" x)))))
  
  ; send-url : str [bool] -> void
  (define send-url
    (opt-lambda (url-str [separate-window? separate-by-default?])
      ; The with-handler reverts to the old error port before printing raised error messages.
      (with-handlers ([void (lambda (exn) (raise exn))])
        (parameterize ([current-input-port null-input]
                       [current-error-port null-output]
                       [current-output-port null-output])
          (cond
            [(procedure? (external-browser))
             ((external-browser) url-str)]
            [(eq? (system-type) 'macos)
             (if (regexp-match "Blue Box" (system-type #t))
                 ;; Classic inside OS X:
                 (let loop ([l '("MSIE" "NAVG")])
                   (if (null? l)
                       (error 'send-url "couldn't start Internet Explorer or Netscape")
                       (with-handlers ([not-break-exn? (lambda (x) (loop (cdr l)))])
                         (subprocess #f #f #f "by-id" (car l))
                         (let loop ([retries 2]) ;; <<< Yuck <<<
                           (if (zero? retries)
                               (error "enough already") ; caught above
                               (with-handlers ([not-break-exn? (lambda (x)
                                                                 (loop (sub1 retries)))])
                                 (let ([t (thread (lambda ()
                                                    (send-event (car l) "GURL" "GURL" url-str)))])
                                   (object-wait-multiple 1 t) ;; <<< Yuck (timeout) <<<
                                   (when (thread-running? t)
                                     (kill-thread t)
                                     (error "timeout")))))))))
                 ;; Normal OS Classic:
                 (send-event "MACS" "GURL" "GURL" url-str))]
            [(or (eq? (system-type) 'macosx)
                 (equal? "ppc-macosxonx" (system-library-subpath)))
             
             ; not sure what changed, but this is wrong now.... -robby
             ;(system (format "osascript -e 'open location \"~a\"'" (regexp-replace* "%" url-str "%25")))
             
             (system (format "osascript -e 'open location \"~a\"'" url-str))]
            [(eq? (system-type) 'windows)
             (shell-execute #f url-str "" (current-directory) 'SW_SHOWNORMAL)]
            [(eq? (system-type) 'unix)
             (let ([preferred (external-browser)])
               (cond
                 [(use-browser 'opera preferred)
                  =>
                  (lambda (browser-path) 
                    ;; opera may not return -- always open asyncronously
                    ;; opera starts a new browser automatically, if it can't find one
                    (process*/close-ports browser-path "-remote"
                                          (format "openURL(~a)"
                                                  (if separate-window?
                                                      (format "~a,new-window" url-str)
                                                      url-str))))]
                 [(use-browser 'galeon preferred)
                  =>
                  (lambda (browser-path)
                    (process*/close-ports browser-path
                                          (if separate-window? "-w" "-x")
                                          url-str))]
                 [(or (use-browser 'netscape preferred)
                      (use-browser 'mozilla preferred))
                  =>
                  (lambda (browser-path)
                    ;; netscape's -remote returns with an error code, if no
                    ;; netscape is around. start a new netscape in that case.
                    (or (system* browser-path "-remote"
                                 (format "openURL(~a)"
                                         (if separate-window?
                                             (format "~a,new-window" url-str)
                                             url-str)))
                        (process*/close-ports browser-path url-str)))]
                 [(use-browser 'dillo preferred)
                  =>
                  (lambda (browser-path)
                    (process*/close-ports browser-path url-str))]
                 [(custom-browser? preferred)
		  (let ([cmd (parse-bash-command
			      (string-append (car preferred)
					     url-str
					     (cdr preferred)))])
		    (if (null? cmd)
			(error 'send-url "no custom browser selected")
			(let ([prog (find-executable-path (car cmd) #f)])
			  (if prog
			      (apply process*/close-ports prog (cdr cmd))
			      (error 'send-url "cannot find custom browser ~e" (car cmd))))))]
                 [else
                  (error 'send-url "Couldn't find ~a to open URL: ~e" (orify unix-browser-list) url-str)]))]
            [else (error 'send-url "don't know how to open URL on platform: ~s" (system-type))])))))
  
  ; : tst -> bool
  (define (custom-browser? x)
    (and (pair? x) (string? (car x)) (string? (cdr x))))
  
  (define unix-browser-list '(opera galeon netscape mozilla dillo))
  
  ; : (cons tst (listof tst)) -> str
  (define (orify l)
    (cond
      [(null? (cdr l)) (format "~a" (car l))]
      [(null? (cddr l)) (format "~a or ~a" (car l) (cadr l))]
      [else 
    (let loop ([l l])
      (cond
        [(null? (cdr l)) (format "or ~a" (car l))]
        [else (string-append (format "~a, " (car l)) (loop (cdr l)))]))]))
  
  ; : sym sym -> (U #f str)
  ; to find the path for the named browser, unless another browser is preferred
  (define (use-browser browser-name preferred)
    (and (or (not preferred)
	     (eq? preferred browser-name))
	 (find-executable-path (symbol->string browser-name) #f)))
  
  ; null-input : iport
  (define null-input
    (make-custom-input-port #f
			    (lambda (s) eof)
			    #f
			    void))
  
  ; null-output : oport
  (define null-output
    (make-custom-output-port #f (lambda (s start end flush?) (- end start)) void void))
  
  (define dev-null "/dev/null") ; unix specific, so slashes are okay
  
  ; process*/close-ports : [arg]* -> void
  ; This is a funny hack.  Closing unused scheme pipe ports right away closes
  ; the ports the browser is using, resulting in errors if they are read from or written
  ; to.  However, closing file ports (i.e. to and from /dev/null) after the subprocess is
  ; spawned does _not_ close the browser's ports since they are copied when the subprocess
  ; loads.  Blech.
  ; All this is necessary because system administrators replace netscape with a funny
  ; perl/zsh/whatever script that prints out extra crud or does weird, system dependent
  ; setup stuff before launching the original browser executable.
  (define (process*/close-ports . args)
    (if (and (memq (system-type) '(unix macosx))
             ; we can't check for _the_ dev-null, so check what we can
             (file-exists? dev-null)
             (let ([perms (file-or-directory-permissions dev-null)])
               (and (memq 'read perms)
                    (memq 'write perms)))
             (zero? (file-size dev-null)))
        (let ([out (open-output-file dev-null 'append)]
              [in (open-input-file dev-null)]
              [err (open-output-file dev-null 'append)])
          (let-values ([(false-out false-in id false-err status) 
                        (apply values (apply process*/ports out in err args))])
            (close-output-port out)
            (close-input-port in)
            (close-output-port err)))
        (let-values ([(out in id err status) 
                      (apply values (apply process*/ports #f #f #f args))])
          (close-input-port out)
          (close-output-port in)
          (close-input-port err))))
  
  ; parse-command : iport -> (listof str)
  (define (parse-command.com in)
    (let parse ()
      (cond
        [(eof-object? (skip-space-tab in)) null]
        [else (cons (list->string (parse-one #t in)) (parse))])))
  
  ; parse-one : bool iport -> (listof char)
  (define (parse-one unquoted in)
    (let parse-1 ([unquoted unquoted])
      (let ([c (read-char in)])
        (cond
          [(eof-object? c) null]
          [(eq? c #\\) (parse-backslashes 1 unquoted in)]
          [(eq? c #\") (parse-1 (not unquoted))]
          [(and unquoted (or (eq? #\space c) (eq? #\tab c))) null]
          [else (cons c (parse-1 unquoted))]))))
  
  ; parse-backslashes : nat bool iport -> (listof char)
  (define (parse-backslashes n unquoted in)
    (let more ([n n])
      (let ([c (read-char in)])
        (cond
          [(eq? c #\\) (more (add1 n))]
          [(eq? c #\")
           (if (even? n)
               (cons-n-backslashes (/ n 2) (parse-one (not unquoted) in))
               (cons-n-backslashes (/ (sub1 n) 2) (cons #\" (parse-one unquoted in))))]
          [(and unquoted (or (eq? #\space c) (eq? #\tab c))) null]
          [else (cons-n-backslashes n (cons c (parse-one unquoted in)))]))))
  
  ; cons-n-backslashes : nat (listof char) -> (listof char)
  (define (cons-n-backslashes n l)
    (cond
      [(zero? n) l]
      [else (cons-n-backslashes (sub1 n) (cons #\\ l))]))
  
  ; skip-space-tab : iport -> (U char eof)
  ; to skip spaces and tabs
  (define (skip-space-tab in)
    (let loop ()
      (let ([c (peek-char in)])
        (cond
          [(or (eq? #\space c) (eq? #\tab c))
           (read-char in)
           (loop)]
          [else c]))))
  
  ; test-parse-command.com : -> bool
  ; all but one of these tests is taken from MS's documentation
  ; http://www.cygwin.com/ml/cygwin/1999-08/msg00701.html
  (define (test-parse-command.com)
    (and (equal? (parse-command.com (open-input-string "\"a b c\" d e"))
                 '("a b c" "d" "e"))
         (equal? (parse-command.com (open-input-string "\"ab\\\"c\" \"\\\\\" d"))
                 '("ab\"c" "\\" "d"))
         (equal? (parse-command.com (open-input-string "a\\\\\\b d\"e f\"g h"))
                 '("a\\\\\\b" "de fg" "h"))
         (equal? (parse-command.com (open-input-string "a\\\"b c d"))
                 '("a\"b" "c" "d"))
         (equal? (parse-command.com (open-input-string "a\\\\\\\"b c d"))
                 '("a\\\"b" "c" "d"))
         (equal? (parse-command.com (open-input-string "a\\\\\\\\\"b c\" d e"))
                 '("a\\\\b c" "d" "e")))) ; : str -> (listof str)
  ; to split a string into an argument list similar to how bash does.
  ; It does not handle backquotes because it does not evaluate bash commands.
  ; It does not expand shell variables, nor does it expand tilda paths.
  (define (parse-bash-command line)
    (let ([in (open-input-string line)])
      (let loop ()
        (let ([w (parse-word in)])
          (if (zero? (string-length w))
              null
              (cons w (loop)))))))
  
  ; : iport -> str
  ; backslashes are literal inside ''s, almost literal inside ""s, and always escape the next character when unquoted
  (define (parse-word in)
    (skip-space-tab in)
    (list->string
     (let parse-word ()
       (let ([c (read-char in)])
         (cond
           [(eof-object? c) null]
           [else
            (case c
              [(#\')
               (let until-quote ()
                 (let ([c (read-char in)])
                   (cond
                     [(eof-object? c) (error 'parse-word "missing close '")]
                     [else (case c
                             [(#\') (parse-word)]
                             [else (cons c (until-quote))])])))]
              [(#\")
               (let until-double-quote ()
                 (let ([c (read-char in)])
                   (cond
                     [(eof-object? c) (error 'parse-word "missing close \"")]
                     [else (case c
                             [(#\") (parse-word)]
                             [(#\\)
                              (let ([escaped-char (read-char in)])
                                (cond
                                  [(eof-object? escaped-char)
                                   (error 'parse-word "backslash at end of string")]
                                  [else (cons escaped-char (until-double-quote))]))]
                             [else (cons c (until-double-quote))])])))]
              [(#\\)
               (let ([escaped-char (read-char in)])
                 (cond
                   [(eof-object? escaped-char)
                    (error 'parse-word "backslash at end of string")]
                   [else (cons escaped-char (parse-word))]))]
              [(#\space #\tab) null]
              [else (cons c (parse-word))])])))))
  
  ;  > (parse-word (open-input-string "a'b\\c\"d'ef\" g\\h\"\\i jkl"))
  ; (#\a #\b #\\ #\c #\" #\d #\e #\f #\space #\g #\h #\i)
  )
