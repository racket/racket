(module sendurl mzscheme
  (require (lib "process.ss")
	   (lib "file.ss")
	   (lib "etc.ss")
	   (lib "sendevent.ss"))
  
  (provide send-url)
  
  (define separate-by-default?
    (get-preference 'new-browser-for-urls (lambda () #t)))
  
  ; send-url : str -> void
  (define send-url
    (opt-lambda (str [separate-window? separate-by-default?])
      (parameterize ([current-input-port null-input]
		     [current-error-port null-output] ; comment out this line to see error messages
		     [current-output-port null-output])
	(cond
	  [(eq? (system-type) 'macos)
	   ;; actually, I think GURL means something slightly different...
	   (send-event "MACS" "GURL" "GURL" str)]
	  [(or (eq? (system-type) 'macosx)
	       (equal? "ppc-macosxonx" (system-library-subpath)))
	   (system (format "osascript -e 'open location \"~a\"'" str))]
	  [(eq? (system-type) 'windows)
	   (shell-execute #f str "" (current-directory) 'SW_SHOWNORMAL)]
	  [(eq? (system-type) 'unix)
	   (let ([preferred (get-preference 'external-browser (lambda () #f))])
	     (cond
	       [(and (or (not preferred)
			 (eq? preferred 'opera))
		     (find-executable-path "opera" #f))
		=>
		(lambda (browser-path) 
		  ;; opera may not return -- always open asyncronously
		  ;; opera starts a new browser automatically, if it can't find one
		  (process*/close-ports browser-path "-remote"
					(format "openURL(~a)"
						(if separate-window?
						    (format "~a,new-window" str)
						    str))))]
	       [(and (and (or (not preferred)
			      (eq? preferred 'netscape)))
		     (find-executable-path "netscape" #f))
		=>
		(lambda (browser-path)
		  ;; netscape's -remote returns with an error code, if no
		  ;; netscape is around. start a new netscape in that case.
		  (or (system* browser-path "-remote"
			       (format "openURL(~a)"
				       (if separate-window?
					   (format "~a,new-window" str)
					   str)))
		      (process*/close-ports browser-path str)))]
               [(and (and (or (not preferred)
			      (eq? preferred 'dillo)))
		     (find-executable-path "dillo" #f))
		=>
		(lambda (browser-path)
		  (process*/close-ports browser-path str))]
	       [else
		(error 'open-url "Couldn't find Opera, Netscape, or Dillo to open URL: ~e" str)]))]
	  [else (error 'send-url "don't know how to open URL on platform: ~s" (system-type))]))))
  
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
                 '("a\\\\b c" "d" "e")))))
