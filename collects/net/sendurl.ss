(module sendurl mzscheme
  (require (lib "process.ss")
           (lib "file.ss")
           (lib "kw.ss")
           (lib "port.ss")
           (lib "sendevent.ss"))

  (provide send-url unix-browser-list browser-preference? external-browser)

  (define separate-by-default?
    (get-preference 'new-browser-for-urls (lambda () #t)))

  ; : any -> bool
  (define (custom-browser? x)
    (and (pair? x) (string? (car x)) (string? (cdr x))))

  ; : any -> bool
  (define (browser-preference? x)
    (or (not x) (eq? 'plt x) (memq x unix-browser-list) (custom-browser? x)
        (procedure? x)))

  (define external-browser
    (make-parameter
     #f ; #f means "consult the preferences file"
     (lambda (x)
       (if (browser-preference? x)
         x
         (error 'external-browser "~a is not a valid browser preference" x)))))

  (define osx-browser?
    (delay (or (eq? (system-type) 'macosx)
               (equal? "ppc-darwin" (path->string (system-library-subpath))))))

  ; send-url : str [bool] -> void
  (define/kw (send-url url-str
                       #:optional [separate-window? separate-by-default?])
    (define external (external-browser))
    (define stype (system-type))
    (cond
      [(procedure? external) (external url-str)]
      [(force osx-browser?)
       (browser-process (format "osascript -e 'open location \"~a\"'" url-str))]
      [(eq? stype 'windows)
       (shell-execute #f url-str "" (current-directory) 'SW_SHOWNORMAL)]
      [(eq? stype 'unix)
       (let ([preferred (or external (get-preference 'external-browser))])
         (cond
           [(use-browser 'opera preferred)
            =>
            (lambda (exe)
              ;; opera may not return -- always open asyncronously
              ;; opera starts a new browser automatically, if it can't find one
              (browser-process* exe "-remote"
                                (format "openURL(~a)"
                                        (if separate-window?
                                          (format "~a,new-window" url-str)
                                          url-str))))]
           [(use-browser 'galeon preferred)
            =>
            (lambda (exe)
              (browser-process* exe (if separate-window? "-w" "-x") url-str))]
           [(or (use-browser 'netscape preferred)
                (use-browser 'mozilla preferred))
            =>
            (lambda (exe)
              ;; netscape's -remote returns with an error code, if no
              ;; netscape is around. start a new netscape in that case.
              (or (system* exe "-remote"
                           (format "openURL(~a)"
                                   (if separate-window?
                                     (format "~a,new-window" url-str)
                                     url-str)))
                  (browser-process* exe url-str)))]
           [(use-browser 'dillo preferred)
            =>
            (lambda (exe) (browser-process* exe url-str))]
           [(custom-browser? preferred)
            (let ([cmd (string-append (car preferred)
                                      url-str
                                      (cdr preferred))])
              (browser-process cmd))]
           [else
            (error 'send-url "Couldn't find ~a to open URL: ~e"
                   (orify unix-browser-list) url-str)]))]
      [else (error 'send-url
                   "don't know how to open URL on platform: ~s" stype)]))

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

  ;; run-browser : process-proc list-of-strings -> void
  (define (run-browser process*/ports args)
    (let-values ([(stdout stdin pid stderr control)
                  (apply values
                         (apply process*/ports
                                (open-output-nowhere) #f (current-error-port)
                                args))])
      (close-output-port stdin)
      (thread (lambda ()
                (control 'wait)
                (when (eq? 'done-error (control 'status))
                  (error 'run-browser "process execute failed: ~e" args))))
      (void)))

  (define (browser-process* . args)
    (run-browser process*/ports args))

  (define (browser-process . args)
    (run-browser process/ports args)))
