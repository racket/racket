;; The main client of this module is browser/external.ss
;;   (others just use the (send-url url [new?]) interface.)

#lang scheme/base

(require scheme/system
         scheme/file
         scheme/promise
         scheme/port)

(provide send-url unix-browser-list browser-preference? external-browser)

(define separate-by-default?
  (get-preference 'new-browser-for-urls (lambda () #t)))

(define unix-browser-list
  '(gnome-open firefox galeon opera netscape mozilla dillo))

;; : any -> bool
(define (custom-browser? x)
  (and (pair? x) (string? (car x)) (string? (cdr x))))

;; : any -> bool
(define (browser-preference? x)
  (or (not x) (memq x unix-browser-list) (custom-browser? x)
      (procedure? x)))

(define external-browser
  (make-parameter
   #f ; #f means "consult the preferences file"
   (lambda (x)
     (if (browser-preference? x)
       x
       (error 'external-browser "~e is not a valid browser preference" x)))))

;; like (system-type), but return the real OS for OSX with XonX
;;  (could do the same for Cygwin, but it doesn't have shell-execute)
(define systype
  (delay (let ([t (system-type)])
           (cond [(not (eq? t 'unix)) t]
                 [(regexp-match? #rx"-darwin($|/)"
                                 (path->string (system-library-subpath)))
                  'macosx]
                 [else t]))))

;; send-url : str [bool] -> void
(define (send-url url-str [separate-window? separate-by-default?])
  (define external (external-browser))
  (define stype (force systype))
  (define preferred '|? ? ?|)
  (define (use-browser browser-name)
    (when (eq? preferred '|? ? ?|)
      (set! preferred (or external (get-preference 'external-browser))))
    (and (or (not preferred) (eq? preferred browser-name))
         (find-executable-path (symbol->string browser-name) #f)))
  (unless (string? url-str)
    (error 'send-url "expected a string, got ~e" url-str))
  (cond
    [(procedure? external) (external url-str)]
    [(eq? stype 'macosx)
     (browser-process (format "osascript -e 'open location \"~a\"'" url-str))]
    [(eq? stype 'windows)
     (let ([simple
            (lambda ()
              (shell-execute #f url-str "" (current-directory) 'SW_SHOWNORMAL))])
       (if (regexp-match #rx"#" url-str)
         ;; complex case: need to launch the browser directly,
         ;; otherwise the fragment is ignored. Use `ftype' to discover
         ;; the browser...
         (let ([p (process "ftype htmlfile")])
           (close-output-port (cadr p))
           (let ([s (read-line (car p) 'return-linefeed)])
             (close-input-port (car p))
             (close-input-port (cadddr p))
             (let ([m (regexp-match #rx"^htmlfile=(.*)" s)])
               (if m
                 (browser-process (string-append (cadr m) " " url-str))
                 ;; give up and use simple mode
                 (simple)))))
         ;; simple case: no fragment
         (simple)))]
    [(not (eq? stype 'unix))
     (error 'send-url "don't know how to open URL on platform: ~s" stype)]
    ;; unix
    [(use-browser 'opera)
     => (lambda (exe)
          ;; opera may not return -- always open asyncronously
          ;; opera starts a new browser automatically, if it can't find one
          (browser-process* exe "-remote"
                            (format "openURL(~a)"
                                    (if separate-window?
                                      (format "~a,new-window" url-str)
                                      url-str))))]
    [(use-browser 'galeon)
     => (lambda (exe)
          (browser-process* exe (if separate-window? "-w" "-x") url-str))]
    [(use-browser 'gnome-open)
     => (lambda (exe) (browser-process* exe url-str))]
    [(or (use-browser 'netscape)
         (use-browser 'mozilla)
         (use-browser 'firefox))
     => (lambda (exe)
          ;; netscape's -remote returns with an error code, if no netscape is
          ;; around.  start a new netscape in that case.
          (or (system* exe "-remote"
                       (format "openURL(~a)"
                               (if separate-window?
                                 (format "~a,new-window" url-str)
                                 url-str)))
              (browser-process* exe url-str)))]
    [(use-browser 'dillo)
     => (lambda (exe) (browser-process* exe url-str))]
    [(custom-browser? preferred)
     (let ([cmd (string-append (car preferred)
                               url-str
                               (cdr preferred))])
       (browser-process cmd))]
    [else
     (error 'send-url "Couldn't find a browser to open URL: ~e" url-str)])
  (void))

;; run-browser : process-proc list-of-strings -> void
(define (run-browser process*/ports args)
  (define-values (stdout stdin pid stderr control)
    (apply values (apply process*/ports
                         (open-output-nowhere) #f (current-error-port)
                         args)))
  (close-output-port stdin)
  (thread (lambda ()
            (control 'wait)
            (when (eq? 'done-error (control 'status))
              (error 'run-browser "process execute failed: ~e" args))))
  (void))

(define (browser-process* . args)
  (run-browser process*/ports args))

(define (browser-process . args)
  (run-browser process/ports args))
