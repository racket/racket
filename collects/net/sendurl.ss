;; The main client of this module is browser/external.ss
;;   (others just use the (send-url url [new?]) interface.)

#lang scheme/base

(require scheme/system
         scheme/file
         scheme/promise
         scheme/port)

(provide send-url unix-browser-list browser-preference? external-browser)

(define separate-by-default?
  ;; internal configuration, 'browser-default lets some browsers decide
  (get-preference 'new-browser-for-urls (lambda () 'browser-default)))

;; all possible unix browsers, filtered later to just existing executables
;; order matters: the default will be the first of these that is found
(define all-unix-browsers
  '(gnome-open firefox galeon opera mozilla konqueror camino skipstone
    epiphany seamonkey netscape dillo mosaic
    ;; a configurable thing that is deprecated
    htmlview))

;; : any -> bool
(define (custom-browser? x)
  (and (pair? x) (string? (car x)) (string? (cdr x))))

(define external-browser
  (make-parameter
   #f ; #f means "consult the preferences file"
   (lambda (x)
     (if (browser-preference? x)
       x
       (error 'external-browser "~e is not a valid browser preference" x)))))

;; by-need filtering of found unix executables
(define existing-unix-browsers->exes
  (delay
    (filter values
            (map (lambda (b)
                   (let ([exe (find-executable-path (symbol->string b) #f)])
                     (and exe (cons b exe))))
                 all-unix-browsers))))
(define existing-unix-browsers
  (delay (map car (force existing-unix-browsers->exes))))
(define-syntax unix-browser-list
  (syntax-id-rules (set!)
    [(_ . xs) ((force existing-unix-browsers) . xs)]
    [(set! _ . xs) (error 'unix-browser-list "cannot be mutated")]
    [_ (force existing-unix-browsers)]))

;; : any -> bool
(define (browser-preference? x)
  (or (not x) (memq x unix-browser-list) (custom-browser? x) (procedure? x)))

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
  (define stype (force systype))
  (unless (string? url-str)
    (error 'send-url "expected a string, got ~e" url-str))
  (let ([url-str
         ;; quote characters that will not work well in shell quotes
         (regexp-replace*
          #rx"[\"'`$\\]" url-str
          (lambda (m)
            (string-append "%" (number->string (char->integer (string-ref m 0))
                                               16))))])
    (if (procedure? (external-browser))
      ((external-browser) url-str)
      (case stype
        [(macosx)  (send-url/mac url-str)]
        [(windows) (send-url/win url-str)]
        [(unix)    (send-url/unix url-str separate-window?)]
        [else (error 'send-url
                     "don't know how to open URL on platform: ~s" stype)])))
  (void))

(define osascript (delay (find-executable-path "osascript" #f)))
(define (send-url/mac url-str)
  (browser-run (force osascript) "-e" (format "open location \"~a\"" url-str)))

(define (send-url/win url-str)
  (define (simple)
    (shell-execute #f url-str "" (current-directory) 'SW_SHOWNORMAL))
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
            (browser-run #:shell #t (string-append (cadr m) " " url-str))
            ;; give up and use simple mode
            (simple)))))
    ;; simple case: no fragment
    (simple)))

(define (send-url/unix url-str separate-window?)
  ;; in cases where a browser was uninstalled, we might get a preference that
  ;; is no longer valid, this will turn it back to #f
  (define (try pref)
    (if (symbol? pref)
      (if (memq pref unix-browser-list) pref #f)
      pref))
  (define browser
    (or (try (external-browser))
        (try (get-preference 'external-browser))
        ;; no preference -- chose the first one from the filtered list
        (and (pair? unix-browser-list) (car unix-browser-list))))
  (define exe
    (cond [(assq browser (force existing-unix-browsers->exes)) => cdr]
          [else #f]))
  (define (simple) (browser-run exe url-str))
  (define (w/arg a) (browser-run exe a url-str))
  (define (try-remote)
    (or (system* exe "-remote"
                 (format "openURL(~a)"
                         (if separate-window?
                           (format "~a,new-window" url-str)
                           url-str)))
        (simple)))
  (cond
    [(not browser)
     (error 'send-url "Couldn't find a browser to open URL: ~e" url-str)]
    [(custom-browser? browser)
     (browser-run #:shell #t
                  (string-append (car browser) url-str (cdr browser)))]
    ;; if it's a known browser, then it must be an existing one at this point
    [(not exe) (error 'send-url "internal error")]
    ;; if it's gone throw an error (refiltering will break assumptions of
    ;; browser/external.ss, and we really mimic the Win/Mac case where there
    ;; should be some builtin facility that doesn't change)
    [(not (file-exists? exe)) (error 'send-url "executable vanished: ~a" exe)]
    ;; finally, deal with the actual browser process
    [else
     (case browser
       [(gnome-open firefox konqueror dillo htmlview) (simple)]
       ;; don't really know how to run these
       [(camino skipstone mosaic) (simple)]
       [(galeon) (if (eq? 'browser-default separate-window?)
                   (simple) (w/arg (if separate-window? "-w" "-x")))]
       [(epiphany) (if separate-window? (w/arg "--new-window") (simple))]
       [(mozilla seamonkey netscape) (try-remote)]
       [(opera)
        ;; opera starts a new browser automatically
        (browser-run exe "-remote"
                     (format "openURL(~a~a)"
                             url-str (if separate-window? ",new-window" "")))]
       [else (error 'send-url "internal error")])]))

;; Process helper
(define (browser-run #:shell? [shell? #f] . args)
  (define-values (stdout stdin pid stderr control)
    (apply values (apply (if shell? process/ports process*/ports)
                         (open-output-nowhere) #f (current-error-port)
                         args)))
  (close-output-port stdin)
  (thread (lambda ()
            (control 'wait)
            (when (eq? 'done-error (control 'status))
              (error 'browser-run "process execute failed: ~e" args))))
  (void))
