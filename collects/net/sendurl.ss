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

(define (find-exe name)
  (find-executable-path name #f))

;; by-need filtering of found unix executables
(define existing-unix-browsers->exes
  (delay
    (filter values (map (lambda (b)
                          (let ([exe (find-exe (symbol->string b))])
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

(define osascript (delay (find-exe "osascript")))
(define (send-url/mac url)
  (browser-run (force osascript) "-e" (format "open location \"~a\"" url)))

(define (send-url/unix url separate-window?)
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
  (define (simple) (browser-run exe url))
  (define (w/arg a) (browser-run exe a url))
  (define (try-remote)
    (or (system* exe "-remote" (format "openURL(~a~a)" url
                                       (if separate-window? ",new-window" "")))
        (simple)))
  (cond
    [(not browser)
     (error 'send-url "Couldn't find a browser to open URL: ~e" url)]
    [(custom-browser? browser)
     (browser-run #:shell #t (string-append (car browser) url (cdr browser)))]
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
                             url (if separate-window? ",new-window" "")))]
       [else (error 'send-url "internal error")])]))

;; Windows -- IE7 has a bug: when launched (either through shell-execute or
;; directly) it will ignore the fragment for `file:' URLs.  A workaround that
;; seems to work is to run `explorer.exe' instead.  The problem is described at
;; http://support.microsoft.com/default.aspx/kb/942172, and when a fix will be
;; released (and enough time to be widely used), this whole thing should go
;; away and the simple `shell-eexcute' will work fine.  (See also
;; http://www.tutorials-win.com/IE/Lauching-HTML/)

(define (using-ie7?)
  (define (bytes0 bs)
    (list->bytes (apply append (map (lambda (b) (list b 0)) (bytes->list bs)))))
  (define (get-regdata)
    (define regfile (make-temporary-file "registry-data-~a"))
    (and (system (format "regedit /e \"~a\" \"~a" regfile
                         (regexp-replace* #rx"/" keypath "\\\\")))
         (let ([x (file-size regfile)])
           (begin0 (with-input-from-file regfile (lambda () (read-bytes x)))
             (delete-file regfile)))))
  (define keypath
    "HKEY_LOCAL_MACHINE/Software/Microsoft/Internet Explorer/Version Vector")
  (define version-rx
    (bytes-append (bytes0 #"\r\n\"IE\"=\"") #"([0-9.\0]+)" (bytes0 #"\"\r\n")))
  (and
   ;; Is IE the default browser?
   (let ([p (process "ftype http")])
     (close-output-port (cadr p))
     (let ([s (read-line (car p) 'return-linefeed)])
       (close-input-port (car p))
       (close-input-port (cadddr p))
       (regexp-match? #px"^(?i:http=\"(.*\\\\|)iexplore.exe\")" s)))
   ;; Get the registry data and check the version.  We could convert the UTF-16
   ;; result to UTF-8, but we're looking for a simple pattern, so just search
   ;; for the expected UTF-16 sequence directly.
   (cond [(regexp-match version-rx (get-regdata))
          => (lambda (m) (regexp-match? #rx#"^7\0\\.\0" (cadr m)))]
         [else #f])))

(define send-url/win-proc
  (delay (let ([explorer (and (using-ie7?) (find-exe "explorer.exe"))])
           (if explorer
             (lambda (url) (browser-run #:ignore-exit-code #t explorer url))
             (lambda (url)
               (shell-execute #f url "" (current-directory) 'SW_SHOWNORMAL))))))

(define (send-url/win url) ((force send-url/win-proc) url))

;; Process helper
(define (browser-run #:shell [shell? #f] #:ignore-exit-code [nowait? #f] . args)
  (define-values (stdout stdin pid stderr control)
    (apply values (apply (if shell? process/ports process*/ports)
                         (open-output-nowhere) #f (current-error-port)
                         args)))
  (close-output-port stdin)
  (unless nowait?
    (thread (lambda ()
              (control 'wait)
              (when (eq? 'done-error (control 'status))
                (error 'browser-run "process execute failed: ~e" args)))))
  (void))
