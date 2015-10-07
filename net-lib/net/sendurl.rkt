;; The main client of this module is browser/external.rkt
;;   (others just use the (send-url url [new?]) interface.)

#lang racket/base

(require racket/system racket/file racket/promise racket/port
         racket/contract racket/promise)

(provide send-url send-url/file send-url/contents
         unix-browser-list browser-preference? external-browser
         (contract-out
          [send-url/mac
           (->* (string?) (#:browser string?)
                #:pre (equal? (system-type) 'macosx)
                void?)]))

(define separate-by-default?
  ;; internal configuration, 'browser-default lets some browsers decide
  (delay (get-preference 'new-browser-for-urls
                         (lambda () 'browser-default)
                         #:timeout-lock-there (lambda (path) 'browser-default))))

;; all possible unix browsers, filtered later to just existing executables
;; order matters: the default will be the first of these that is found
(define all-unix-browsers
  '(;; common browsers
    ;; xdg-open
    firefox google-chrome galeon opera mozilla konqueror seamonkey epiphany
    ;; known browsers
    camino skipstone
    ;; broken browsers (broken in that they won't work with plt-help)
    ;; this is a configurable thing that is deprecated, but better
    ;; than gnome-open (because it works)
    htmlview
    ;; gnome-open could be high, but the problem is that it doesn't
    ;; handle file:// URLs with a query string.
    gnome-open
    ;; dillo does not have javascript
    dillo
    ;; ancient browsers
    netscape mosaic
    ))

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
  (delay/sync
    (filter values
            (map (lambda (b)
                   (let ([exe (find-executable-path (symbol->string b) #f)])
                     (and exe (cons b exe))))
                 all-unix-browsers))))
(define existing-unix-browsers
  (delay/sync (map car (force existing-unix-browsers->exes))))
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
  (delay/sync (let ([t (system-type)])
                (cond [(not (eq? t 'unix)) t]
                      [(regexp-match? #rx"-darwin($|/)"
                                      (path->string (system-library-subpath)))
                       'macosx]
                      [else t]))))

(define (%escape str)
  (apply string-append
         (map (lambda (b)
                (string-append "%" (if (< b 16) "0" "") (number->string b 16)))
              (bytes->list (string->bytes/utf-8 str)))))

;; Used for quoting characters that will not work well in shell quotes, and
;; only these characters.  This is only for protection when passing arguments
;; to subprocesses, it's best to pass properly escaped urls to `send-url'.
(define (escape-url url)
  (regexp-replace* #px"(?:[^[:graph:]]|[$\"'`\\\\])" url %escape))

;; send-url : str [bool] -> void
(define (send-url url-str [separate-window? separate-by-default?]
                  #:escape? [escape? #t])
  (define stype (force systype))
  (unless (string? url-str)
    (error 'send-url "expected a string, got ~e" url-str))
  (let ([url-str (if escape? (escape-url url-str) url-str)])
    (if (procedure? (external-browser))
      ((external-browser) url-str)
      (case stype
        [(macosx)  (send-url/mac url-str)]
        [(windows) (send-url/win url-str)]
        [(unix)    (send-url/unix url-str (force separate-window?))]
        [else (error 'send-url
                     "don't know how to open URL on platform: ~s" stype)])))
  (void))

(define (send-url/file path [separate-window? separate-by-default?]
                       #:fragment [fragment #f] #:query [query #f])
  (let* ([path (path->string (path->complete-path path))]
         [path (if (eq? 'windows (force systype))
                 ;; see http://msdn2.microsoft.com/en-us/library/ms775098.aspx
                 (let* ([path (regexp-replace* #rx"\\\\" path "/")]
                        [slashes (cdar (regexp-match-positions #rx"^/*" path))])
                   (case slashes
                     [(0) (string-append "/" path)]
                     [(1) (error 'send-url/file
                                 "unexpected path, please submit a bug: ~s"
                                 path)]
                     [else (substring path 2)]))
                 path)]
         [path (regexp-replace* #rx"[^A-Za-z0-9_./:-]" path %escape)]
         [path (string-append "file://" path)]
         [path (if query (string-append path "?" (escape-url query)) path)]
         [path (if fragment (string-append path "#" (escape-url fragment))
                   path)])
    (send-url path (force separate-window?) #:escape? #f)))

;; See the documentation for the `delete-at' argument
;; separate-window? is never used
(define (send-url/contents contents [separate-window? separate-by-default?]
                           #:fragment [fragment #f] #:query [query #f]
                           #:delete-at [delete-at #f])
  (define tmp-tmpl "plt-sendurl-contents-file-~a.html")
  (define tmp-rx   #rx"^plt-sendurl-contents-file-.*\\.html$")
  ;; The code below will often leave leftovers (for example, plt-help will quit
  ;; before deletion happens), so every once in a while, do a cleanup.  This
  ;; can also remove files that were created with no intention for deletion
  ;; (when delete-at is #f), so don't remove files that are less than 15
  ;; minutes old.
  (when (zero? (random 5))
    (parameterize ([current-directory (find-system-path 'temp-dir)])
      (let ([now (current-seconds)])
        (for ([file (directory-list)]
              #:when (and (file-exists? file)
                          (regexp-match tmp-rx (path-element->string file))
                          (> (- now (file-or-directory-modify-seconds file))
                             (* 15 60))))
          ;; The temp directory may be shared with other users, so silently
          ;; ignore failures to remove files.
          (with-handlers ([void void]) (delete-file file))))))
  (let ([temp (make-temporary-file tmp-tmpl)])
    (with-output-to-file temp #:exists 'truncate
      (lambda () (display contents)))
    (when delete-at (thread (lambda () (sleep delete-at) (delete-file temp))))
    (send-url/file temp)))

(define osascript (delay/sync (find-executable-path "osascript" #f)))
(define (send-url/mac url #:browser [browser #f])
  (browser-run (force osascript) "-e"
               (if browser
                   (format "tell application \"~a\" to open location \"~a\""
                           browser url)
                   (format "open location \"~a\"" url))))

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
    ;; browser/external.rkt, and we really mimic the Win/Mac case where there
    ;; should be some builtin facility that doesn't change)
    [(not (file-exists? exe)) (error 'send-url "executable vanished: ~a" exe)]
    ;; finally, deal with the actual browser process
    [else
     (case browser
       [(xdg-open gnome-open firefox konqueror dillo htmlview google-chrome)
        (simple)]
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

;; Windows has a bug when using `shell-execute' or when running `iexplore.exe'
;; directly -- it silently drops the fragment and query from URLs that have
;; them.  This is described at
;;   http://support.microsoft.com/default.aspx/kb/942172
;; It seems that the IE7 problem happens either way (`shell-execute' or running
;; directly) -- but it also happens with firefox when using `shell-execute'.
;; One possible solution is to run `ftype http' to find the default browser
;; command, and if it uses `iexplore.exe' then change it to `explorer.exe', and
;; run the resulting command directly.  This is described at
;;   http://www.tutorials-win.com/IE/Lauching-HTML/
;; But this still fails on Vista, since the problem there is that launching a
;; browser with a file:// URL makes it start a more priviliged process, and
;; doing that drops the fragment again.  So the solution that the code below
;; implements is to write and use (via `send-url/contents') a trampoline html
;; that redirects to the actual file and fragment.

(define (send-url/win url)
  (if (not (regexp-match? #rx"[#?]" url))
    (shell-execute #f url "" (current-directory) 'SW_SHOWNORMAL)
    (send-url/contents
     (string-append
      "<html><head><meta http-equiv=\"refresh\" content=\"0;URL="url"\"></head>"
      "<body>Please go <a href=\""url"\">here</a>.</body></html>")
     ;; starting the browser may take a while, don't remove the file
     ;; immediately (this means that when used via plt-help, these files are
     ;; never removed by a timer)
     #:delete-at 15)))

;; Process helper
(define (browser-run #:shell [shell? #f] . args)
  (define-values (stdout stdin pid stderr control)
    (apply values (apply (if shell? process/ports process*/ports)
                         (open-output-nowhere) #f (current-error-port)
                         args)))
  (close-output-port stdin)
  ;; this is called from plt-help which will immediately exit when we
  ;; return, so wait just a little bit in case we'll catch an error
  ;; starting the browser
  (sync/timeout 0.25
    (thread (lambda ()
              (control 'wait)
              (when (eq? 'done-error (control 'status))
                (error 'browser-run "process execute failed: ~e" args)))))
  (void))
