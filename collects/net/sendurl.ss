;; The main client of this module is browser/external.ss
;;   (others just use the (send-url url [new?]) interface.)

#lang scheme/base

(require scheme/system
         scheme/file
         scheme/promise
         scheme/port)

(provide send-url send-url/file
         unix-browser-list browser-preference? external-browser)

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

(define (%escape str)
  (apply string-append
         (map (lambda (b)
                (string-append "%" (if (< b 16) "0" "") (number->string b 16)))
              (bytes->list (string->bytes/utf-8 str)))))

;; Used for quoting characters that will not work well in shell quotes, and
;; only these characters.  This is only for protection when passing arguments
;; to subprocesses, it's best to pass properly escaped urls to `send-url'.
(define (escape-url url)
  (regexp-replace* #px"(?:[^[:graph:]]|[\"'`\\\\])" url %escape))

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
        [(unix)    (send-url/unix url-str separate-window?)]
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
    (send-url path separate-window? #:escape? #f)))

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

;; Windows has a bug when using `shell-execute' or when running `iexplore.exe'
;; directly -- it silently drops the fragment and query from URLs that have
;; them.  This is described at
;;   http://support.microsoft.com/default.aspx/kb/942172
;; It seems that the IE7 problem happens either way (`shell-execute' or running
;; directly) -- but it also happens with firefox when using `shell-execute'.
;; The current solution is to run `ftype http' to find the default browser
;; command, if it uses `iexplore.exe', then change it to `explorer.exe', and
;; run the resulting command directly.  This is described at
;;   http://www.tutorials-win.com/IE/Lauching-HTML/
;; Hopefully this works.  One question is whether IE6 will still work fine;
;; another is other browsers work; and finally, we need to parse the command
;; and substitute the url for `%1' (if it appears).  If there are other `%'s,
;; throw an error so we can hack that in too.
;; Oh and it seems that there is no way to get this to work on Vista, the above
;; MS page says that the problem is that IE will start a more priviliged one,
;; handing over the URL -- which, again, gets the fragment+query stripped
;; away...

(define windows-http-command
  (delay (let ([out (open-output-string)])
           (parameterize ([current-output-port out]
                          [current-input-port (open-input-string "")]
                          [current-error-port (open-output-nowhere)])
             (and (system "ftype http")
                  (cond [(regexp-match #rx"(?:^|\r?\n)?http=([^\r\n]+)\r?\n"
                                       (get-output-string out))
                         => cadr]
                        [else #f]))))))

(define (send-url/win url)
  (let ([cmd (force windows-http-command)])
    (browser-run
     #:shell #t #:ignore-exit-code #t
     (cond [(and (or (not cmd)
                     (regexp-match? #px"(?:^|[/\\\\])(?i:iexplore.exe)" cmd))
                 ;; IE: try to find exeplorer instead
                 (find-exe "explorer.exe"))
            => (lambda (exe) (format "\"~a\" ~a" exe url))]
           [(not (regexp-match? #rx"%" cmd))
            (format "~a ~a" cmd url)]
           [(regexp-match? #rx"%[^1]" cmd)
            (error 'send-url/win "Unknown browser configuration: ~s\n~a"
                   cmd "*** Please report this as a bug!")]
           [else (regexp-replace* #rx"%1" cmd url)]))))

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
