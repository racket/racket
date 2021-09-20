;; The main client of this module is browser/external.rkt
;;   (others just use the (send-url url [new?]) interface.)

#lang racket/base

(require racket/system racket/file racket/port
         racket/contract
         "sendurl/preferences.rkt"
         (submod "sendurl/preferences.rkt" internals))

(provide send-url send-url/file send-url/contents

         ;; Reproviding bindings from sendurl/preferences for backwards compatibility
         browser-list external-browser
         ;; A new config parameter: allowing trampoline or not
         external-browser-trampoline
         ;; Obsolete definitions
         unix-browser-list browser-preference?
         (contract-out
          [send-url/mac
           (->* (string?) (#:browser string?)
                #:pre (equal? (system-type) 'macosx)
                void?)]))

;; Will we open a new browser window (where possible) by default?
(define separate-by-default? #t)

(define (send-url/mac url #:browser [browser #f])
  (let ([browser-command (car browser-list)])
    (if browser
        (browser-run browser-command "-a" browser url)
        (browser-run browser-command url))))


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
  (unless (string? url-str)
    (error 'send-url "expected a string, got ~e" url-str))
  (let ([url-str (if escape? (escape-url url-str) url-str)])
    (if (procedure? (external-browser))
      ((external-browser) url-str)
      (if (and (regexp-match? #rx"[#?]" url-str)
               (external-browser-allow-trampoline?))
          (send-url/trampoline url-str separate-window?)
          (send-url/simple url-str separate-window?))))
  (void))

(define (send-url/file path [separate-window? separate-by-default?]
                       #:fragment [fragment #f] #:query [query #f])
  (let* ([path (path->string (path->complete-path path))]
         [path (if (eq? 'windows (system-type))
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

(define (send-url/simple url [separate-window? separate-by-default?])
  ;; in cases where a browser was uninstalled, we might get a preference that
  ;; is no longer valid, this will turn it back to #f
  (define browser
    (available-external-browser))
  (define exe
    (cond [(existing-browsers->exes browser) => cdr]
          [else #f]))
  (define (simple) (browser-run exe url))
  (define (w/arg a) (browser-run exe a url))
  (define (try-remote)
    (or (browser-run exe "-remote" (format "openURL(~a~a)" url
                                           (if separate-window? ",new-window" "")))
        (simple)))
  (define (windows-start)
    (shell-execute #f url "" (current-directory) 'SW_SHOWNORMAL))
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
       [(open xdg-open
         sensible-browser x-www-browser firefox konqueror google-chrome chromium-browser)
        (simple)]
       [(cmd.exe) (windows-start)]
       ;; don't really know how to run these
       [(epiphany) (if separate-window? (w/arg "--new-window") (simple))]
       [(seamonkey opera) (try-remote)]
       [else (error 'send-url "internal error")])]))

;; Write and use (via `send-url/contents') a trampoline html that redirects
;; to the actual file and fragment, for launchers that can't cope with query
;; and fragment part of the URL.
(define (send-url/trampoline url [separate-window? separate-by-default?])
  (send-url/contents
   (string-append
    "<html><head><meta http-equiv=\"refresh\" content=\"0;URL="url"\"></head>"
    "<body>Please go <a href=\""url"\">here</a>.</body></html>")
   separate-window?
   ;; starting the browser may take a while, don't remove the file
   ;; immediately (this means that when used via plt-help, these files are
   ;; never removed by a timer)
   #:delete-at 15))

;; Process helper
(define (browser-run #:shell [shell? #f] . args)
  (define stderr (open-output-string))
  (define-values (stdout stdin pid _stderr control)
    (apply values (apply (if shell? process/ports process*/ports)
                         (open-output-nowhere) #f stderr
                         args)))
  (close-output-port stdin)
  ;; this is called from plt-help which will immediately exit when we
  ;; return, so wait just a little bit in case we'll catch an error
  ;; starting the browser
  (sync/timeout 0.25
    (thread (lambda ()
              (control 'wait)
              (when (eq? 'done-error (control 'status))
                (error 'browser-run "process execute failed: ~e\n~a"
                       args
                       (get-output-string stderr))))))
  (void))
