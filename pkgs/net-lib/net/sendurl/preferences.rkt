#lang racket/base

(require racket/file racket/promise racket/contract)

(provide browser-list
         unix-browser-list browser-preference? ;; Obsolete definitions

         ;; Browser preference
         external-browser
         available-external-browser
         browser-preference-key

         ;; URL (trampoline) preference
         external-browser-trampoline
         external-browser-allow-trampoline?
         trampoline-preference-key)

(module* internals #f
  (provide existing-browsers->exes
           custom-browser?
           browser-preference?))

(define browser-preference-key 'external-browser)
(define trampoline-preference-key 'external-browser-trampoline)
(define trampoline-preference-default #t)

(define-syntax browser-list
  (syntax-id-rules (set!)
    [(_ . xs) ((existing-browsers) . xs)]
    [(set! _ . xs) (error 'browser-list "cannot be mutated")]
    [_ (existing-browsers)]))

(define (existing-browsers)
  (force existing-browsers-exes-keys))

(define (existing-browsers->exes browser)
  (assq browser (force existing-browsers-exes-dict)))

;; all possible unix browsers, filtered later to just existing executables
;; order matters: the default will be the first of these that is found
(define all-browsers/unix
  '(;; general purpose launchers
    xdg-open
    ;; default browser launchers
    sensible-browser x-www-browser
    ;; common browsers
    firefox chromium-browser google-chrome opera seamonkey epiphany
    ))
(define all-browsers/win '(cmd.exe)) ; proxy for a basic functioning Windows system
(define all-browsers/mac '(open))

;; by-need filtering of found executables
(define existing-browsers-exes-dict
  (delay/sync
    (filter values
            (map (lambda (b)
                   (let ([exe (find-executable-path (symbol->string b) #f)])
                     (and exe (cons b exe))))
                 (case (system-type)
                   [(macosx)  all-browsers/mac]
                   [(windows) all-browsers/win]
                   [(unix)    all-browsers/unix]
                   [else (error 'send-url
                                "don't know how to open URL on platform: ~s" (system-type))])))))

(define existing-browsers-exes-keys
  (delay/sync (map car (force existing-browsers-exes-dict))))


;; Backwards compatibility

(define unix-browser-list browser-list)

;; : any -> bool
(define (custom-browser? x)
  (and (pair? x) (string? (car x)) (string? (cdr x))))

;; : any -> bool
(define (browser-preference? x)
  (or (not x) (memq x browser-list) (custom-browser? x) (procedure? x)))

(define external-browser
  (make-parameter
   #f ; #f means "consult the preferences file"
   (lambda (x)
     (if (browser-preference? x)
       x
       (error 'external-browser "~e is not a valid browser preference" x)))))

;; in cases where a browser was uninstalled, we might get a preference that
;; is no longer valid, this will turn it back to #f
(define (try pref)
  (if (symbol? pref)
      (if (memq pref browser-list) pref #f)
      pref))

(define (available-external-browser)
  (or (try (external-browser))
      (try (get-preference browser-preference-key))
      ;; no preference -- chose the first one from the filtered list
      (and (pair? browser-list) (car browser-list))))

(define external-browser-trampoline
  (make-parameter
   'dont-care
   (lambda (x)
     (unless (or (eq? x 'dont-care)
                 (boolean? x))
       (raise-argument-error 'external-browser-allow-trampoline
                             "(or/c #t #f 'dont-care)"
                             x))
     x)))

(define (external-browser-allow-trampoline?)
  (define trampoline-parameter-value
    (external-browser-trampoline))
  (cond
    [(boolean? trampoline-parameter-value) trampoline-parameter-value]
    [else
     (get-preference trampoline-preference-key
                     (lambda () trampoline-preference-default))]))


