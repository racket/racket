;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;; Add the Swindle languages to DrRacket
#lang mzscheme

(require mzlib/unit
         drscheme/tool
         mzlib/class
         mzlib/list
         mred
         net/sendurl
         string-constants)
(provide tool@)

(define tool@
  (unit (import drscheme:tool^) (export drscheme:tool-exports^) 
    ;; Swindle languages
    (define (swindle-language module* name* entry-name* num* one-line* url*)
      (class (drscheme:language:module-based-language->language-mixin
              (drscheme:language:simple-module-based-language->module-based-language-mixin
               (class* object%
                       (drscheme:language:simple-module-based-language<%>)
                 (define/public (get-language-numbers) `(-200 2000 ,num*))
                 (define/public (get-language-position)
                   (list (string-constant legacy-languages)
                         "Swindle" entry-name*))
                 (define/public (get-module) module*)
                 (define/public (get-one-line-summary) one-line*)
                 (define/public (get-language-url) url*)
                 (define/public (get-reader)
                   (lambda (src port)
                     (let ([v (read-syntax src port)])
                       (if (eof-object? v)
                         v
                         (namespace-syntax-introduce v)))))
                 (super-instantiate ()))))
        (define/augment (capability-value key)
          (cond
           [(eq? key 'macro-stepper:enabled) #t]
           [else (inner (drscheme:language:get-capability-default key)
                        capability-value key)]))
        (define/override (use-namespace-require/copy?) #t)
        (define/override (default-settings)
          (drscheme:language:make-simple-settings
           #t 'write 'mixed-fraction-e #f #t 'debug))
        (define/override (get-language-name) name*)
        (define/override (config-panel parent)
          (let* ([make-panel
                  (lambda (msg contents)
                    (make-object message% msg parent)
                    (let ([p (instantiate vertical-panel% ()
                               (parent parent)
                               (style '(border))
                               (alignment '(left center)))])
                      (if (string? contents)
                        (make-object message% contents p)
                        (contents p))))]
                 [title-panel
                  (instantiate horizontal-panel% ()
                    (parent parent)
                    (alignment '(center center)))]
                 [title-pic
                  (make-object message%
                    (make-object bitmap%
                      (build-path (collection-path "swindle")
                                  "swindle-logo.png"))
                    title-panel)]
                 [title (let ([p (instantiate vertical-panel% ()
                                   (parent title-panel)
                                   (alignment '(left center)))])
                          (make-object message% (format "Swindle") p)
                          (make-object message% (format "Setup") p)
                          p)]
                 [input-sensitive?
                  (make-panel (string-constant input-syntax)
                              (lambda (p)
                                (make-object check-box%
                                  (string-constant case-sensitive-label)
                                  p void)))]
                 [debugging
                  (make-panel
                   (string-constant dynamic-properties)
                   (lambda (p)
                     (instantiate radio-box% ()
                       (label #f)
                       (choices
                        `(,(string-constant no-debugging-or-profiling)
                          ,(string-constant debugging)
                          ,(string-constant debugging-and-profiling)))
                       (parent p)
                       (callback void))))])
            (case-lambda
              [()
               (drscheme:language:make-simple-settings
                (send input-sensitive? get-value)
                'write 'mixed-fraction-e #f #t
                (case (send debugging get-selection)
                  [(0) 'none]
                  [(1) 'debug]
                  [(2) 'debug/profile]))]
              [(settings)
               (send input-sensitive? set-value
                     (drscheme:language:simple-settings-case-sensitive
                      settings))
               (send debugging set-selection
                     (case (drscheme:language:simple-settings-annotations
                            settings)
                       [(none) 0]
                       [(debug) 1]
                       [(debug/profile) 2]))])))
        (define last-port #f)
        (define/override (render-value/format value settings port width)
          (unless (eq? port last-port)
            (set! last-port port)
            ;; this is called with the value port, so copy the usual swindle
            ;; handlers to this port
            (port-write-handler
             port (port-write-handler (current-output-port)))
            (port-display-handler
             port (port-display-handler (current-output-port))))
          ;; then use them instead of the default pretty print
          (write value port)
          (newline port))
        (super-instantiate ())))
    (define (add-swindle-language name module entry-name num one-line url)
      (drscheme:language-configuration:add-language
       (make-object
        ((drscheme:language:get-default-mixin)
         (swindle-language `(lib ,(string-append module ".rkt") "swindle")
                           name entry-name num one-line url)))))
    (define phase1 void)
    (define (phase2)
      (for-each (lambda (args) (apply add-swindle-language `(,@args #f)))
                '(("Swindle" "main" "Full Swindle" 0
                   "Full Swindle extensions")
                  ("Swindle w/o CLOS" "turbo" "Swindle without CLOS" 1
                   "Swindle without the object system")
                  ("Swindle Syntax" "base" "Basic syntax only" 2
                   "Basic Swindle syntax: keyword-arguments etc")))
      (parameterize ([current-directory (collection-path "swindle")])
        (define counter 100)
        (define (do-customize file)
          (when (regexp-match? #rx"\\.rkt$" file)
            (with-input-from-file file
              (lambda ()
                (let ([l (read-line)])
                  (when (regexp-match? #rx"^;+ *CustomSwindle *$" l)
                    (let ([file (regexp-replace #rx"\\.rkt$" file "")]
                          [name #f] [dname #f] [one-line #f] [url #f])
                      (let loop ([l (read-line)])
                        (cond
                          [(regexp-match #rx"^;+ *([A-Z][A-Za-z]*): *(.*)$" l)
                           => (lambda (m)
                                (let ([sym (string->symbol (cadr m))]
                                      [val (caddr m)])
                                  (case sym
                                    [(|Name|)       (set! name     val)]
                                    [(|DialogName|) (set! dname    val)]
                                    [(|OneLine|)    (set! one-line val)]
                                    [(|URL|)        (set! url      val)])
                                  (loop (read-line))))]))
                      (unless name (set! name file))
                      (unless dname (set! dname name))
                      (unless one-line
                        (set! one-line
                              (string-append "Customized Swindle: " name)))
                      (set! counter (add1 counter))
                      (add-swindle-language
                       name file dname counter one-line url))))))))
        (for-each do-customize
                  (sort (map path->string (directory-list)) string<?))))
    ))
