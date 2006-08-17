;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;; This allows adding a Swindle icon on startup.
(module tool mzscheme
  (require (lib "unitsig.ss")
           (lib "tool.ss" "drscheme")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred")
           (lib "sendurl.ss" "net")
           (lib "string-constant.ss" "string-constants"))
  (provide tool@)
  (define tool@
    (unit/sig drscheme:tool-exports^ (import drscheme:tool^)
      ;; Swindle languages
      (define (swindle-language module* name* entry-name* num* one-line* url*)
        (class (drscheme:language:module-based-language->language-mixin
                (drscheme:language:simple-module-based-language->module-based-language-mixin
                 (class* object%
                         (drscheme:language:simple-module-based-language<%>)
                   (define/public (get-language-numbers) `(-1000 2000 ,num*))
                   (define/public (get-language-position)
                     (list (string-constant professional-languages)
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
          (define/override (use-namespace-require/copy?) #t)
          (define/override (default-settings)
            (drscheme:language:make-simple-settings
             #t 'current-print 'mixed-fraction-e #f #t 'debug))
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
                         (callback void))))]
                   [output
                    (make-panel (string-constant output-style-label)
                                "always current-print")])
              (case-lambda
               [()
                (drscheme:language:make-simple-settings
                 (send input-sensitive? get-value)
                 'current-print 'mixed-fraction-e #f #t
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
          (define/override (render-value/format value settings port port-write)
            (parameterize ([current-output-port port]
                           [current-inspector (make-inspector)])
              ((current-print) value)))
          (super-instantiate ())))
      (define (add-swindle-language name module entry-name num one-line url)
        (drscheme:language-configuration:add-language
         (make-object
          ((drscheme:language:get-default-mixin)
           (swindle-language `(lib ,(string-append module ".ss") "swindle")
                             name entry-name num one-line url)))))
      (define phase1 void)
      (define (phase2)
        (for-each (lambda (args)
                    (apply add-swindle-language `(,@args #f)))
                  '(("Swindle" "swindle" "Full Swindle" 0
                     "Full Swindle extensions")
                    ("Swindle w/o CLOS" "turbo" "Swindle without CLOS" 1
                     "Swindle without the object system")
                    ("Swindle Syntax" "base" "Basic syntax only" 2
                     "Basic Swindle syntax: keyword-arguments etc")
                    ("HTML Swindle" "html" "HTML Swindle" 3
                     "Swindle's HTML extension")))
        (parameterize ([current-directory (collection-path "swindle")])
          (define counter 100)
          (define (do-customize file)
            (when (regexp-match #rx"\\.ss$" file)
              (with-input-from-file file
                (lambda ()
                  (let ([l (read-line)])
                    (when (regexp-match #rx"^;+ *CustomSwindle *$" l)
                      (let ([file (regexp-replace #rx"\\.ss$" file "")]
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
      )))
