#lang scheme
(require (planet cce/scheme:6/planet))
(require scheme/gui/base
         framework
         drscheme/tool
         scheme/match
         scheme/unit
         scheme/class
         string-constants
         "syntax-color.ss"
         "../private/compiler.ss"
         "../parse.ss"
         "../pretty.ss"
         "../eval.ss"
         "../runtime.ss"
         (only-in (planet dherman/pprint:4)
                  pretty-print)
         (for-template "../lang/lang.ss"))
(provide tool@)

(define tool@
  (unit 
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    
    (define (phase1) (void))    
    (define (phase2) 
      (drscheme:language-configuration:add-language
       (make-object ((drscheme:language:get-default-mixin)
                     (datalog-lang-mixin)))))
    
    (define (datalog-lang-mixin)
      (class* object% (drscheme:language:language<%>)
        (define/public (default-settings) #f)
        (define/public (default-settings? x) (false? x))
        (define/public (marshall-settings x) x)
        (define/public (unmarshall-settings x) x)
        (define/public (get-reader-module) #f)
        (define/public (get-metadata a b) #f)
        (define/public (metadata->settings m) #f)
        (define/public (get-metadata-lines) #f)
        (define/public (capability-value capability)
          (case capability
            [(drscheme:check-syntax-button) #t]
            [(drscheme:language-menu-title) "Datalog"]
            [(drscheme:define-popup) #f]
            [(drscheme:special:insert-fraction) #f]
            [(drscheme:special:insert-lambda) #f]
            [(drscheme:special:insert-large-letters) #t]
            [(drscheme:special:insert-image) #f]
            [(drscheme:special:insert-comment-box) #f]
            [(drscheme:special:insert-gui-tool) #f]
            [(drscheme:special:slideshow-menu-item) #f]
            [(drscheme:special:insert-text-box) #f]
            [(drscheme:special:xml-menus) #f]
            [else (drscheme:language:get-capability-default capability)]))
        (define/public (first-opened) (void))
        (define/public (get-comment-character) (values "%" #\*))
        (define/public (config-panel parent)
          (letrec ([top (instantiate vertical-panel% ()
                          (parent parent)
                          (alignment '(center center))
                          (stretchable-height #f)
                          (stretchable-width #f))])
            (case-lambda
              [() #f]
              [(settings)
               (void)])))
        (define/public (front-end/complete-program port settings)
          (lambda ()
            (if (eof-object? (peek-char-or-special port))
                eof
                (namespace-syntax-introduce (datum->syntax #f `(script-begin ,@(parse-program port)))))))
        (define/public (extra-repl-information settings port) (void))
        (define/public (front-end/finished-complete-program settings) (void))
        (define/public (front-end/interaction port settings)
          (lambda ()
            (if (or (not (char-ready? port))
                    (eof-object? (peek-char-or-special port)))                          
                eof
                (namespace-syntax-introduce (datum->syntax #f `(interaction-begin ,(parse-statement port)))))))
        (define/public (get-style-delta) #f)
        (define/public (get-language-position)
          (list (string-constant experimental-languages)
                "Datalog"))
        (define/public (order-manuals x)
          ; XXX Returns a sublist of its input, that specifies the manuals (and their order) to search in. 
          ;     The boolean result indicates if doc.txt files should be searched.
          (values x #f))
        (define/public (get-language-name) "Datalog")
        (define/public (get-language-url) "http://en.wikipedia.org/wiki/Datalog")
        (define/public (get-language-numbers) (list 1000 42))
        (define/public (get-teachpack-names) null)
        (define/public (on-execute settings run-in-user-thread)
          (let ([module-forms `(planet ,(this-package-version-symbol drscheme/module-forms))]
                [runtime `(planet ,(this-package-version-symbol eval))]
                [lang `(planet ,(this-package-version-symbol lang/lang))])
            (dynamic-require module-forms #f)
            (dynamic-require runtime #f)
            (dynamic-require lang #f)
            (let ([path1 ((current-module-name-resolver) module-forms #f #f)]
                  [path2 ((current-module-name-resolver) runtime #f #f)]
                  [path5 ((current-module-name-resolver) lang #f #f)]
                  [n (current-namespace)])
              (run-in-user-thread
               (lambda ()
                 (let ([previous-eval (drscheme:debug:make-debug-eval-handler (current-eval))])
                   (current-eval
                    (lambda (exp)
                      (previous-eval (if (syntax? exp)
                                         (namespace-syntax-introduce exp)
                                         exp)))))
                 (namespace-attach-module n path1)
                 (namespace-require path1)
                 (namespace-attach-module n path2)
                 (namespace-require path2)
                 (namespace-attach-module n path5)
                 (namespace-require path5)
                 (current-theory (make-mutable-theory))
                 )))))
        (define/public (render-value value settings port)
          (if (void? value)
              (void)
              (pretty-print (format-literals value) port)))
        (define/public (render-value/format value settings port width)
          (if (void? value)
              (void)
              (pretty-print (format-literals value) port width)))
        (define/public (create-executable fn parent . args)
          (message-box "Unsupported"
                       "Sorry - executables are not supported for Datalog at this time"
                       parent)
          (void))
        (define/public (get-one-line-summary) "Datalog")
        (super-make-object)))
    
    ; Syntax coloring
    (define (short-sym->pref-name sym) (string->symbol (short-sym->style-name sym)))
    
    ;; short-sym->style-name : symbol->string
    ;; converts the short name (from the table above) into a name in the editor list
    ;; (they are added in by `color-prefs:register-color-pref', called below)
    (define (short-sym->style-name sym) (format "datalog:syntax-colors:scheme:~a" sym))
    
    (define color-prefs-table
      `((keyword     ,(make-object color% 38 38 128)      "keyword")
        (parenthesis ,(make-object color% 132 60 36)     "parenthesis")
        (string      ,(make-object color% "forestgreen") "string")
        (comment     ,(make-object color% 194 116 31)    "comment")
        (error       ,(make-object color% "red")         "error")
        (identifier  ,(make-object color% "purple")     "identifer")
        (default     ,(make-object color% "black")       "default")))
    
    ;; extend-preferences-panel : vertical-panel -> void
    ;; adds in the configuration for the Honu colors to the prefs panel
    (define (extend-preferences-panel parent)
      (for-each
       (lambda (line)
         (let ([sym (car line)])
           (color-prefs:build-color-selection-panel 
            parent
            (short-sym->pref-name sym)
            (short-sym->style-name sym)
            (format "~a" sym))))
       color-prefs-table))
    
    (define datalog:surrogate-text%
      (class mode:surrogate-text%
        (define/override (put-file text sup directory default-name)
          (parameterize ([finder:default-filters
                          (list (list "Prolog (.prolog)" "*.prolog")
                                (list "Datalog (.datalog)" "*.datalog")
                                (list "Any" "*.*"))]
                         [finder:default-extension "datalog"])
            (sup directory default-name)))
        (super-make-object)))
    
    (define datalog:surrogate-text-mode%
      (color:text-mode-mixin datalog:surrogate-text%))
    
    (define mode-surrogate
      (new datalog:surrogate-text-mode% ; color:text-mode%
           (matches (list (list '|(| '|)|)))
           (get-token get-syntax-token)
           (token-sym->style short-sym->style-name)))
    
    (define (matches-language? l)
      (match l
        [(list _ "Datalog" _ ...) #t]
        [_ #f]))
    
    (define (delimiter-pair? x y)
      (and (char=? x #\() (char=? y #\))))
    
    ;; repl-submit? : drscheme:rep:text<%> nat -> boolean?
    (define (repl-submit? text prompt-position)
      (let loop ([i prompt-position]
                 [blank? #t]
                 [string-char #f]
                 [delimiter-stack null]
                 [closed? #f])
        (let ([c (send text get-character i)])
          (case c
            [(#\nul)
             (and closed?
                  (not blank?)
                  (not string-char)
                  (null? delimiter-stack))]
            [(#\. #\? #\~)
             (if string-char
                 (loop (add1 i) #f string-char delimiter-stack #f)
                 (loop (add1 i) #f #f delimiter-stack #t))]
            [(#\()
             (if string-char
                 (loop (add1 i) #f string-char delimiter-stack #f)
                 (loop (add1 i) #f #f (cons c delimiter-stack) #f))]
            [(#\))
             (cond
               [string-char
                (loop (add1 i) #f string-char delimiter-stack #f)]
               [(and (pair? delimiter-stack)
                     (delimiter-pair? (car delimiter-stack) c))
                (loop (add1 i) #f #f (cdr delimiter-stack) #f)]
               [else
                (loop (add1 i) #f #f delimiter-stack #f)])]
            [(#\")
             (cond
               [(and string-char (char=? c string-char))
                (loop (add1 i) #f #f delimiter-stack #f)]
               [string-char
                (loop (add1 i) #f string-char delimiter-stack #f)]
               [else
                (loop (add1 i) #f c delimiter-stack #f)])]
            [(#\\)
             (if string-char
                 (loop (+ i 2) #f string-char delimiter-stack #f)
                 (loop (add1 i) #f string-char delimiter-stack #f))]
            [else
             (loop (add1 i)
                   (and blank? (char-whitespace? c))
                   string-char
                   delimiter-stack
                   closed?)]))))
    
    (drscheme:modes:add-mode "Datalog mode" mode-surrogate repl-submit? matches-language?)
    (color-prefs:add-to-preferences-panel "Datalog" extend-preferences-panel)
    
    (for ([line color-prefs-table])
      (let ([sym (car line)]
            [color (cadr line)])
        (color-prefs:register-color-preference (short-sym->pref-name sym)
                                               (short-sym->style-name sym)
                                               color)))))