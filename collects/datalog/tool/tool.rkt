#lang racket
(require racket/gui/base
         framework
         drracket/tool
         racket/match
         racket/unit
         racket/class
         string-constants
         "syntax-color.rkt"
         "../private/compiler.rkt"
         "../parse.rkt"
         "../pretty.rkt"
         "../eval.rkt"
         "../runtime.rkt"
         (only-in (planet dherman/pprint:4)
                  pretty-print)
         (for-template "../lang/lang.rkt"))
(provide tool@)

(define tool@
  (unit 
    (import drracket:tool^)
    (export drracket:tool-exports^)
    
    (define (phase1) (void))    
    (define (phase2) 
      (drracket:language-configuration:add-language
       (make-object ((drracket:language:get-default-mixin)
                     (datalog-lang-mixin)))))
    
    (define (datalog-lang-mixin)
      (class* object% (drracket:language:language<%>)
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
            [(drracket:check-syntax-button) #t]
            [(drracket:language-menu-title) "Datalog"]
            [(drracket:define-popup) #f]
            [(drracket:special:insert-fraction) #f]
            [(drracket:special:insert-lambda) #f]
            [(drracket:special:insert-large-letters) #t]
            [(drracket:special:insert-image) #f]
            [(drracket:special:insert-comment-box) #f]
            [(drracket:special:insert-gui-tool) #f]
            [(drracket:special:slideshow-menu-item) #f]
            [(drracket:special:insert-text-box) #f]
            [(drracket:special:xml-menus) #f]
            [else (drracket:language:get-capability-default capability)]))
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
          (let ([module-forms `(planet ,(this-package-version-symbol drracket/module-forms))]
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
                 (let ([previous-eval (drracket:debug:make-debug-eval-handler (current-eval))])
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
    (define (short-sym->style-name sym) (format "datalog:syntax-colors:racket:~a" sym))
    
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
    
    ;; repl-submit? : drracket:rep:text<%> nat -> boolean?
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
    
    (drracket:modes:add-mode "Datalog mode" mode-surrogate repl-submit? matches-language?)
    (color-prefs:add-to-preferences-panel "Datalog" extend-preferences-panel)
    
    (for ([line color-prefs-table])
      (let ([sym (car line)]
            [color (cadr line)])
        (color-prefs:register-color-preference (short-sym->pref-name sym)
                                               (short-sym->style-name sym)
                                               color)))))