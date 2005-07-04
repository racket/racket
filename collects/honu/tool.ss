(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "class.ss")
           (lib "list.ss" "srfi" "1")
           "parsers/lex.ss"
           "parsers/parse.ss"
           (only "base.ss" null%)
           "tenv.ss"
           "compile.ss"
	   (lib "string-constant.ss" "string-constants"))

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) 
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) (honu-lang-mixin 'normal)))))
      
      (define (honu-lang-mixin level)
        (class* object% (drscheme:language:language<%>)
          (define/public (config-panel parent)
            (case-lambda
              [() null]
              [(x) (void)]))
          (define/public (get-comment-character) (values "//" #\*))
          (define/public (default-settings) null)
          (define/public (default-settings? x) #t)
          (define tenv (empty-tenv))
          (define lenv (get-builtin-lenv))
          (define level-parser
            (case level
              [(normal) parse-port]))
          (define/public (front-end/complete-program port settings teachpack-cache)
            (set! tenv (empty-tenv))
            (set! lenv (get-builtin-lenv))
            (let ([name (object-name port)])
              (lambda ()
                (if (eof-object? (peek-char-or-special port))
                    eof
                    (let* ([parsed (level-parser port name)])
                      (let ([compiled-defns (compile/defns tenv lenv parsed)])
                        (datum->syntax-object #f (cons 'begin compiled-defns) #f)))))))
          (define/public (front-end/interaction port settings teachpack-cache)
            (let ([name (object-name port)])
              (lambda ()
                (if (eof-object? (peek-char-or-special port))
                    eof
                    (let ([parsed (parse-interaction port name)])
                      (let ([compiled-expr (compile/interaction tenv lenv parsed)])
                        (datum->syntax-object #f compiled-expr #f)))))))
          (define/public (get-style-delta) #f)
          (define/public (get-language-position)
	    (list (string-constant experimental-languages)
		  "Honu"))
          (define/public (order-manuals x) 
            (values 
             (list #"drscheme" #"tour" #"help")
             #f))
          (define/public (get-language-name)
            (case level
              [(normal) "Honu"]))
          (define/public (get-language-url) #f)
          (define/public (get-language-numbers)
            (case level
              [(normal) (list 1000 10)]))
          (define/public (get-teachpack-names) null)
          (define/public (marshall-settings x) x)
          (define/private (syntax-as-top s)
            (if (syntax? s) (namespace-syntax-introduce s) s))
          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require '(lib "base.ss" "honu") #f)
            (let ([path ((current-module-name-resolver) '(lib "base.ss" "honu") #f #f)]
                  [n (current-namespace)])
              (run-in-user-thread
               (lambda ()
		 (error-display-handler 
		  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
		 (let ([old-current-eval (drscheme:debug:make-debug-eval-handler (current-eval))])
                   (current-eval
                    (lambda (exp)
                      (old-current-eval (syntax-as-top exp)))))
                 (namespace-attach-module n path)
                 (namespace-require path)))))
          (define/public (render-value value settings port) (display (format-honu value settings 0) port))
          (define/public (render-value/format value settings port width) (render-value value settings port) (if (not (null? value)) (newline port)))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable settings parent src-file teachpacks)
	    (message-box "Unsupported"
			 "Sorry - executables are not supported for Honu at this time"
			 parent))
	  (define/public (get-one-line-summary)
            (case level
              [(normal) "Honu (also not Scheme at all!)"]))
          
          (super-instantiate ())))
      
      ;; The following copies the Java mode to make one for Honu, but it's better right now than using
      ;; the Scheme mode.  Ugh.
      
      ;; matches-language : (union #f (listof string)) -> boolean
      (define (matches-language l)
        (and l (pair? l) (pair? (cdr l)) (equal? (cadr l) "Honu")))

      (define (format-honu value settings indent)
        (cond
          [(number? value)    (format "~a" value)]
          [(char? value)      (format "'~a'" value)]
          [(string? value)    (format "~v" value)]
          [(boolean? value)   (if value "true" "false")]
          [(procedure? value) "procedure"]
          ;; tuples -- first the zero tuple, then the non-empty tuples
          ;;
          ;; if you want void values to be printed out, uncomment
          ;; the following:
          ;; [(null? value)      "()"]
          [(null? value)
           ;; the following makes it so that nothing is printed out
           ;; for a void value, but if a zero-tuple is part of a tuple
           ;; or structure, then it is printed out.
           (if (= indent 0) "" "()")]
          [(list? value)
           (if (any (lambda (v)
                      ;; checking to see if it's a non-null object
                      (and (object? v) (not (is-a? v null%))))
                    value)
               (string-append "("
                              (fold (lambda (v s)
                                      ;; if there are objects in the list, then we'll
                                      ;; print each value on its own line.
                                      (string-append s ",\n" (make-string (+ indent 1) #\space)
                                                     (format-honu v settings (+ indent 1))))
                                    (format-honu (car value) settings (+ indent 1))
                                    (cdr value))
                              ")")
               (string-append "("
                              (fold (lambda (v s)
                                      ;; if there are no objects, then we'll just print out
                                      ;; the list on the same line.
                                      (string-append s ", " (format-honu v settings (+ indent 1))))
                                    (format-honu (car value) settings (+ indent 1))
                                    (cdr value))
                              ")"))]
          [(is-a? value null%) "null"]
          [(object? value) (send value format-class 
                                 (lambda (value at-top?)
                                   (format-honu value settings at-top?))
                                 indent)]
          [else (format "~a" value)]))
                                   
      
      ;Set the Honu editing colors
      (define color-prefs-table
        `((keyword ,(make-object color% "black")       "keyword")
          (parenthesis ,(make-object color% 132 60 36) "parenthesis")
          (string ,(make-object color% "forestgreen")  "string")
          (literal ,(make-object color% "forestgreen") "literal")
          (comment ,(make-object color% 194 116 31)    "comment")
          (error ,(make-object color% "red")           "error")
          (identifier ,(make-object color% 38 38 128)  "identifer")
          (default ,(make-object color% "black")       "default")))
      
      ;; short-sym->pref-name : symbol -> symbol
      ;; returns the preference name for the color prefs
      (define (short-sym->pref-name sym) (string->symbol (short-sym->style-name sym)))
      
      ;; short-sym->style-name : symbol->string
      ;; converts the short name (from the table above) into a name in the editor list
      ;; (they are added in by `color-prefs:register-color-pref', called below)
      (define (short-sym->style-name sym) (format "honu:syntax-coloring:scheme:~a" sym))
      
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
      
      ;Create the Honu editing mode
      (define mode-surrogate
        (new color:text-mode%
             (matches (list (list '|{| '|}|)
                            (list '|(| '|)|)
                            (list '|[| '|]|)))
             (get-token get-syntax-token)
             (token-sym->style short-sym->style-name)))
            
      ;repl-submit: text int -> bool
      ;Determines if the reple should submit or not
      (define (repl-submit text prompt-position)
        (let ((is-empty? #t)
              (is-string? #f)
              (open-parens 0)
              (open-braces 0)
              (open-curlies 0))
          (let loop ((index 1) (char (send text get-character prompt-position)))
            (unless (eq? char #\nul)
              (cond 
                ((eq? char #\()
                 (set! is-empty? #f)
                 (unless is-string? (set! open-parens (add1 open-parens)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\))
                 (set! is-empty? #f)
                 (unless is-string? (set! open-parens (sub1 open-parens)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\{)
                 (set! is-empty? #f)
                 (unless is-string? (set! open-curlies (add1 open-curlies)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\})
                 (set! is-empty? #f)
                 (unless is-string? (set! open-curlies (sub1 open-curlies)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\[)
                 (set! is-empty? #f)
                 (unless is-string? (set! open-braces (add1 open-braces)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\])
                 (set! is-empty? #f)
                 (unless is-string? (set! open-braces (sub1 open-braces)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ;beginning of string
                ((eq? char #\")
                 (set! is-empty? #f)
                 (set! is-string? (not is-string?))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((char-whitespace? char)
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                (else
                 (set! is-empty? #f)
                 (loop (add1 index) (send text get-character (+ index prompt-position)))))))
          (not (or (not (= open-parens 0))
                   (not (= open-braces 0))
                   (not (= open-curlies 0))
                   is-empty?))))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  Wire up to DrScheme
      ;;
      
      (drscheme:modes:add-mode "Honu mode" mode-surrogate repl-submit matches-language)
      (color-prefs:add-to-preferences-panel "Honu" extend-preferences-panel)
      
      (for-each (lambda (line)
                  (let ([sym (car line)]
                        [color (cadr line)])
                    (color-prefs:register-color-pref (short-sym->pref-name sym)
                                                     (short-sym->style-name sym)
                                                     color)))
                color-prefs-table)      
      )))
