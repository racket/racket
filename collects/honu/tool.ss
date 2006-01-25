(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "class.ss")
           (lib "list.ss" "srfi" "1")
           (lib "match.ss")
           (lib "port.ss")
           "parsers/lex.ss"
           "parsers/parse.ss"
           "private/typechecker/type-utils.ss"
           (only "base.ss" null%)
           "tenv.ss"
           "compile.ss"
           "format.ss"
	   (lib "string-constant.ss" "string-constants"))

  (provide tool@)

  ;; tool@ : Unit/Sig[drscheme:tool^ -> drscheme:tool-exports^]
  ;; Implements Honu as a DrScheme language
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)

      ;; phase1 : -> Void
      ;; Performs general language extensions to DrScheme for Honu.
      ;; Currently none exist.
      (define (phase1) (void))

      ;; phase2 : -> Void
      ;; Adds the Honu language to DrScheme.
      (define (phase2)
        (define honu (new ((drscheme:language:get-default-mixin) honu-lang%)))
        (drscheme:language-configuration:add-language honu)
        (send honu pre-initialize))

      ;; honu-lang% : Class[drscheme:language:language<%>]
      ;; Honu implementation as a class.
      (define honu-lang%
        (class* object% (drscheme:language:language<%>)

          ;; tenv : TypeEnvironment
          ;; The current type environment for evaluation.
          (define tenv #f)

          ;; lenv : LexicalEnvironment
          ;; The current lexical environment for evaluation.
          (define lenv #f)

          ;; pre-initialize : -> Void
          ;; Hooks Honu up to DrScheme after being added as a language.
          (define/public (pre-initialize)
            (drscheme:modes:add-mode "Honu mode" mode-surrogate repl-submit matches-language)
            (color-prefs:add-to-preferences-panel "Honu" extend-preferences-panel)
            (for-each register-color-pref color-prefs-table))
          
          ;; first-opened : -> Void
          ;; Sets Honu state to initial values.
          (define/public (first-opened)
            (reset-evaluation!))

          ;; get-comment-character : -> String Character
          ;; Provides prefix and filler character for comments
          ;; for use in "Insert Large Letters"
          (define/public (get-comment-character) (values "//" #\*))

          ;; default-settings : -> HonuSetting
          ;; Provides default global Honu configuration
          (define/public (default-settings) #f)

          ;; default-settings? : HonuSetting -> Boolean
          ;; Reports whether Honu configuration is set to defaults
          (define/public (default-settings? s) #t)

          ;; marshall-settings : HonuSetting -> Writable
          ;; Converts a Honu configuration to a value which can be
          ;; written to a port.
          (define/public (marshall-settings s) s)

          ;; unmarshall-settings : Writable -> HonuSetting
          ;; Converts the result of a previous marshall-settings to
          ;; a Honu configuration.
          (define/public (unmarshall-settings s) s)

          ;; config-panel : panel% -> [Case (-> HonuSetting) (HonuSetting -> Void)]
          ;; Assembles a language configuration dialog for Honu
          ;; and produces a get/set function for the displayed configuration.
          (define/public (config-panel parent)
            (letrec ([output-panel (new group-box-panel%
                                     [label "Honu Preferences (Currently Empty)"]
                                     [parent parent]
                                     [alignment '(left center)])])
              (case-lambda
                [() (default-settings)]
                [(settings) (void)])))

          ;; front-end/complete-program :
          ;; InputPort HonuSetting TeachpackCache -> (-> (Union Syntax EOF))
          ;; Produces a thunk which compiles and returns a Honu definition when one
          ;; is available on the input port, or EOF when none are left.
          (define/public (front-end/complete-program original-port settings teachpack-cache)
            (define port (single-stream-port original-port))
            (reset-evaluation!)
            (lambda ()
              (if (eof-object? (peek-char-or-special port))
                  eof
                  (let*-values
                      ([(syntax-annotation compiled-defns)
                        (compile/defns tenv lenv (parse-port port (object-name port)))])
                    ;; This particular syntax construction is compatible with Check Syntax
                    ;; and can be distinguished from compiled Interactions.
                    (datum->syntax-object
                     #f
                     (list 'begin
                           syntax-annotation
                           (datum->syntax-object
                            #f
                            (cons 'begin compiled-defns)
                            #f))
                     #f)))))

          ;; front-end/interaction :
          ;; InputPort HonuSetting TeachpackCache -> (-> (Union Syntax EOF))
          ;; Produces a thunk which compiles and returns a Honu expression or definition
          ;; when one is available on the input port, or EOF when none are left.
          (define/public (front-end/interaction original-port settings teachpack-cache)
            (define port (single-stream-port original-port))
            (lambda ()
              (if (eof-object? (peek-char-or-special port))
                  eof
                  (let*-values ([(compiled-expr type)
                                 (compile/interaction
                                  tenv lenv
                                  (parse-interaction port (object-name port)))])
                    (datum->syntax-object
                     #f
                     (if type
                         `(compiled-expression ,compiled-expr ,type)
                         `(compiled-binding ,compiled-expr))
                     #f)))))

          ;; get-style-delta : -> #f
          ;; Reports that the name Honu has no specific text style.
          (define/public (get-style-delta) #f)

          ;; order-manuals : [Listof ByteString] -> (values [Listof ByteString] Boolean)
          ;; Reports which manuals from the input contain Honu documentation
          ;; and whether to search doc.txt files for Honu documentation.
          ;; Currently lists no manuals, but includes doc.txt.
          (define/public (order-manuals manuals)
            (values '() #t))

          ;; get-language-name : -> String
          ;; Produces Honu's name.
          (define/public (get-language-name) "Honu")

          ;; get-language-url : -> (Union String #f)
          ;; Reports that Honu has no URL.
          (define/public (get-language-url) #f)

          ;; get-language-position : -> [NonEmptyListof String]
          ;; Reports Honu's place in the language hierarchy.
          (define/public (get-language-position)
	    (list (string-constant experimental-languages)
		  "Honu"))

          ;; get-language-numbers : -> [NonEmptyListof String]
          ;; Reports Honu's sort order in the language hierarchy.
          (define/public (get-language-numbers) (list 1000 10))

          ;; get-one-line-summary : -> String
          ;; Produces a short description of Honu.
	  (define/public (get-one-line-summary) "Honu (not a Scheme dialect)")

          ;; on-execute : HonuSetting ((-> Void) -> Void) -> Void
          ;; Sets parameters for Honu execution.
          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require '(lib "base.ss" "honu") #f)
            (let ([path ((current-module-name-resolver) '(lib "base.ss" "honu") #f #f)]
                  [namespace (current-namespace)])
              (run-in-user-thread
               (lambda ()
                 (define base-eval (drscheme:debug:make-debug-eval-handler (current-eval)))
                 (define (eval stx)
                   (syntax-case stx (compiled-binding compiled-expression)
                     [(compiled-binding BINDING)
                      (base-eval (namespace-syntax-introduce #'BINDING))]
                     [(compiled-expression EXPR TYPE)
                      (cons (base-eval (namespace-syntax-introduce #'EXPR))
                            (syntax-e #'TYPE))]
                     [(_ ANNOTATION PROGRAM)
                      (base-eval (namespace-syntax-introduce #'PROGRAM))]))
                 
                 (current-eval eval)
		 (error-display-handler 
		  (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
                 (namespace-attach-module namespace path)
                 (namespace-require path)))))

          ;; render-value : Value HonuSetting OutputPort -> Void
          ;; Writes value to port as a single line with no newline.
          (define/public (render-value value settings port) 
            (display (format-honu value settings) port))

          ;; render-value/format : Value HonuSetting OutputPort (Union Integer #f) -> Void
          ;; Writes value to port as lines of length no greater than width.
          ;; Terminates all lines with newline.
          ;; Currently fails to actually account for width.
          (define/public (render-value/format value settings port width)
            (render-value value settings port)
            (newline port))

          ;; create-executable : HonuSetting (Union Dialog Frame) String TeachpackCache -> Void
          ;; Raises an error reporting that Honu programs cannot be made into executables.
	  (define/public (create-executable settings parent src-file teachpacks)
	    (message-box "Unsupported"
			 "Sorry - executables are not supported for Honu at this time"
			 parent))

          ;; Finish the class instantiation
          (super-new)

          ;; ------------------------------------------------------------
          ;; BEGIN PRIVATE FUNCTIONS
          
          ;; reset-evaluation! : -> Void
          ;; Restore Honu state to initial values.
          (define (reset-evaluation!)
            (set! tenv (empty-tenv))
            (set! lenv (get-builtin-lenv)))

          ;; format-honu : (cons Value Ast:Type) HonuSetting -> String
          ;; Formats the result of Honu evaluation for printing.
          (define (format-honu result settings)
            (format "~a : ~a"
                    (honu-value->string (car result))
                    (honu-type->string (cdr result))))

          ;; matches-language : [NonEmptyListof String] -> Boolean
          ;; Reports whether a language dialog choice matches Honu.
          (define (matches-language l)
            (equal? l (get-language-position)))

          ;; register-color-pref : (list Symbol Color String) -> Void
          ;; Registers a single color preference setting in the correct menu.
          (define (register-color-pref pref)
            (let ([sym (car pref)]
                  [color (cadr pref)])
              (color-prefs:register-color-pref (short-sym->pref-name sym)
                                               (short-sym->style-name sym)
                                               color)))

          ;; color-prefs-table : [Listof (list Symbol Color String)]
          ;; Lists the Honu color preference entries
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

          ;; mode-surrogate : TextMode
          ;; Create the Honu editing mode
          (define mode-surrogate
            (new color:text-mode%
                 (matches (list (list '|{| '|}|)
                                (list '|(| '|)|)
                                (list '|[| '|]|)))
                 (get-token get-syntax-token)
                 (token-sym->style short-sym->style-name)))
            
          ;; repl-submit: text int -> bool
          ;; Determines if the reple should submit or not
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
                   ;; beginning of string
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

          ;; single-stream-port : InputPort -> InputPort
          ;; Consumes an arbitrary input port.
          ;; Produces a port which produces the same data as its input
          ;; up to EOF, then produces EOF constantly.
          (define (single-stream-port port)
            (let*-values ([(in-port out-port)
                           (make-pipe #f (object-name port) (object-name port))])
              (copy-port port out-port)
              (close-output-port out-port)
              in-port))
          
          )))))
