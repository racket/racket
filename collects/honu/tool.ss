(module tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "unitsig.ss")
           (lib "etc.ss")
           (lib "class.ss")
           "parsers/parse.ss"
           "ast.ss"
           "tenv.ss"
           "private/typechecker/honu-type-utils.ss"
           "compile.ss"
           "honu-compile-context.ss"
	   (lib "string-constant.ss" "string-constants"))

  (provide tool@)

  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) 
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) (honu-lang-mixin 'single))))
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) (honu-lang-mixin 'group)))))
      
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
          (define env (empty-env))
          (define level-parser
            (case level
              [(single) parse-port]
              [(group)  parse-group]))
          (define/public (front-end/complete-program port settings teachpack-cache)
            (set! tenv (empty-tenv))
            (let ([name (object-name port)])
              (lambda ()
                (if (eof-object? (peek-char-or-special port))
                    eof
                    (let* ([parsed (level-parser port name)]
                           [compiled-defns (compile/complete-program tenv parsed)])
                      (set! env (get-initial-env tenv))
                      (datum->syntax-object #f `(run-honu-full-program ,compiled-defns) #f))))))
          (define/public (front-end/interaction port settings teachpack-cache)
            (let ([name (object-name port)])
              (lambda ()
                (if (eof-object? (peek-char-or-special port))
                    eof
                    (let ([parsed (parse-interaction port name)])
                      (let-values ([(compiled-expr new-env)
                                    (compile/interaction tenv env parsed)])
                        (begin (set! env new-env)
                               (datum->syntax-object #f `(run-honu-interaction ,compiled-expr) #f))))))))
          (define/public (get-style-delta) #f)
          (define/public (get-language-position)
	    (list (string-constant experimental-languages)
		  "Honu"
                  (case level
                    [(single) "Single File"]
                    [(group)  "Group File"])))
          (define/public (order-manuals x) 
            (values 
             (list #"drscheme" #"tour" #"help")
             #f))
          (define/public (get-language-name)
            (case level
              [(single) "Honu (single)"]
              [(group)  "Honu (group)"]))
          (define/public (get-language-url) #f)
          (define/public (get-language-numbers)
            (case level
              [(single) (list 1000 10 1)]
              [(group)  (list 1000 10 2)]))
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
                    (with-handlers ([(lambda (x) #t) (lambda (x) (printf "~a~n" (exn-message x)))])
                      (lambda (exp)
                      (syntax-case exp (run-honu-full-program run-honu-interaction)
                        [(run-honu-full-program defns)
                         (let loop ([defns (syntax->list #'defns)])
                           (if (null? defns)
                               (void)
                               (begin (old-current-eval (syntax-as-top (car defns)))
                                      (loop (cdr defns)))))]
                        [(run-honu-interaction ex)
                         (old-current-eval (syntax-as-top #'ex))]
                        [(_) (old-current-eval exp)])))))
                 (with-handlers ([(lambda (x) #t) (lambda (x) (printf "~a~n" (exn-message x)))])
                   (namespace-attach-module n path)
                   (namespace-require path))))))
          (define/public (render-value value settings port) (write value port))
          (define/public (render-value/format value settings port width) (write value port))
          (define/public (unmarshall-settings x) x)
	  (define/public (create-executable settings parent src-file teachpacks)
	    (message-box "Unsupported"
			 "Sorry - executables are not supported for Honu at this time"
			 parent))
	  (define/public (get-one-line-summary)
            (case level
              [(single) "Honu (also not Scheme at all!)"]
              [(group)  "List of Honu files to run together"]))
          
          (super-instantiate ())))
      
      ;; The following copies the Java mode to make one for Honu, but it's better right now than using
      ;; the Scheme mode.  Ugh.
      
      ;; matches-language : (union #f (listof string)) -> boolean
      (define (matches-language l)
        (and l (pair? l) (pair? (cdr l)) (equal? (cadr l) "Honu")))

      ;Create the Honu editing mode
      (define mode-surrogate
        (new color:text-mode%
             (matches (list (list '|{| '|}|)
                            (list '|(| '|)|)
                            (list '|[| '|]|)))))
            
      ;repl-submit: text int -> bool
      ;Determines if the reple should submit or not
      (define (repl-submit text prompt-position)
        (let ((is-if? #f)
              (is-string? #f)
              (open-parens 0)
              (open-braces 0)
              (open-curlies 0))
          (let loop ((index 1) (char (send text get-character prompt-position)))
            (unless (eq? char #\nul)
              (cond 
                ;beginning of if statement
                ((and (= index 1) 
                      (eq? char #\i) 
                      (eq? (send text get-character (add1 prompt-position)) #\f)
                      (eq? (send text get-character (+ 2 prompt-position)) #\space))
                 (set! is-if? #t)
                 (loop 3 (send text get-character (+ 3 prompt-position))))
                ((eq? char #\()
                 (unless is-string? (set! open-parens (add1 open-parens)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\))
                 (unless is-string? (set! open-parens (sub1 open-parens)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\{)
                 (unless is-string? (set! open-curlies (add1 open-curlies)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\})
                 (unless is-string? (set! open-curlies (sub1 open-curlies)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\[)
                 (unless is-string? (set! open-braces (add1 open-braces)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ((eq? char #\])
                 (unless is-string? (set! open-braces (sub1 open-braces)))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                ;beginning of string
                ((eq? char #\")
                 (set! is-string? (not is-string?))
                 (loop (add1 index) (send text get-character (+ index prompt-position))))
                (else
                 (loop (add1 index) (send text get-character (+ index prompt-position)))))))
          (not (or (not (= open-parens 0))
                   (not (= open-braces 0))
                   (not (= open-curlies 0))
                   is-if?))))
      
      (drscheme:modes:add-mode "Honu mode" mode-surrogate repl-submit matches-language)
      )))
