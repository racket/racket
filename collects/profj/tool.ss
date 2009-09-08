(module tool scheme/base
  (require drscheme/tool scheme/contract
           mred framework
           errortrace/errortrace-lib
           (prefix-in u: scheme/unit)
           scheme/file
           scheme/runtime-path
           scheme/class
	   string-constants)
  (require "parameters.ss" 
           "parsers/lexer.ss"
           (lib "test-engine/test-engine.scm")
           "java-tests.scm"
           (lib "test-engine/test-coverage.scm")
           (except-in "ast.ss" for)
           "display-java.ss")

  (require (for-syntax scheme/base))
  
  (define drs-ns (current-namespace))
  (define-syntax (dr stx)
    (syntax-case stx ()
      [(_ fn id ...)
       #'(begin
           (define-runtime-path the-file fn)
           (define (id . x)
             (let ([orig-fn (parameterize ([current-namespace drs-ns])
                              (dynamic-require the-file 'id))])
               (apply orig-fn x)))
           ...)]))
    
  (dr "compile.ss"
      compile-java compile-interactions compile-files compile-ast compile-interactions-ast
      compilation-unit-code compilation-unit-contains set-compilation-unit-code!
      read-record write-record
      set-syntax-location create-type-record
      compile-to-ast)

  (dr "parser.ss"
      parse parse-interactions parse-expression parse-type parse-name)
  
  (provide tool@)

  ;Set the default classpath
  (preferences:set-default 'profj:classpath null (lambda (v) (and (list? v) (andmap string? v))))
  
  ;; comment gif
  (define-runtime-path comment-gif-path '(lib "icons/slash-slash.gif"))
  (define comment-gif #f)
  (define (get-comment-gif)
    (unless comment-gif
      (set! comment-gif (make-object bitmap% comment-gif-path)))
    comment-gif)
  
  ;;Java interactions box
  (define-runtime-path ji-gif-path '(lib "icons/j.gif"))
  (define ji-gif #f)
  (define (get-ji-gif)
    (unless ji-gif
      (set! ji-gif (make-object bitmap% ji-gif-path)))
    ji-gif)

  
  (define tool@
    (u:unit
      (u:import drscheme:tool^)
      (u:export drscheme:tool-exports^)
      ;Set the Java editing colors
      (define color-prefs-table
        `((keyword ,(make-object color% "black") ,(string-constant profj-java-mode-color-keyword))
          (prim-type ,(make-object color% "darkmagenta")
                     ,(string-constant profj-java-mode-color-prim-type))
          (identifier ,(make-object color% 38 38 128) ,(string-constant profj-java-mode-color-identifier))
          (string ,(make-object color% "forestgreen") ,(string-constant profj-java-mode-color-string))
          (literal ,(make-object color% "forestgreen") ,(string-constant profj-java-mode-color-literal))
          (comment ,(make-object color% 194 116 31) ,(string-constant profj-java-mode-color-comment))
          (error ,(make-object color% "red") ,(string-constant profj-java-mode-color-error))
          (default ,(make-object color% "black") ,(string-constant profj-java-mode-color-default))))
      (define colors-table
        (cons `(block-comment ,(make-object color% 194 116 31) ,(string-constant profj-java-mode-color-comment))
              color-prefs-table))
      
      ;Set the Java coverage colors
      (define coverage-color-prefs
        `((uncovered ,(make-object color% "black") ,(string-constant profj-java-mode-color-default))
          (covered ,(make-object color% "darkmagenta") ,(string-constant profj-coverage-color-covered))))
      
      ;; short-sym->pref-name : symbol -> symbol
      ;; returns the preference name for the color prefs
      (define (short-sym->pref-name sym) (string->symbol (short-sym->style-name sym)))
      
      ;; short-sym->style-name : symbol->string
      ;; converts the short name (from the table above) into a name in the editor list
      ;; (they are added in by `color-prefs:register-color-preference', called below)
      (define (short-sym->style-name sym) (format "profj:syntax-colors:scheme:~a" sym))
      
      ;; extend-preferences-panel : vertical-panel -> void
      ;; adds in the configuration for the Java colors to the prefs panel
      (define ((extend-preferences-panel color-table) parent)
        (let ((put 
               (lambda (line)
                 (let ([sym (car line)]
                       [str (caddr line)])
                   (color-prefs:build-color-selection-panel 
                    parent
                    (short-sym->pref-name sym)
                    (short-sym->style-name sym)
                    str)))))
          (for-each put color-table)))
      
      (define mode-surrogate% 
        (class color:text-mode%
          
          (define/override (put-file text sup directory default-name)
            (parameterize ([finder:default-extension "java"]
                           [finder:default-filters '(("Any" "*.*"))])
              ;; don't call the surrogate's super, since it sets the default extension
              (sup directory default-name)))
          
          (define/override (on-disable-surrogate text)
            (keymap:remove-chained-keymap text java-keymap)
            (super on-disable-surrogate text))
          
          (define/override (on-enable-surrogate text)
            (super on-enable-surrogate text)
            (send (send text get-keymap) chain-to-keymap java-keymap #t))
          (super-new)))
      
      ;Create the Java editing mode
      (define mode-surrogate
        (new mode-surrogate%
             (matches (list (list '|{| '|}|)
                            (list '|(| '|)|)
                            (list '|[| '|]|)))
             (get-token get-syntax-token)
             (token-sym->style short-sym->style-name)))

      (define java-keymap (new keymap:aug-keymap%))
      (send java-keymap add-function "do-return" (λ (edit event) (send edit do-return)))
      (send java-keymap map-function "return" "do-return")
      (send java-keymap map-function "s:return" "do-return")
      (send java-keymap map-function "s:c:return" "do-return")
      (send java-keymap map-function "a:return" "do-return")
      (send java-keymap map-function "s:a:return" "do-return")
      (send java-keymap map-function "c:a:return" "do-return")
      (send java-keymap map-function "c:s:a:return" "do-return")
      (send java-keymap map-function "c:return" "do-return")
      (send java-keymap map-function "d:return" "do-return")
      (keymap:send-map-function-meta java-keymap "return" "do-return")
      (keymap:send-map-function-meta java-keymap "s:return" "do-return")
      (keymap:send-map-function-meta java-keymap "s:c:return" "do-return")
      (keymap:send-map-function-meta java-keymap "a:return" "do-return")
      (keymap:send-map-function-meta java-keymap "s:a:return" "do-return")
      (keymap:send-map-function-meta java-keymap "c:a:return" "do-return")
      (keymap:send-map-function-meta java-keymap "c:s:a:return" "do-return")
      (keymap:send-map-function-meta java-keymap "c:return" "do-return")
    
      (send java-keymap add-function "tabify-at-caret" (λ (edit event) (send edit java-tabify-selection)))
      (send java-keymap map-function "TAB" "tabify-at-caret")
      
      (send java-keymap add-function "insert-{" (lambda (edit event) (send edit open-brace)))
      (send java-keymap map-function "{" "insert-{")
      (keymap:send-map-function-meta java-keymap "{" "insert-{")
      
      (send java-keymap add-function "insert-(" (lambda (edit event) (send edit open-par)))
      (send java-keymap map-function "(" "insert-(")
      (keymap:send-map-function-meta java-keymap "(" "insert-(")
      
      (send java-keymap add-function "insert-[" (lambda (edit event) (send edit open-brack)))
      (send java-keymap map-function "[" "insert-[")
      (keymap:send-map-function-meta java-keymap "[" "insert-[")

      
      (define indent-mixin
        (mixin (color:text<%> editor:keymap<%>) ()
          (inherit insert classify-position set-position
                   get-start-position get-end-position get-character delete
                   backward-match backward-containing-sexp
                   find-string position-paragraph paragraph-start-position
                   begin-edit-sequence end-edit-sequence
                   is-stopped? is-frozen?
                   skip-whitespace forward-match)
          
          (define single-tab-stop 2)
          (define eol "\n")
          
          ;Returns the position immediately following the nearest open, or the start of the buffer
          ;In some cases of mismatched parens, returns false
          (define/private (get-sexp-start pos)
            (let ([sexp-start+whitespace (backward-containing-sexp pos 0)])
              (and sexp-start+whitespace
                   (skip-whitespace sexp-start+whitespace 'backward #t))))
          
          ;Returns whether a block comment just ended when at the end of the buffer
          (define/private (blockcomment-end? pos)
            (and (eq? (classify-position pos) 'block-comment)
                 (let ([close (find-string "*/" 'backward pos 0 #f)])
                   (and close (= pos (+ 2 close))))))
          
          (define/private (get-indentation start-pos)
            (letrec ([last-offset
                      (lambda (previous-line last-line-start)
                        (max (sub1 (if (> last-line-start start-pos)
                                       (- start-pos previous-line)
                                       (- last-line-start previous-line)))
                             0))]
                     [blockcomment-open
                      (lambda (pos)
                        (let loop ([open-pos (find-string "/*" 'backward pos 0 #f)])
                          (cond
                            [(or (not open-pos) (zero? open-pos)) #f]
                            [(eq? (classify-position (sub1 open-pos)) 'block-comment)
                             (loop (find-string "/*" 'backward open-pos 0 #f))]
                            [else open-pos])))]
                     [sensitive-indent
                      (lambda (last-line-indent last-line-start opener open-at)
                        #;(printf "sensitive-indent opener ~a~n" opener)
                        #;(printf "previous open at ~a~n" (get-sexp-start (sub1 open-at)))
                        (cond
                          [(equal? opener #\{)
                           (let* ([previous-open (get-sexp-start (sub1 open-at))]
                                  [brace? (and previous-open 
                                               (> (sub1 previous-open) 0)
                                               (equal? #\{ (get-character (sub1 previous-open))))]
                                  [base-line-text (and brace? (skip-whitespace (add1 previous-open) 'forward #f))]
                                  [base-line-start (and base-line-text (find-string eol 'backward base-line-text 0 #f))])
                             #;(printf "brace? ~a blt ~a bls ~a~n" brace? base-line-text base-line-start)
                             (cond
                               [base-line-start (+ single-tab-stop
                                                   (max 0 (sub1 (- base-line-text base-line-start))))]
                               [brace? (+ single-tab-stop 0)]
                               [else (+ single-tab-stop last-line-indent)]))]
                          [(equal? opener #\()
                           (+ (max 0
                                   (- open-at last-line-start))
                              last-line-indent)]
                          [(equal? opener #\[)
                           (+ (max 0
                                   (- open-at
                                      last-line-start))
                              last-line-indent)]
                          [else last-line-indent]))]                           
                     [indent
                      (if (or (is-stopped?) (is-frozen?))
                          0
                          (let* ([base-offset 0]
                                 [curr-open (get-sexp-start start-pos)])
                            #;(printf "indent starts at ~a open is ~a~n" start-pos curr-open)
                            (cond 
                              [(and (eq? (classify-position start-pos) 'block-comment)
                                         (not (blockcomment-end? start-pos)))
                               (let* ([comment-open (blockcomment-open start-pos)]
                                      [comment-line-start (and comment-open
                                                               (find-string eol 'backward comment-open 0 #f))])
                                 (+ single-tab-stop
                                    (cond
                                      [(not comment-line-start) base-offset]
                                      [else (max 0
                                                 (sub1 (- comment-open comment-line-start)))])))]
                              [(or (not curr-open) (= curr-open 0)) base-offset]
                              [else
                               (let ([previous-line (find-string eol 'backward start-pos 0 #f)])
                                 (cond 
                                   [(not previous-line) (+ base-offset single-tab-stop)]
                                   [(eq? (classify-position previous-line) 'block-comment)
                                    (let* ([comment-open (blockcomment-open previous-line)]
                                           [comment-line-start (and comment-open
                                                                    (find-string eol 'backward comment-open 0 #f))])
                                      (cond
                                        [(not comment-line-start) base-offset]
                                        [else (max 0
                                                   (sub1 (- comment-open comment-line-start)))]))]
                                   [(or (eq? (classify-position previous-line) 'comment)
                                        (eq? (classify-position previous-line) 'block-comment))
                                    (let* ([last-line-start (skip-whitespace (add1 previous-line) 'forward #f)]
                                           [last-line-indent (last-offset previous-line last-line-start)]
                                           [old-open (get-sexp-start last-line-start)])
                                      #;(printf "lls ~a lli ~a oo ~a~n" last-line-start last-line-indent old-open)
                                      (cond
                                        [(not old-open) last-line-indent]
                                        [(and old-open (<= curr-open old-open)) last-line-indent]
                                        [(< (sub1 curr-open) 0) base-offset]
                                        [else 
                                         (sensitive-indent last-line-indent last-line-start (get-character (sub1 curr-open)) curr-open)
                                         #;(+ single-tab-stop last-line-indent)]))]
                                   [else
                                    (let* ([last-line-start (skip-whitespace previous-line 'forward #f)]
                                           [last-line-indent (last-offset previous-line last-line-start)]
                                           [old-open (get-sexp-start last-line-start)])
                                      #;(printf "lls ~a lli ~a oo~a ~n" last-line-start last-line-indent old-open)
                                      (cond
                                        [(not old-open) last-line-indent]
                                        [(and old-open (<= curr-open old-open)) last-line-indent]
                                        [(< (sub1 curr-open) 0) base-offset]
                                        [else 
                                         (sensitive-indent last-line-indent last-line-start (get-character (sub1 curr-open)) curr-open)
                                         #;(+ single-tab-stop last-line-indent)]))]))])))])
              (make-string (max indent 0) #\space)))
          
          (define/public (do-return)
            (let ([start-pos (get-start-position)]
                  [end-pos (get-end-position)])
              (if (= start-pos end-pos)
                  (insert (string-append "\n" (get-indentation start-pos)))
                  (insert "\n"))))
          
          (define/public (open-brace)
            (let* ([start-pos (get-start-position)]
                   [end-pos (get-end-position)]
                   [cur-class (classify-position start-pos)])
              (cond 
                [(and (= start-pos end-pos) 
                      (or (and (eq? cur-class 'block-comment) (blockcomment-end? start-pos))
                          (not (memq cur-class '(comment string error block-comment)))))
                 (insert "{")
                 (insert (string-append "\n" 
                                        (let ([indent (get-indentation (add1 start-pos))])
                                          (substring indent 0 (max 0 (- (string-length indent) single-tab-stop))))
                                        "}"))
                 (set-position (add1 start-pos))
                 ]
                [else (insert "{")])))
          
          (define/public (open-par)
            (let* ([start-pos (get-start-position)]
                   [end-pos (get-end-position)]
                   [cur-class (classify-position start-pos)])
              (cond 
                [(and (= start-pos end-pos) 
                      (or (and (eq? cur-class 'block-comment) (blockcomment-end? start-pos))
                          (not (memq cur-class '(comment string error block-comment)))))
                 (insert "()")
                 (set-position (add1 start-pos))
                 ]
                [else (insert "(")])))
          
          (define/public (open-brack)
            (let* ([start-pos (get-start-position)]
                   [end-pos (get-end-position)]
                   [cur-class (classify-position start-pos)])
              (cond 
                [(and (= start-pos end-pos) 
                      (or (and (eq? cur-class 'block-comment) (blockcomment-end? start-pos))
                          (not (memq cur-class '(comment string error block-comment)))))
                 (insert "[]")
                 (set-position (add1 start-pos))
                 ]
                [else (insert "[")])))
                 
          (define/public (java-tabify-selection)
            (let ([start-para (position-paragraph (get-start-position))]
                  [end-para (position-paragraph (get-end-position))])
              (begin-edit-sequence)
              (let loop ([para start-para])
                #;(printf "in tabify outer loop ~a ~a~n" para end-para)
                (let* ([para-start (paragraph-start-position para)]
                       [curr-white-space (skip-whitespace para-start 'forward #f)]
                       [insertion (get-indentation (max 0 (sub1 para-start)))]
                       [closer? #f]
                       [delete? #f])
                  (let loop ()
                    #;(printf "in tabify inner loop ~a ~a, ~a ~n" para para-start curr-white-space)
                    (let ([c (get-character para-start)]
                          [class (classify-position para-start)])
                      #;(printf "character is ~a, ~a~n" c class)
                      (cond
                        [(and (eq? 'white-space class)
                              (not (= curr-white-space para-start))
                              (not (char=? c #\015))
                              (not (char=? c #\012)))
                         (set! delete? #t)
                         (delete para-start (+ para-start 1))
                         (loop)]
                        [(and (not (eq? 'block-comment class)) (char=? #\} c))
                         (set! closer? #t)])))
                  (cond
                    [closer?
                     (insert (substring insertion 0 (max 0 (- (string-length insertion) single-tab-stop))) para-start para-start)]
                    [(or delete? (not (eq? 'block-comment (classify-position para-start))))
                     (insert insertion para-start para-start)]))
                (unless (= para end-para) (loop (+ para 1))))
              (end-edit-sequence)))

          (super-new)))
      
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
      
      ;; Mixin to store type information in the repl
      (define user-types
        (mixin (drscheme:rep:text<%>) ()
          (define types #f)
          (define/public (get-user-types) types)
          (define/public (set-user-types t) (set! types t))
          (super-new)))
      
      ;; matches-language : (union #f (listof string)) -> boolean
      (define (matches-language l)
        (and l (pair? l) (pair? (cdr l)) (equal? (cadr l) "ProfessorJ")))
      
      (define (phase1) void)
      ;Add all the ProfessorJ languages into DrScheme
      (define (phase2) 
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) dynamic-lang%)))
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) full-lang%)))
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) advanced-lang%)))
        (drscheme:language-configuration:add-language 
         (make-object ((drscheme:language:get-default-mixin) intermediate+access-lang%)))
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) intermediate-lang%)))
        (drscheme:language-configuration:add-language
         (make-object ((drscheme:language:get-default-mixin) beginner-lang%))))
      
      ;(make-profj-settings symbol boolean boolean boolean boolean (list string))
      (define-struct profj-settings 
        (print-style print-full? allow-check? allow-test? run-tests? coverage? classpath)
        #:transparent)
      
      ;ProfJ general language mixin
      (define (java-lang-mixin level name number one-line dyn? manual-dirname)
        (when dyn? (dynamic? #t))
        (class* object% (drscheme:language:language<%>)
          #;(define/public (front-end/finished-complete-program settings) (void))
          (define/public (extra-repl-information settings port) (void))
          (define/public (get-reader-module) #f)
          (define/public (get-metadata a b) #f)
          (define/public (metadata->settings m) #f)
          (define/public (get-metadata-lines) #f)
          
          (define autocomplete-words #f)
          
          (define/public (capability-value s) 
            (cond
              [(eq? s 'drscheme:autocomplete-words)
               (unless autocomplete-words 
                 (set! autocomplete-words
                       (if manual-dirname
                           (text:get-completions/manuals (list manual-dirname))
                           '())))
               autocomplete-words]
              [(eq? s 'drscheme:language-menu-title) (string-constant profj-java)]
              [(eq? s 'drscheme:tabify-menu-callback)
               (lambda (ed start stop)
                 (send ed java-tabify-selection))]
              [(memq s '(profj:special:java-comment-box 
                         profj:special:java-examples-box 
                         profjWizard:special:java-class
                         profjWizard:special:java-union
                         drscheme:special:insert-image
                         drscheme:special:insert-large-letters
                         tests:dock-menu
                         tests:test-menu)) #t]
              [(memq s '(slideshow:special-menu 
                         drscheme:define-popup
                         profj:special:java-interactions-box)) #f]
              [(regexp-match #rx"^drscheme:special:" (format "~a" s)) #f]
              [else (drscheme:language:get-capability-default s)]))
          (define/public (first-opened) (void))
          
          ;default-settings: -> profj-settings
          (define/public (default-settings) 
            (if (memq level `(beginner intermediate intermediate+access advanced))
                (make-profj-settings 'field #f #t #f #t #f null)
                (make-profj-settings 'type #f #t #t #f #f null)))
          ;default-settings? any -> bool
          (define/public (default-settings? s) (equal? s (default-settings)))
          
          (define/public (update-test-setting s test?)
            (make-profj-settings (profj-settings-print-style s)
                                 (profj-settings-print-full? s)
                                 (profj-settings-allow-check? s)
                                 (profj-settings-allow-test? s)
                                 test?
                                 (profj-settings-coverage? s)
                                 (profj-settings-classpath s)))

          ;marshall-settings: profj-settings -> (list (list symbol) (list bool) (list string))
          (define/public (marshall-settings s)
            (list (list (profj-settings-print-style s))
                  (list (profj-settings-print-full? s))
                  (list (profj-settings-allow-check? s))
                  (list (profj-settings-allow-test? s))
                  (list (profj-settings-run-tests? s))
                  (list (profj-settings-coverage? s))))
          
          ;unmarshall-settings: any -> (U profj-settings #f)
          (define/public (unmarshall-settings s)
            (if (and (pair? s) (= (length s) 6)
                     (pair? (car s)) (= (length (car s)) 1)
                     (pair? (cadr s)) (= (length (cadr s)) 1)
                     (pair? (caddr s)) (= (length (caddr s)) 1)
                     (pair? (cadddr s)) (= (length (cadddr s)) 1)
                     (pair? (list-ref s 4)) (= (length (list-ref s 4)) 1)
                     (pair? (list-ref s 5)) (= (length (list-ref s 5)) 1))
                (make-profj-settings (caar s) (caadr s) (caaddr s) 
                                     (car (cadddr s))
                                     (car (list-ref s 4)) (car (list-ref s 5)) null)
                #f))

          ;Create the ProfessorJ settings selection panel
          (define/public (config-panel _parent)
            (letrec ([parent (instantiate vertical-panel% ()
                               (parent _parent)
                               (alignment '(center center))
                               (stretchable-height #f)
                               (stretchable-width #f))]                     
                     [print-prefs (instantiate group-box-panel% ()
                                     (label (string-constant profj-language-config-display-preferences))
                                     (parent parent)
                                     (alignment '(left center)))]
                     [print-full (when (memq level '(advanced full))
                                   (make-object check-box% 
                                     (string-constant profj-language-config-display-array)
                                     print-prefs 
                                     (lambda (x y) update-pf)))]
                     [print-style (make-object radio-box%
                                    (string-constant profj-language-config-display-style)
                                    (list (string-constant profj-language-config-class) (string-constant profj-language-config-display-field));"Graphical")
                                    print-prefs
                                    (lambda (x y) (update-ps)))]
                     [testing-prefs (instantiate group-box-panel% ()
                                      (label (string-constant profj-language-config-testing-preferences))
                                      (parent parent)
                                      (alignment '(left center)))]
                     [allow-testing (when (eq? level 'full)
                                      (make-object check-box% 
                                        (string-constant profj-language-config-testing-check) 
                                        testing-prefs
                                        (lambda (x y) update-at)))]
                     [allow-test (when (eq? level 'full)
                                   (make-object check-box% (string-constant profj-language-config-support-test-language)
                                     testing-prefs (lambda (x y) update-at2)))]
                     #;[display-testing 
                      (make-object check-box% (string-constant profj-language-config-testing-enable)
                        testing-prefs (lambda (x y) (update-dt x y)))]
                     [collect-coverage 
                      (make-object check-box% (string-constant profj-language-config-testing-coverage)
                        testing-prefs (lambda (x y) update-cc))]
                     
                     [update-pf (lambda () (void))]
                     [update-ps (lambda () (void))]
                     [update-at (lambda () (void))]
                     [update-at2 (lambda () (void))]
                     [update-dt (lambda (box event) 
                                  (when (eq? 'check-box (send event get-event-type))
                                    (send collect-coverage enable (send box get-value))))]
                     [update-cc (lambda () (void))]
                     
                     [cp-panel (instantiate group-box-panel% ()
                                            (parent parent)
                                            (alignment '(left center))
                                            (label (string-constant profj-language-config-classpath)))]
                     [tp-panel (instantiate horizontal-panel% ()
                                 (parent cp-panel)
                                 (alignment '(center center))
                                 (stretchable-height #f))]
                     [lb (instantiate list-box% ()
                                     (parent cp-panel)
                                     (choices `("a" "b" "c"))
                                     (label #f)
                                     (callback (lambda (x y) (update-buttons))))]
                     [top-button-panel (instantiate horizontal-panel% ()
                                         (parent cp-panel)
                                         (alignment '(center center))
                                         (stretchable-height #f))]
                     [bottom-button-panel (instantiate horizontal-panel% ()
                                            (parent cp-panel)
                                            (alignment '(center center))
                                            (stretchable-height #f))]
                     [list-button 
                      (make-object button% (string-constant profj-language-config-classpath-display) tp-panel 
                        (lambda (x y) (list-callback)))]
                     [add-button 
                      (make-object button% (string-constant ml-cp-add) bottom-button-panel 
                        (lambda (x y) (add-callback)))]
                     [remove-button 
                      (make-object button% (string-constant ml-cp-remove) bottom-button-panel 
                        (lambda (x y) (remove-callback)))]
                     [raise-button 
                      (make-object button% (string-constant ml-cp-raise) top-button-panel 
                        (lambda (x y) (raise-callback)))]
                     [lower-button 
                      (make-object button% (string-constant ml-cp-lower) top-button-panel 
                        (lambda (x y) (lower-callback)))]
                     [enable? #f]
                     
                     [update-buttons 
                      (lambda ()
                        (let ([lb-selection (send lb get-selection)]
                              [lb-tot (send lb get-number)])
                          (send remove-button enable (and lb-selection enable?))
                          (send raise-button enable (and lb-selection enable? (not (= lb-selection 0))))
                          (send lower-button enable (and lb-selection enable? (not (= lb-selection (- lb-tot 1)))))))]
                     [add-callback
                      (lambda ()
                        (let ([dir (get-directory (string-constant profj-language-config-choose-classpath-directory) 
                                                  (send parent get-top-level-window))])
                          (when dir
                            (send lb append dir #f)
                            (preferences:set 'profj:classpath (cons dir (preferences:get 'profj:classpath)))
                            (update-buttons))))]
                     [list-callback
                      (lambda ()
                        (send lb clear)
                        (let ((cpath (preferences:get 'profj:classpath)))
                          (let loop ((n 0) (l cpath))
                            (cond
                              ((> n (sub1 (length cpath))) (void))
                              (else (send lb append (car l))
                                    (send lb set-data n (car l))
                                    (loop (+ n 1) (cdr l)))))
                          (unless (null? cpath)
                            (send lb set-selection 0))
                          (set! enable? #t)
                          (update-buttons)))]
                     [remove-callback
                      (lambda ()
                        (let ([to-delete (send lb get-selection)])
                           (send lb delete to-delete)
                          (unless (zero? (send lb get-number))
                            (send lb set-selection (min to-delete (- (send lb get-number) 1))))
                          (preferences:set 'profj:classpath (get-classpath))
                          (update-buttons)))]
                     [lower-callback
                      (lambda ()
                        (let* ([sel (send lb get-selection)]
                               [vec (get-lb-vector)]
                               [below (vector-ref vec (+ sel 1))])
                          (vector-set! vec (+ sel 1) (vector-ref vec sel))
                          (vector-set! vec sel below)
                          (set-lb-vector vec)
                          (send lb set-selection (+ sel 1))
                          (preferences:set 'profj:classpath (get-classpath))
                          (update-buttons)))]
                     [raise-callback
                      (lambda ()
                        (let* ([sel (send lb get-selection)]
                               [vec (get-lb-vector)]
                               [above (vector-ref vec (- sel 1))])
                          (vector-set! vec (- sel 1) (vector-ref vec sel))
                          (vector-set! vec sel above)
                          (set-lb-vector vec)
                          (send lb set-selection (- sel 1))
                          (preferences:set 'profj:classpath (get-classpath))
                          (update-buttons)))]
                     [get-lb-vector
                      (lambda ()
                        (list->vector
                         (let loop ([n 0])
                           (cond
                             [(= n (send lb get-number)) null]
                             [else (cons (cons (send lb get-string n)
                                               (send lb get-data n))
                                         (loop (+ n 1)))]))))]
                     [set-lb-vector
                      (lambda (vec)
                        (send lb clear)
                        (let loop ([n 0])
                          (cond
                            [(= n (vector-length vec)) (void)]
                            [else (send lb append (car (vector-ref vec n)))
                                  (send lb set-data n (cdr (vector-ref vec n)))
                                  (loop (+ n 1))])))]
                     [get-classpath
                      (lambda ()
                        (let loop ([n 0])
                          (cond
                            [(= n (send lb get-number)) null]
                            [else
                             (let ([data (send lb get-data n)])
                               (cons (if data
                                         'default
                                         (send lb get-string n))
                                     (loop (+ n 1))))])))]
                     [install-classpath
                      (lambda (paths)
                        (send lb clear)
                        (for-each (lambda (cp)
                                    (if (symbol? cp)
                                        (send lb append "Default" #t)
                                        (send lb append cp #f)))
                                  paths))])
              (send lb set '())
              (update-buttons)
              
              (case-lambda
                [()
                 (make-profj-settings (case (send print-style get-selection)
                                        [(0) 'type]
                                        [(1) 'field]
                                        [(2) 'graphical])
                                      (and (memq level '(advanced full))
                                           (send print-full get-value))
                                      (or (not (eq? level 'full))
                                          (send allow-testing get-value))
                                      (and (eq? level 'full)
                                           (send allow-test get-value))
                                      #t #;(send display-testing get-value)
                                      (and #t #;(send display-testing get-value)
                                           (send collect-coverage get-value))
                                      (get-classpath))]
                [(settings)
                 (send print-style set-selection
                       (case (profj-settings-print-style settings)
                         ((type default) 0)
                         ((field) 1)
                         ((graphical) 2)))
                 (when (memq level '(advanced full))
                   (send print-full set-value (profj-settings-print-full? settings)))
                 (when (eq? level 'full)
                   (send allow-testing set-value (profj-settings-allow-check? settings)))
                 (when (eq? level 'full)
                   (send allow-test set-value (profj-settings-allow-test? settings)))
                 #;(send display-testing set-value (profj-settings-run-tests? settings))
                 (if #t #;(send display-testing get-value)
                     (send collect-coverage set-value (profj-settings-coverage? settings))
                     (send collect-coverage enable #f))
                 (install-classpath (profj-settings-classpath settings))])))
                               
          (define eventspace (current-eventspace))
          (define/public (front-end/complete-program port settings)
            (mred? #t)
            (let ([name (object-name port)]
                  [rep (drscheme:rep:current-rep)]
                  #;[eventspace (current-eventspace)]
                  [execute-types (create-type-record)])
              (let ([name-to-require #f]
                    [require? #f]
                    [tests-run? #f]
                    [tests #f]
                    [compiled? #f]
                    [modules null]
                    [extras null])
              (lambda ()
                (syntax-as-top
                 (let ([end? (eof-object? (peek-char-or-special port))])
                   #;(if end? 
                       eof 
                       (datum->syntax #f `(parse-java-full-program ,(parse port name level)
                                                                   ,name) #f))
                   (cond
                     [(and end? (null? modules) (null? extras) tests-run? (not require?)) eof]
                     [(not compiled?)
                      (execution? #t)
                      (let* ([parsed (parse port name level)]
                             [compilation-units (compile-ast parsed level execute-types)]
                             [examples (if (testcase-ext?)
                                           (list (send execute-types get-test-classes) null)
                                           (find-examples compilation-units))])
                          #;(printf "ProfJ compilation complete~n")
                        #;(printf "compilation units- ~a~n" (map syntax->datum 
                                                               (apply append (map compilation-unit-code compilation-units))))
                        (set! compiled? #t)
                        (set! modules (order compilation-units))
                        (when rep (send rep set-user-types execute-types))
                        #;(set! extras (process-extras (send execute-types get-interactions-boxes) execute-types))
                        (set! tests examples))
                      (datum->syntax #f '(void) #f)]
                     [else
                      (cond
                        [(and (not require?) (null? modules) (not tests-run?))
                         (let* ([test-engine-obj 
                                 (make-object (if (testcase-ext?) java-test-base% java-examples-engine%))]
                                [tc-info (send test-engine-obj get-info)]
                                [format (lambda (o break?) 
                                          (format-java-value o (make-format-style #t 'field break?)))])
                           (set! tests-run? #t)
                           (datum->syntax 
                            #f
                            `((lambda ()
                                (,namespace-set-variable-value! 'current~test~object% ,tc-info)
                                (send ,test-engine-obj install-tests 
                                      (map (lambda (c)
                                             (list c ([current-eval] (string->symbol c)) c))
                                           (list ,@(car tests))))
                                (when ,(coverage?)
                                  (send (send ,test-engine-obj get-info) add-analysis 
                                        ,(make-object coverage-analysis%)))
                                (send ,test-engine-obj refine-display-class 
                                      ,(cond
                                         [(and (testcase-ext?) (coverage?)) java-test-coverage-graphics%]
                                         [(coverage?) java-examples-coverage-graphics%]
                                         [(testcase-ext?) java-test-graphics%]
                                         [else java-examples-graphics%]))
                                #;(printf "About to run tests~n")
                                (send ,test-engine-obj run)
                                #;(printf "Test methods run~n")
                                (send ,test-engine-obj setup-display ,rep ,eventspace)
                                (let ([test-objs (send ,test-engine-obj test-objects)])
                                  (let inner-loop ((os test-objs))
                                    (unless (null? os)
                                      (let ((formatted (,format (car os) #f)))
                                        (when (< 24 (,(lambda (t) (total-length t)) formatted))
                                          (set! formatted (,format (car os) #t)))
                                        (let loop ((out formatted))
                                          (unless (null? out)
                                            (write-special (car out))
                                            (loop (cdr out))))
                                        (newline))
                                      (inner-loop (cdr os)))))
                                (send ,test-engine-obj summarize-results (current-output-port))
                                ))
                            #f))]
                        [(and (not require?) (null? modules) tests-run?)
                         (begin0
                           (car extras)
                           (set! extras (cdr extras)))]
                        [require? 
                         (set! require? #f)
                         (with-syntax ([name name-to-require])
                           (syntax (require (quote name))))]
                        [else 
                         #;(printf "~a~n" (syntax->datum (car mods)))
                         (let-values (((name syn) (get-module-name (expand (car modules)))))
                           (set! name-to-require name)
                           (set! require? #t)
                           (set! modules (cdr modules))
                           (errortrace-annotate syn))])])))))))
          (define/public (front-end/interaction port settings)
            (mred? #t)
            (let* ([name (object-name port)]
                   [executed? #f]
                   [rep (drscheme:rep:current-rep)]
                   [types (and rep (or (send rep get-user-types)
                                       (begin
                                         (send rep set-user-types (create-type-record))
                                         (send rep get-user-types))))])
              (lambda ()
                (if executed? 
                    eof
                    (begin
                      (set! executed? #t)
                      (syntax-as-top
                         (compile-interactions-ast 
                          (parse-interactions port name level)
                          name level types  #t)
                        
                       #;(datum->syntax
                        #f
                        `(parse-java-interactions ,(parse-interactions port name level) ,name)
                        #f)))))))
          (define/public (front-end/finished-complete-program settings) (void))
          
          (define (get-defn-editor port-name)
            (let* ([dr-frame (and (drscheme:rep:current-rep)
                                  (send (drscheme:rep:current-rep) get-top-level-window))]
                   [tabs (and dr-frame  (send dr-frame get-tabs))]
                   [defs (if dr-frame
                             (map (lambda (t) (send t get-defs)) tabs)
                             null)]
                   [def (filter (lambda (d)
                                  (and (is-a? d drscheme:unit:definitions-text<%>)
                                       (send d port-name-matches? port-name)))
                                    defs)])
              (and dr-frame (= 1 (length def)) (car def))))
           
          ;process-extras: (list struct) type-record -> (list syntax)
          (define/private (process-extras extras type-recs)
            (cond
              ((null? extras) null)
              ((example-box? (car extras))
               (let ((contents (eval (example-box-contents (car extras)))))
                 (append 
                  (map (lambda (example)
                         (let* ((type-editor (car example))
                                (type (parse-type (open-input-text-editor type-editor) type-editor level))
                                (name-editor (cadr example))
                                (name (parse-name (open-input-text-editor name-editor) name-editor))
                                (val-editor (caddr example))
                                (val (parse-expression (open-input-text-editor val-editor) val-editor level)))
                           (compile-interactions-ast
                            (make-var-init (make-var-decl name null type #f #f) val #f)
                            val-editor level type-recs #t)))
                       contents)
                  (process-extras (cdr extras) type-recs))))
              ((test-case? (car extras))
               ;(printf "in process-extras~n")
               ;(printf "~a~n" (test-case-test (car extras)))
               (cons
                (let ((tc (test-case-test (car extras))))
                  (syntax-case tc (parse-java-interactions)
                    ((test-case eq (parse-java-interactions ast-1 ed-1) 
                                (parse-java-interactions ast-2 ed-2) end1 end2)
                     (datum->syntax #f
                                    `(,(syntax test-case)
                                      ,(dynamic-require '(lib "profj-testing.ss" "profj") 'java-values-equal?);,(syntax eq)
                                      ,(compile-interactions-ast (syntax->datum (syntax ast-1))
                                                                 (syntax->datum (syntax ed-1)) level type-recs #f)
                                      ,(compile-interactions-ast (syntax->datum (syntax ast-2))
                                                                 (syntax->datum (syntax ed-2)) level type-recs #f)
                                      ,(syntax end1) ,(syntax end2))
                                    #f))
                    (_ tc))) (process-extras (cdr extras) type-recs))
               #;(cons (test-case-test (car extras)) (process-extras (cdr extras) type-recs)))
              #;((interact-case? (car extras))
               (let ((interact-box (interact-case-box (car extras))))
                 (send interact-box set-level level)
                 (send interact-box set-records execute-types)
                 (send interact-box set-ret-kind #t)
                 (append 
                  (with-handlers ((exn? 
                                   (lambda (e)
                                     (send execute-types clear-interactions)
                                     (raise e))))
                    (let-values (((syn-list t t2) 
                                  (send interact-box read-special #f #f #f #f))) syn-list))
                  (process-extras (cdr extras) type-recs))))))
        
          (define/private (find-examples cus)
            (let cu-loop ((cs cus) (examples null) (near-examples null))
              (cond
                ((null? cs) (list examples near-examples))
                (else
                 (let class-loop ((names (compilation-unit-contains (car cs)))
                                  (ex examples)
                                  (ne near-examples))
                   (cond
                     ((null? names) (cu-loop (cdr cs) ex ne))
                     ((regexp-match "Example" (car names))
                      (class-loop (cdr names)
                                  (cons (car names) ex)
                                  ne))
                     ((or (regexp-match "Eample" (car names))
                          (regexp-match "Exmple" (car names))
                          (regexp-match "Exaple" (car names))
                          (regexp-match "Examle" (car names))
                          (regexp-match "Exampe" (car names))
                          (regexp-match "Exampl" (car names))
                          (regexp-match "Eaxmple" (car names)))
                      (class-loop (cdr names)
                                  ex
                                  (cons (format (string-constant profj-test-name-close-to-example)
                                                (car names))
                                        ne)))
                     ((regexp-match "example" (car names))
                      (class-loop (cdr names)
                                  ex
                                  (cons (format (string-constant profj-test-name-example-miscapitalized)
                                                (car names))
                                        ne)))
                     (else
                      (class-loop (cdr names) ex ne))))))))
                
            
          
          ;find-main-module: (list compilation-unit) -> (U syntax #f)
          (define/private (find-main-module mod-lists)
            (if (null? mod-lists)
                #f
                (let ((names (compilation-unit-contains (car mod-lists)))
                      (syntaxes (compilation-unit-code (car mod-lists))))
                  (if (member (cadr (main)) names)
                      (if (= (length syntaxes) 1)
                          (list-ref syntaxes 0)
                          (list-ref syntaxes (find-position names 1)))
                      (find-main-module (cdr mod-lists))))))
        
          ;find-position: (list string) number-> number
          (define/private (find-position l p)
            (when (null? l)
              (error 'find-position "Internal Error: member incorrectly chose an element as a member"))
            (if (equal? (cadr (main)) (car l))
                p
                (find-position (cdr l) (add1 p))))
          
          ;order: (list compilation-unit) -> (list syntax)
          (define/private (order mod-lists)
            (if (null? mod-lists)
                null
                (append (compilation-unit-code (car mod-lists))
                        (order (cdr mod-lists)))))
              
          (define/public (get-comment-character) (values "//" #\*))
          (define/public (get-style-delta) #f)
          (define/public (get-language-position) 
            (cons (string-constant experimental-languages) (list "ProfessorJ" name)))
          (define/public (get-language-numbers) (list 1000 -1000 number))
          (define/public (get-language-name) (string-append "ProfessorJ: " name))
          (define/public (get-language-url) #f)
          (define/public (get-teachpack-names) null)

          (define/private (syntax-as-top s)
	    (if (syntax? s) (namespace-syntax-introduce s) s))
          
          (define/private (get-program-windows rep source)
            (let* ([dr-frame (send rep get-top-level-window)]
                   [tabs (and dr-frame 
                              (send dr-frame get-tabs))]
                   [tab/defs (if dr-frame
                                 (map (lambda (t) (cons (send t get-defs) t)) tabs)
                                 null)]
                   [tab/def (filter (lambda (t/d)
                                      (and (is-a? (car t/d) drscheme:unit:definitions-text<%>)
                                           (send (car t/d) port-name-matches? source)))
                                    tab/defs)])
              (and dr-frame 
                   (= 1 (length tab/def))
                   (list dr-frame (car (car tab/def)) (cdr (car tab/def))))))               
          
          (define/public (on-execute settings run-in-user-thread)
            (dynamic-require 'profj/libs/java/lang/Object #f)
            (let ([obj-path ((current-module-name-resolver) 'profj/libs/java/lang/Object #f #f)]
                  [string-path ((current-module-name-resolver) 'profj/libs/java/lang/String #f #f)]
                  [class-path ((current-module-name-resolver) 'scheme/class #f #f)]
                  [mred-path ((current-module-name-resolver) 'mred #f #f)]
                  [n (current-namespace)]
                  [e (current-eventspace)])
              (test-ext? (profj-settings-allow-check? settings))
              (testcase-ext? (profj-settings-allow-test? settings))
              (let ((execute-types (create-type-record)))
                (run-in-user-thread
                 (lambda ()
                   (test-ext? (profj-settings-allow-check? settings))
                   (testcase-ext? (profj-settings-allow-test? settings))
                   (test-execute (get-preference 'tests:enable? (lambda () #t)))
                   (coverage? (and (test-execute) (profj-settings-coverage? settings)))
                   (error-display-handler 
                    (drscheme:debug:make-debug-error-display-handler (error-display-handler)))
                   #;(let ((old-current-eval (drscheme:debug:make-debug-eval-handler (current-eval))))
                     (current-eval 
                      (lambda (exp)
                        (syntax-case exp (parse-java-full-program parse-java-interactions)
                          #;((parse-java-full-program ex s)
                           (let ((exp (old-current-eval (syntax ex)))
                                 (src (old-current-eval (syntax (quote s)))))
                             (execution? #t)
                             (set! execute-types (create-type-record))
                             (let* ((compilation-units (time (compile-ast exp level execute-types)))
                                    (examples (if (testcase-ext?)
                                                  (list (send execute-types get-test-classes) null)
                                                  (find-examples compilation-units))))
                               (printf "ProfJ compilation complete~n")
                               (let ((name-to-require #f)
                                     (tests-run? #f))
                                 (let loop ((mods (order compilation-units))
                                            (extras (process-extras 
                                                     (send execute-types get-interactions-boxes) 
                                                     execute-types))
                                            (require? #f))
                                   (cond
                                     ((and (not require?) (null? mods) tests-run? (null? extras)) (void))
                                     ((and (not require?) (null? mods) (not tests-run?))
                                      (let* ([test-engine-obj 
                                              (make-object (if (testcase-ext?) java-test-base% java-examples-engine%))]
                                             [tc-info (send test-engine-obj get-info)])
                                        (namespace-set-variable-value! 'current~test~object% tc-info)
                                        (send test-engine-obj install-tests 
                                              (map (lambda (c)
                                                     (list c (old-current-eval (string->symbol c)) c))
                                                   (car examples)))
                                        (when (coverage?)
                                          (send (send test-engine-obj get-info) add-analysis 
                                                (make-object coverage-analysis%)))
                                        (send test-engine-obj refine-display-class 
                                              (cond
                                                [(and (testcase-ext?) (coverage?)) java-test-coverage-graphics%]
                                                [(coverage?) java-examples-coverage-graphics%]
                                                [(testcase-ext?) java-test-graphics%]
                                                [else java-examples-graphics%]))
                                        #;(printf "About to run tests~n")
                                        (send test-engine-obj run)
                                        #;(printf "Test methods run~n")
                                        (send test-engine-obj setup-display (drscheme:rep:current-rep) e)
                                        (send test-engine-obj summarize-results (current-output-port))
                                        (let ([test-objs (send test-engine-obj test-objects)])
                                          (let inner-loop ((os test-objs))
                                            (unless (null? os)
                                              (let ((formatted 
                                                     (format-java-value (car os) (make-format-style #t 'field #f))))
                                                (when (< 24 (total-length formatted))
                                                  (set! formatted 
                                                        (format-java-value (car os) (make-format-style #t 'field #t))))
                                                (let loop ((out formatted))
                                                  (unless (null? out)
                                                    (write-special (car out))
                                                    (loop (cdr out))))
                                                (newline))
                                              (inner-loop (cdr os))))))
                                      (set! tests-run? #t)
                                      (loop mods extras require?))
                                     ((and (not require?) (null? mods) tests-run?)
                                      (old-current-eval (syntax-as-top (car extras)))
                                      (loop mods (cdr extras) require?))
                                     (require? 
                                      (old-current-eval 
                                       (syntax-as-top (with-syntax ([name name-to-require])
                                                        (syntax (require (quote name))))))
                                      (loop mods extras #f))
                                     (else 
                                      #;(printf "~a~n" (syntax->datum (car mods)))
                                      (let-values (((name syn) (get-module-name (expand (car mods)))))
                                        (set! name-to-require name)
                                        (syntax-as-top  (old-current-eval 
                                                         (errortrace-annotate syn)))
                                        (loop (cdr mods) extras #t)))))))))
                          ((parse-java-interactions ex loc)
                           (let ((exp (syntax->datum (syntax ex))))
                             (old-current-eval 
                              (syntax-as-top (compile-interactions-ast exp (syntax loc) level execute-types #t)))))
                          (_ (old-current-eval exp))))))
                   (with-handlers ([void (lambda (x)  (printf "~a~n" (exn-message x)))])
                     (namespace-require 'mzscheme)
                     (namespace-attach-module n obj-path)
                     (namespace-attach-module n string-path)
                     (namespace-attach-module n class-path)
                     (namespace-attach-module n mred-path)
                     (namespace-require obj-path)
                     (namespace-require string-path)
                     (namespace-require class-path)
                     (namespace-require mred-path)
                     (namespace-require '(prefix javaRuntime: profj/libs/java/runtime))
                     (namespace-require '(prefix c: mzlib/contract))
                     ))))))
          
          #;(define/public (render-value value settings port); port-write)
            (let ((print-full? (profj-settings-print-full? settings))
                  (style (profj-settings-print-style settings)))
              (write-special 
               (if (is-a? value String)
                     (format-java value print-full? style null #t 0)
                     (let ((out (format-java value print-full? style null #f 0)))
                       (if (< 25 (string-length out))
                           (format-java value print-full? style null #t 0)
                           out))) port)
              (void)))
          
          (define/public (render-value value settings port)
            (let* ((print-full? (profj-settings-print-full? settings))
                   (style (profj-settings-print-style settings))
                   (formatted (format-java-value value 
                                                 (make-format-style print-full? style #f))))
              (when (< 24 (total-length formatted))
                (set! formatted (format-java-value value (make-format-style print-full? style #t))))
              (let loop ((out formatted))
                (unless (null? out)
                  (write-special (car out) port)
                  (loop (cdr out))))))
          
          (define/private (total-length lst)
            (cond
              ((null? lst) 0)
              ((string? (car lst)) (+ (string-length (car lst))
                                      (total-length (cdr lst))))
              (else (add1 (total-length (cdr lst))))))
              
          (define/public (render-value/format value settings port width) 
            (render-value value settings port)
            (newline port))
          
          (define/public (create-executable fn parent . args)
            ;(printf "create-exe called~n")
	    (message-box (string-constant profj-unsupported)
			 (string-constant profj-executables-unsupported)
			 parent))
	  (define/public (get-one-line-summary) one-line)
          
          (super-instantiate ())))
      
      ;Create the ProfessorJ languages
      (define dynamic-lang% (java-lang-mixin 'full (string-constant profj-dynamic-lang) 6 (string-constant profj-dynamic-lang-one-summary) #t #f))
      (define full-lang% (java-lang-mixin 'full (string-constant profj-full-lang) 5 (string-constant profj-full-lang-one-line-summary) #f #f))
      (define advanced-lang% (java-lang-mixin 'advanced (string-constant profj-advanced-lang) 4 (string-constant profj-advanced-lang-one-line-summary) #f
                                              "profj-advanced"))
      (define intermediate+access-lang% 
        (java-lang-mixin 'intermediate+access 
                         (string-constant profj-intermediate-access-lang) 3 (string-constant profj-intermediate-access-lang-one-line-summary) #f
                         "profj-intermediate-access"))
      (define intermediate-lang% 
        (java-lang-mixin 'intermediate (string-constant profj-intermediate-lang) 2 (string-constant profj-intermediate-lang-one-line-summary) #f
                         "profj-intermediate"))
      (define beginner-lang% (java-lang-mixin 'beginner (string-constant profj-beginner-lang) 1 (string-constant profj-beginner-lang-one-line-summary) #f
                                              "profj-beginner"))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  Wire up to DrScheme
      ;;
      
      (drscheme:modes:add-mode (string-constant profj-java-mode) mode-surrogate repl-submit matches-language)
      (color-prefs:add-to-preferences-panel (string-constant profj-java) 
                                            (extend-preferences-panel color-prefs-table))
      (color-prefs:add-to-preferences-panel (string-constant profj-java-coverage)
                                            (extend-preferences-panel coverage-color-prefs))
      
      (define (register line)
        (let ([sym (car line)]
              [color (cadr line)])
          (color-prefs:register-color-preference (short-sym->pref-name sym)
                                                 (short-sym->style-name sym)
                                                 color)))
      (for-each register colors-table)
      (for-each register coverage-color-prefs)
      
      ;;Java Boxes
      (define java-box%
        (class* decorated-editor-snip% ()
          (inherit get-admin get-editor)
          (define/public (get-comment) "// ")
          (define/public (get-mesg) (string-constant profj-convert-to-text-comment))

          (define/override get-text
            (lambda (offset num [flattened? #t])
              (let* ([super-res (super get-text offset num flattened?)]
                     [replaced (string-append (send this get-comment) 
                                              (regexp-replace* "\n" super-res 
                                                               (string-append "\n" (send this get-comment))))])
                (if (char=? #\newline (string-ref replaced (- (string-length replaced) 1)))
                    replaced
		    (string-append replaced "\n")))))
          (define/override (get-menu)
            (let ([menu (make-object popup-menu%)])
              (make-object menu-item% 
                (send this get-mesg)
                menu
                (lambda (x y)
                  (let ([to-ed (find-containing-editor)])
                    (when to-ed
                      (let ([this-pos (find-this-position)])
                        (when this-pos
                          (let ([from-ed (get-editor)])
                            (send to-ed begin-edit-sequence)
                            (send from-ed begin-edit-sequence)
                            (copy-contents-with-comment-char-to-position to-ed from-ed (+ this-pos 1))
                            (send to-ed delete this-pos (+ this-pos 1))
                            (send to-ed end-edit-sequence)
                            (send from-ed end-edit-sequence))))))))
              menu))
          ;; find-containing-editor : -> (union #f editor)
          (define/private (find-containing-editor)
            (let ([admin (get-admin)])
              (and admin
                   (send admin get-editor))))
          
          ;; find-this-position : -> (union #f number)
          (define/private (find-this-position)
            (let ([ed (find-containing-editor)])
              (and ed
                   (send ed get-snip-position this))))
          
          ;; copy-contents-with-comment-char-to-position : (is-a? text%) number -> void
          (define/private (copy-contents-with-comment-char-to-position to-ed from-ed pos)
            (let loop ([snip (find-last-snip from-ed)])
              (cond
                [snip 
                 (when (or (memq 'hard-newline (send snip get-flags))
                           (memq 'newline (send snip get-flags)))
                   (send to-ed insert (send this get-comment) pos))
                 (send to-ed insert (send snip copy) pos)
                 (loop (send snip previous))]
                [else 
                 (send to-ed insert (send this get-comment) pos)])))
          
          ;; find-last-snip : editor -> snip
          ;; returns the last snip in the editor
          (define/private (find-last-snip ed)
            (let loop ([snip (send ed find-first-snip)]
                       [acc (send ed find-first-snip)])
              (cond
                [snip (loop (send snip next) snip)]
                [else acc])))
          
          (super-instantiate ())
          ))
      
      ;;The following code has been taken with small modifications from framework/private/comment-box.ss
      (define snipclass-java-comment%
        (class decorated-editor-snipclass%
          (define/override (make-snip stream-in) (instantiate java-comment-box% ()))
          (super-instantiate ())))
      
      (define snipclass-comment (make-object snipclass-java-comment%))
      (send snipclass-comment set-version 1)
      (send snipclass-comment set-classname "java-comment-box%")
      (send (get-the-snip-class-list) add snipclass-comment)
      
      (define java-comment-box%
        (class* java-box% (readable-snip<%>)
          (define/override (make-editor) (new text:keymap%))
          (define/override (make-snip) (make-object java-comment-box%))
          (define/override (get-corner-bitmap) (get-comment-gif))
          (define/override (get-position) 'left-top)

          (define/public (read-special source line column position)
            (make-special-comment 1))
          
          (super-instantiate ())
          (inherit set-snipclass get-editor)
          (set-snipclass snipclass-comment)))
          
      (define (java-comment-box-mixin %)
        (class %
          (inherit
            get-insert-menu
            get-special-menu get-edit-target-object register-capability-menu-item)
          
          (super-new)
          (new menu-item%
            (label  (string-constant profj-insert-java-comment-box))
            (parent (get-insert-menu))
            (callback
             (lambda (menu event)
               (let ([c-box (new java-comment-box%)]
                     [text (get-edit-target-object)])
                 (send text insert c-box)
                 (send text set-caret-owner c-box 'global))))
            (demand-callback
             (lambda (mi)
               (send mi enable ((get-edit-target-object) . is-a? . text%)))))
          (register-capability-menu-item 'profj:special:java-comment-box (get-insert-menu))
          ))
      
      (drscheme:get/extend:extend-unit-frame java-comment-box-mixin)
      (drscheme:language:register-capability 'profj:special:java-comment-box (flat-contract boolean?) #f)
            
      (define snipclass-java-interactions%
        (class decorated-editor-snipclass%
          (define/override (make-snip stream-in) (instantiate java-interactions-box% ()))
          (super-instantiate ())))
      
      (define snipclass-interactions (make-object snipclass-java-interactions%))
      (send snipclass-interactions set-version 1)
      (send snipclass-interactions set-classname "java-interactions-box%")
      (send (get-the-snip-class-list) add snipclass-interactions)
      
      (define java-interactions-box%
        (class* java-box% (readable-snip<%>)
          (define/override (make-editor) (new ((drscheme:unit:get-program-editor-mixin) color:text%)))
          (define/override (make-snip) (make-object java-interactions-box%))
          (define/override (get-corner-bitmap) (get-ji-gif))
          (define/override (get-mesg) (string-constant profj-convert-to-comment))
          (define level 'full)
          (define type-recs (create-type-record))
          (define ret-list? #f)
          (define/public (set-level l) (set! level l))
          (define/public (set-records tr) (set! type-recs tr))
          (define/public (set-ret-kind k) (set! ret-list? k)) 
          (define-struct input-length (start-pos end-pos))

          (define/private (newline? char) (memq char '(#\015 #\012)))
          
          (define/public (read-special source line column position)
            (let* ((ed (get-editor))
                   (port (open-input-text-editor ed 0 'end (editor-filter #t)))
                   (inputs-list null))
              (let outer-loop ((c (read-char-or-special port)) (start 0))
                (unless (eof-object? c)
                  (let inner-loop ((put c) (offset start))
                    (cond
                      ((eof-object? put)
                       (set! inputs-list (cons (make-input-length start offset) inputs-list))
                       (outer-loop (read-char-or-special port) (add1 offset)))
                      ((newline? put) 
                       (let ((new-put (read-char-or-special port))) 
                         (if (or (eof-object? new-put) (newline? new-put))
                             (begin
                               (set! inputs-list (cons (make-input-length start (add1 offset)) inputs-list))
                               (outer-loop (read-char-or-special port) (+ 2 offset)))
                             (inner-loop new-put (add1 offset)))))
                      #;((or (eq? put #\015) (eq? put #\012) (eof-object? put))
                         (set! inputs-list (cons (make-input-length start offset) inputs-list))
                         (outer-loop (read-char-or-special port) (add1 offset)))
                      (else (inner-loop (read-char-or-special port) (add1 offset)))))))
              (let ((syntax-list (map 
                                  (lambda (input-len)
                                    (interactions-offset (input-length-start-pos input-len))
                                    
                                    (compile-interactions (open-input-text-editor ed 
                                                                                  (input-length-start-pos input-len)
                                                                                  (input-length-end-pos input-len)
                                                                                  (editor-filter #t))
                                                          ed type-recs level))
                                  (reverse inputs-list))))
                ;(printf "~a~n~a~n" syntax-list (map remove-requires syntax-list))
                (if ret-list?
                    syntax-list
                    (datum->syntax #f `(begin ,@(map remove-requires syntax-list)) #f)))))
          (define (remove-requires syn)
            (syntax-case* syn (begin require) (lambda (r1 r2) (eq? (syntax-e r1) (syntax-e r2)))
              ((begin (require x ...) exp1 exp ...) (syntax (begin exp1 exp ...)))
              (else syn)))
          
          (super-instantiate ())
          (inherit set-snipclass get-editor)
          (set-snipclass snipclass-interactions)
          (send (get-editor) start-colorer
                short-sym->style-name get-syntax-token 
                (list (list '|{| '|}|)
                      (list '|(| '|)|)
                      (list '|[| '|]|)))
          ))
      
      (define (java-interactions-box-mixin %)
        (class %
          (inherit get-insert-menu
                   get-special-menu get-edit-target-object register-capability-menu-item)
          
          (super-new)
          (new menu-item%
               (label (string-constant profj-insert-java-interactions-box))
               (parent (get-insert-menu))
               (callback
                (lambda (menu event)
                  (let ([i-box (new java-interactions-box%)]
                        [text (get-edit-target-object)])
                    (send text insert i-box)
                    (send text set-caret-owner i-box 'global)))))
          (register-capability-menu-item 'profj:special:java-interactions-box (get-insert-menu))
          ))
      
      (drscheme:get/extend:extend-definitions-text indent-mixin)
      (drscheme:get/extend:extend-interactions-text indent-mixin)
      (drscheme:get/extend:extend-interactions-text user-types)
      (drscheme:get/extend:extend-unit-frame java-interactions-box-mixin)
      (drscheme:language:register-capability 'profj:special:java-interactions-box (flat-contract boolean?) #t)
 
  ))
  (define (editor-filter delay?)
    (lambda (s)
      (let ((name (send (send s get-snipclass) get-classname)))
        (cond
          ((equal? "test-case-box%" name) (values (make-test-case s) 1))
          ((equal? "java-interactions-box%" name) (values (make-interact-case s) 1))
          ((equal? "java-class-box%" name) (values (make-class-case s) 1))
          (delay? (values (lambda () (send s read-one-special 0 #f #f #f #f)) 1))
          (else (values s 1))))))
    
  (provide compile-interactions-helper)
  (define-syntax (compile-interactions-helper syn)
    (syntax-case syn ()
      ((_ comp ast)
        (namespace-syntax-introduce ((syntax->datum (syntax comp))
                                     (syntax->datum (syntax ast)))))))
  
  (define (get-module-name stx)
    (syntax-case stx (module #%plain-module-begin)
      [(module name lang (#%plain-module-begin bodies ...))
       (values (syntax name)
               (syntax (module name lang
                         (#%plain-module-begin bodies ...))))]
      [else 
       (raise-syntax-error 'Java 
                           "Internal Syntax error in getting module name"
                           stx)]))
  
  (define (add-main-call stx)
    (syntax-case stx (module #%plain-module-begin)
      [(module name lang (#%plain-module-begin bodies ...))
       (let ([execute-body (if (car (main))
                               `(lambda (x) 
				  (display (string-constant profj-executing-main))
                                  (display " - ")
                                  (display (,(string->symbol (string-append (cadr (main)) "-main_java.lang.String1")) x)))
                               'void)])
         (with-syntax ([main (datum->syntax #f execute-body #f)]) 
           (values (syntax name)
                   (syntax (module name lang 
                             (#%plain-module-begin 
                              (begin bodies ...)
                              (main "temporary")))))))]
      [else
       (raise-syntax-error 'Java
                           "Internal Syntax error in compiling Java Program"
                           stx)])))
