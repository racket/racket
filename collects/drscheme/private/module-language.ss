#lang scheme/base

(provide module-language@)
(require scheme/unit
         scheme/class
         scheme/list
         mred
         compiler/embed
         launcher
         framework
         string-constants
         "drsig.ss"
         scheme/contract)

(define op (current-output-port))
(define (oprintf . args) (apply fprintf op args))

(define-unit module-language@
  (import [prefix drscheme:language-configuration: drscheme:language-configuration/internal^]
          [prefix drscheme:language: drscheme:language^]
          [prefix drscheme:unit: drscheme:unit^]
          [prefix drscheme:rep: drscheme:rep^])
  (export drscheme:module-language^)
  
  (define module-language<%>
    (interface ()
      ))
  
  ;; add-module-language : -> void
  ;; adds the special module-only language to drscheme
  (define (add-module-language)
    (define module-language%
      (module-mixin
       ((drscheme:language:get-default-mixin)
        (drscheme:language:module-based-language->language-mixin
         (drscheme:language:simple-module-based-language->module-based-language-mixin
          drscheme:language:simple-module-based-language%)))))
    (drscheme:language-configuration:add-language
     (new module-language%)))
  
  ;; collection-paths : (listof (union 'default string))
  ;; command-line-args : (vectorof string)
  (define-struct (module-language-settings drscheme:language:simple-settings)
    (collection-paths command-line-args))
  
  ;; module-mixin : (implements drscheme:language:language<%>)
  ;;             -> (implements drscheme:language:language<%>)
  (define (module-mixin %)
    (class* % (drscheme:language:language<%> module-language<%>)
      (define/override (use-namespace-require/copy?) #t)
      (field [iteration-number 0])
      
      (define/augment (capability-value key)
        (cond
          [(eq? key 'drscheme:autocomplete-words)
           (drscheme:language-configuration:get-all-manual-keywords)]
          [else (drscheme:language:get-capability-default key)]))
      
      ;; config-panel : as in super class
      ;; uses drscheme:language:simple-module-based-language-config-panel
      ;; and adds a collection paths configuration to it.
      (define/override (config-panel parent)
        (module-language-config-panel parent))
      
      (define/override (default-settings)
        (let ([super-defaults (super default-settings)])
          (apply make-module-language-settings
                 (append 
                  (vector->list (drscheme:language:simple-settings->vector super-defaults))
                  (list '(default)
                        #())))))
      
      ;; default-settings? : -> boolean
      (define/override (default-settings? settings)
        (and (super default-settings? settings)
             (equal? (module-language-settings-collection-paths settings)
                     '(default))
             (equal? (module-language-settings-command-line-args settings)
                     #())))
      
      (define/override (marshall-settings settings)
        (let ([super-marshalled (super marshall-settings settings)])
          (list super-marshalled
                (module-language-settings-collection-paths settings)
                (module-language-settings-command-line-args settings))))
      
      (define/override (unmarshall-settings marshalled)
        (and (pair? marshalled)
             (pair? (cdr marshalled))
             (pair? (cddr marshalled))
             (null? (cdddr marshalled))
             (list? (cadr marshalled))
             (vector? (caddr marshalled))
             (andmap string? (vector->list (caddr marshalled)))
             (andmap (λ (x) (or (string? x) (symbol? x)))
                     (cadr marshalled))
             (let ([super (super unmarshall-settings (car marshalled))])
               (and super
                    (apply make-module-language-settings
                           (append
                            (vector->list (drscheme:language:simple-settings->vector super))
                            (list (cadr marshalled)
                                  (caddr marshalled))))))))
      
      (define/override (on-execute settings run-in-user-thread)
        (set! iteration-number 0)
        (super on-execute settings run-in-user-thread)
        (run-in-user-thread
         (λ ()
           (current-command-line-arguments (module-language-settings-command-line-args settings))
           (let ([default (current-library-collection-paths)])
             (current-library-collection-paths
              (apply 
               append
               (map (λ (x) (if (symbol? x)
                               default
                               (list x)))
                    (module-language-settings-collection-paths settings))))))))
      
      (define/override (get-one-line-summary)
        (string-constant module-language-one-line-summary))
      
      (inherit get-reader)
      (define/override (front-end/interaction port settings)
        (if (thread-cell-ref hopeless-repl)
            (begin (fprintf (current-error-port)
                            "Module Language: ~a\n" hopeless-message)
                   (λ x eof))
            (super front-end/interaction port settings)))
      
      (define/override (front-end/complete-program port settings)
        (let* ([super-thunk (λ () ((get-reader) (object-name port) port))]
               [path (get-filename port)]
               [module-name #f]
               [get-require-module-name
                (λ ()
                  ;; "clearing out" the module-name via datum->syntax ensures
                  ;; that check syntax doesn't think the original module name
                  ;; is being used in this require (so it doesn't get turned red)
                  (datum->syntax #'here (syntax-e module-name)))])
          (λ ()
            (set! iteration-number (+ iteration-number 1))
            (case iteration-number
              [(1)
               #`(current-module-declare-name
                  (if #,path
                      (make-resolved-module-path '#,path)
                      #f))]
              [(2)
               (let ([super-result (super-thunk)])
                 (if (eof-object? super-result)
                     (raise-syntax-error 'Module\ Language hopeless-message)
                     (let-values ([(name new-module)
                                   (transform-module path super-result)])
                       (set! module-name name)
                       new-module)))]
              [(3)
               (let ([super-result (super-thunk)])
                 (if (eof-object? super-result)
                     #`(current-module-declare-name #f)
                     (raise-syntax-error
                      'module-language
                      "there can only be one expression in the definitions window"
                      super-result)))]
              [(4)
               (if path
                   #`(begin ((current-module-name-resolver)
                             (make-resolved-module-path #,path))
                            (call-with-continuation-prompt
                             (λ () (dynamic-require #,path #f))))
                   #`(call-with-continuation-prompt
                      (λ () (dynamic-require ''#,(get-require-module-name) #f))))]
              [(5)
               (if path
                   #`(#%app current-namespace (#%app module->namespace #,path))
                   #`(#%app current-namespace
                            (#%app module->namespace
                                   ''#,(get-require-module-name))))]
              [else eof]))))
      
      ;; printer settings are just ignored here.
      (define/override (create-executable setting parent program-filename)
        (let* ([executable-specs (drscheme:language:create-executable-gui
                                  parent program-filename #t #t)])
          (when executable-specs
            (let ([launcher? (eq? 'launcher (car executable-specs))]
                  [gui? (eq? 'mred (cadr executable-specs))]
                  [executable-filename (caddr executable-specs)])
              (with-handlers ([(λ (x) #f) ;exn:fail?
                               (λ (x)
                                 (message-box
                                  (string-constant drscheme)
                                  (if (exn? x)
                                      (format "~a" (exn-message x))
                                      (format "uncaught exception: ~s" x))))])
                (if (not launcher?)
                    (let ([short-program-name
                           (let-values ([(base name dir) (split-path program-filename)])
                             (path-replace-suffix name #""))])
                      ((if (eq? 'distribution (car executable-specs))
                           drscheme:language:create-distribution-for-executable
                           (lambda (executable-filename gui? make)
                             (make executable-filename)))
                       executable-filename
                       gui?
                       (lambda (exe-name)
                         (create-embedding-executable
                          exe-name
                          #:mred? gui?
                          #:verbose? #f ;; verbose?
                          #:modules (list (list #f program-filename))
                          #:literal-expression
                          (begin
                            (parameterize ([current-namespace (make-base-empty-namespace)])
                              (namespace-require 'scheme/base)
                              (compile 
                               `(namespace-require '',(string->symbol (path->string short-program-name))))))
                          #:cmdline null))))
                    (let ([make-launcher (if gui? make-mred-launcher make-mzscheme-launcher)])
                      (make-launcher (list "-qt-" (path->string program-filename))
                                     executable-filename))))))))
      
      (super-new
       [module #f]
       [language-position (list "Module")]
       [language-numbers (list -32768)])))
  
  (define hopeless-repl (make-thread-cell #t))
  (define hopeless-message
    (string-append
     "There must be a module in the\n"
     "definitions window. Try starting your program with\n"
     "\n"
     "  #lang scheme\n"
     "\n"
     "and clicking ‘Run’."))
  
  ;; module-language-config-panel : panel -> (case-> (-> settings) (settings -> void))
  (define (module-language-config-panel parent)
    (define new-parent
      (new vertical-panel%
           [parent parent]
           [alignment '(center center)]
           [stretchable-height #f]
           [stretchable-width #f]))
    (define simple-case-lambda
      (drscheme:language:simple-module-based-language-config-panel new-parent))
    (define cp-panel (new group-box-panel%
                          [parent new-parent]
                          [label (string-constant ml-cp-collection-paths)]))
    
    (define args-panel (new group-box-panel%
                            [parent new-parent]
                            [label (string-constant ml-command-line-arguments)]))
    (define args-text-box (new text-field%
                               [parent args-panel]
                               [label #f]
                               [init-value "#()"]
                               [callback void]))
    
    ;; data associated with each item in listbox : boolean
    ;; indicates if the entry is the default paths.
    (define lb (new list-box%
                    [parent cp-panel]
                    [choices '("a" "b" "c")]
                    [label #f]
                    [callback (λ (x y) (update-buttons))]))
    (define button-panel (new horizontal-panel%
                              [parent cp-panel]
                              [alignment '(center center)]
                              [stretchable-height #f]))
    (define add-button
      (make-object button% (string-constant ml-cp-add) button-panel
        (λ (x y) (add-callback))))
    (define add-default-button
      (make-object button% (string-constant ml-cp-add-default) button-panel
        (λ (x y) (add-default-callback))))
    (define remove-button
      (make-object button% (string-constant ml-cp-remove) button-panel
        (λ (x y) (remove-callback))))
    (define raise-button
      (make-object button% (string-constant ml-cp-raise) button-panel
        (λ (x y) (move-callback -1))))
    (define lower-button
      (make-object button% (string-constant ml-cp-lower) button-panel
        (λ (x y) (move-callback +1))))
    
    (define (update-buttons)
      (let ([lb-selection (send lb get-selection)]
            [lb-tot (send lb get-number)])
        (send remove-button enable lb-selection)
        (send raise-button enable (and lb-selection (not (= lb-selection 0))))
        (send lower-button enable
              (and lb-selection (not (= lb-selection (- lb-tot 1)))))))
    
    (define (add-callback)
      (let ([dir (get-directory (string-constant ml-cp-choose-a-collection-path)
                                (send parent get-top-level-window))])
        (when dir
          (send lb append (path->string dir) #f)
          (update-buttons))))
    
    (define (add-default-callback)
      (cond [(has-default?)
             (message-box (string-constant drscheme)
                          (string-constant ml-cp-default-already-present)
                          (send parent get-top-level-window))]
            [else
             (send lb append (string-constant ml-cp-default-collection-path) #t)
             (update-buttons)]))
    
    ;; has-default? : -> boolean
    ;; returns #t if the `default' entry has already been added
    (define (has-default?)
      (let loop ([n (send lb get-number)])
        (cond [(= n 0) #f]
              [(send lb get-data (- n 1)) #t]
              [else (loop (- n 1))])))
    
    (define (remove-callback)
      (let ([to-delete (send lb get-selection)])
        (send lb delete to-delete)
        (unless (zero? (send lb get-number))
          (send lb set-selection (min to-delete (- (send lb get-number) 1))))
        (update-buttons)))
    
    (define (move-callback d)
      (let* ([sel (send lb get-selection)]
             [vec (get-lb-vector)]
             [new (+ sel d)]
             [other (vector-ref vec new)])
        (vector-set! vec new (vector-ref vec sel))
        (vector-set! vec sel other)
        (set-lb-vector vec)
        (send lb set-selection new)
        (update-buttons)))
    
    (define (get-lb-vector)
      (list->vector (for/list ([n (in-range (send lb get-number))])
                              (cons (send lb get-string n) (send lb get-data n)))))
    
    (define (set-lb-vector vec)
      (send lb clear)
      (for ([x (in-vector vec)] [n (in-naturals)])
           (send lb append (car x))
           (send lb set-data n (cdr x))))
    
    (define (get-collection-paths)
      (for/list ([n (in-range (send lb get-number))])
                (let ([data (send lb get-data n)])
                  (if data 'default (send lb get-string n)))))
    
    (define (install-collection-paths paths)
      (send lb clear)
      (for ([cp paths])
           (if (symbol? cp)
               (send lb append (string-constant ml-cp-default-collection-path) #t)
               (send lb append cp #f))))
    
    (define (get-command-line-args)
      (let* ([str (send args-text-box get-value)]
             [read-res (parameterize ([read-accept-graph #f])
                         (with-handlers ([exn:fail:read? (λ (x) #())])
                           (read (open-input-string str))))])
        (if (and (vector? read-res) (andmap string? (vector->list read-res)))
            read-res
            #())))
    
    (define (install-command-line-args vec)
      (send args-text-box set-value
            (parameterize ([print-vector-length #f])
              (format "~s" vec))))
    
    (send lb set '())
    (update-buttons)
    
    (case-lambda
      [()
       (let ([simple-settings (simple-case-lambda)])
         (apply make-module-language-settings
                (append 
                 (vector->list (drscheme:language:simple-settings->vector simple-settings))
                 (list (get-collection-paths)
                       (get-command-line-args)))))]
      [(settings)
       (simple-case-lambda settings)
       (install-collection-paths (module-language-settings-collection-paths settings))
       (install-command-line-args (module-language-settings-command-line-args settings))
       (update-buttons)]))
  
  ;; transform-module : (union #f string) syntax
  ;;   -> (values syntax[name-of-module] syntax[module])
  ;; = User =
  ;; in addition to exporting everything, the result module's name
  ;; is the fully path-expanded name with a directory prefix, 
  ;; if the file has been saved
  (define (transform-module filename stx)
    (syntax-case* stx (module) (λ (x y) (eq? (syntax-e x) (syntax-e y)))
      [(module name lang . rest)
       (eq? 'module (syntax-e #'module))
       (let ([v-name #'name])
         (when filename (check-filename-matches filename v-name stx))
         (thread-cell-set! hopeless-repl #f)
         (values
          v-name
          ;; rewrite the module to use the scheme/base version of `module'
          (let ([module (datum->syntax #'here 'module #'form)])
            (datum->syntax stx `(,module ,#'name ,#'lang . ,#'rest) stx))))]
      [else (raise-syntax-error
             'module-language
             (string-append "only a module expression is allowed, either\n"
                            "    #lang <language-name>\n or\n"
                            "    (module <name> <language> ...)\n")
             stx)]))
  
  ;; get-filename : port -> (union string #f)
  ;; extracts the file the definitions window is being saved in, if any.
  (define (get-filename port)
    (let ([source (object-name port)])
      (cond
        [(path? source) source]
        [(is-a? source text%)
         (let ([canvas (send source get-canvas)])
           (and canvas
                (let ([frame (send canvas get-top-level-window)])
                  (and (is-a? frame drscheme:unit:frame%)
                       (let* ([b (box #f)]
                              [filename (send (send frame get-definitions-text)
                                              get-filename
                                              b)])
                         (if (unbox b)
                             #f
                             filename))))))]
        [else #f])))
  
  ;; check-filename-matches : string syntax syntax -> void
  (define (check-filename-matches filename name unexpanded-stx)
    (define datum (syntax-e name))
    (unless (symbol? datum)
      (raise-syntax-error 'module-language
                          "bad syntax in name position of module"
                          unexpanded-stx name))
    (let-values ([(base name dir?) (split-path filename)])
      (let ([expected (string->symbol
                       (path->string (path-replace-suffix name #"")))])
        (unless (equal? expected datum)
          (raise-syntax-error
           'module-language
           (format "module name doesn't match saved filename, got ~s and expected ~a"
                   datum
                   expected)
           unexpanded-stx)))))
  
  (define module-language-put-file-mixin
    (mixin (text:basic<%>) ()
      (inherit get-text last-position get-character get-top-level-window)
      (define/override (put-file directory default-name)
        (let ([tlw (get-top-level-window)])
          (if (and tlw
                   (is-a? tlw drscheme:unit:frame<%>))
              (let* ([definitions-text (send tlw get-definitions-text)]
                     [module-language?
                      (is-a? (drscheme:language-configuration:language-settings-language
                              (send definitions-text get-next-settings))
                             module-language<%>)]
                     [module-default-filename
                      (and module-language? (get-module-filename))])
                (super put-file directory module-default-filename))
              (super put-file directory default-name))))
      
      ;; returns the name after "(module " suffixed with .scm
      ;; in the beginning of the editor
      ;; or #f if the beginning doesn't match "(module "
      (define/private (get-module-filename)
        (let ([open-paren (skip-whitespace 0)])
          (or (match-paren open-paren "(")
              (match-paren open-paren "[")
              (match-paren open-paren "{"))))
      
      (define/private (match-paren open-paren paren)
        (and (matches open-paren paren)
             (let ([module (skip-whitespace (+ open-paren 1))])
               (and (matches module "module")
                    (let* ([end-module (+ module (string-length "module"))]
                           [filename-start (skip-whitespace end-module)]
                           [filename-end (skip-to-whitespace filename-start)])
                      (and (not (= filename-start end-module))
                           (string-append (get-text filename-start filename-end)
                                          ".scm")))))))
      
      
      (define/private (matches start string)
        (let ([last-pos (last-position)])
          (let loop ([i 0])
            (cond
              [(and (i . < . (string-length string))
                    ((+ i start) . < . last-pos))
               (and (char=? (string-ref string i)
                            (get-character (+ i start)))
                    (loop (+ i 1)))]
              [(= i (string-length string)) #t]
              [else #f]))))
      
      (define/private (skip-whitespace start)
        (let ([last-pos (last-position)])
          (let loop ([pos start])
            (cond
              [(pos . >= . last-pos) last-pos]
              [else
               (let ([char (get-character pos)])
                 (cond
                   [(char-whitespace? char)
                    (loop (+ pos 1))]
                   [else pos]))]))))
      
      (define/private (skip-to-whitespace start)
        (let ([last-pos (last-position)])
          (let loop ([pos start])
            (cond 
              [(pos . >= . last-pos)
               last-pos]
              [(char-whitespace? (get-character pos))
               pos]
              [else
               (loop (+ pos 1))]))))
      
      (super-new))))
