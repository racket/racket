
(module module-language mzscheme
  (provide module-language@)
  (require (lib "unit.ss")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "embed.ss" "compiler")
           (lib "launcher.ss" "launcher")
           (lib "framework.ss" "framework")
           (lib "string-constant.ss" "string-constants")
           "drsig.ss"
           (lib "contract.ss"))
  
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
         (instantiate module-language% ())))
      
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
                 (and (equal? (module-language-settings-collection-paths settings)
                              '(default))
                      (eq? (module-language-settings-command-line-args settings)
                           #()))))
          
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
          
          (define/override (get-style-delta) module-language-style-delta)
          
          (inherit get-reader)
          (define/override (front-end/complete-program port settings)
            (let* ([super-thunk (λ () ((get-reader) (object-name port) port))]
                   [filename (get-filename port)]
                   [module-name #f]
                   [module-name-prefix (get-module-name-prefix filename)]
                   [get-full-module-name
                    (λ ()
                      ;; "clearing out" the module-name via datum->syntax-object ensures
                      ;; that check syntax doesn't think the original module name
                      ;; is being used in this require (so it doesn't get turned red)
                      (datum->syntax-object #'here 
                                            (string->symbol
                                             (format "~a~a"
                                                     (or module-name-prefix "")
                                                     (syntax-e module-name)))))])
              (λ ()
                (set! iteration-number (+ iteration-number 1))
                (cond
                  [(= 1 iteration-number)
                   #`(current-module-name-prefix '#,module-name-prefix)]
                  [(= 2 iteration-number)
                   (let ([super-result (super-thunk)])
                     (if (eof-object? super-result)
                         (raise-syntax-error
                          'module-language
                          "the definitions window must contain a module")
                         (let-values ([(name new-module)
                                       (transform-module filename super-result super-result)])
                           (set! module-name name)
                           new-module)))]
                  [(= 3 iteration-number)
                   (let ([super-result (super-thunk)])
                     (if (eof-object? super-result)
                         #`(begin 
                             (current-module-name-prefix #f)
                             (call-with-continuation-prompt
                              (λ () (eval '(require #,(get-full-module-name))))))
                         (raise-syntax-error
                          'module-language
                          "there can only be one expression in the definitions window"
                          super-result)))]
                  [(= 4 iteration-number)
                   #`(eval '(current-namespace (module->namespace '#,(get-full-module-name))))]
                  [else eof]))))
          
          ;; printer settings are just ignored here.
          (define/override (create-executable setting parent program-filename)
            (let* ([executable-specs (drscheme:language:create-executable-gui
                                      parent 
                                      program-filename
                                      #t
                                      #t)])
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
                             (make-embedding-executable
                              exe-name
                              gui?
                              #f ;; verbose?
                              (list (list #f program-filename))
                              null
                              null
                              (list (if gui? "-Zmvqe-" "-mvqe-")
                                    (format "~s" `(require ,(string->symbol (path->string short-program-name)))))))))
                        (let ([make-launcher (if gui? make-mred-launcher make-mzscheme-launcher)])
                          (make-launcher (list "-mvqt-" (path->string program-filename))
                                         executable-filename))))))))
          
          (super-new
            (module '(lib "plt-mred.ss" "lang"))
            (language-position (list (string-constant professional-languages) "(module ...)"))
            (language-numbers (list -1000 1000)))))
      
      ;; module-language-config-panel : panel -> (case-> (-> settings) (settings -> void))
      (define (module-language-config-panel parent)
        (define new-parent 
          (instantiate vertical-panel% ()
            (parent parent)
            (alignment '(center center))
            (stretchable-height #f)
            (stretchable-width #f)))
        (define simple-case-lambda (drscheme:language:simple-module-based-language-config-panel new-parent))
        (define cp-panel (instantiate group-box-panel% ()
                           (parent new-parent)
                           (label (string-constant ml-cp-collection-paths))))
        
        (define args-panel (instantiate group-box-panel% ()
                             (parent new-parent)
                             (label (string-constant ml-command-line-arguments))))
        (define args-text-box (new text-field%
                                   (parent args-panel)
                                   (label #f)
                                   (init-value "#()")
                                   (callback void)))
        
        ;; data associated with each item in listbox : boolean
        ;; indicates if the entry is the default paths.
        (define lb (instantiate list-box% ()
                     (parent cp-panel)
                     (choices '("a" "b" "c"))
                     (label #f)
                     (callback (λ (x y) (update-buttons)))))
        (define button-panel (instantiate horizontal-panel% ()
                               (parent cp-panel)
                               (alignment '(center center))
                               (stretchable-height #f)))
        (define add-button (make-object button% (string-constant ml-cp-add) button-panel
                             (λ (x y) (add-callback))))
        (define add-default-button (make-object button% (string-constant ml-cp-add-default) button-panel
                                     (λ (x y) (add-default-callback))))
        (define remove-button (make-object button% (string-constant ml-cp-remove) button-panel
                                (λ (x y) (remove-callback))))
        (define raise-button (make-object button% (string-constant ml-cp-raise) button-panel
                               (λ (x y) (raise-callback))))
        (define lower-button (make-object button% (string-constant ml-cp-lower) button-panel
                               (λ (x y) (lower-callback))))
        
        (define (update-buttons)
          (let ([lb-selection (send lb get-selection)]
                [lb-tot (send lb get-number)])
            (send remove-button enable lb-selection)
            (send raise-button enable 
                  (and lb-selection
                       (not (= lb-selection 0))))
            (send lower-button enable 
                  (and lb-selection
                       (not (= lb-selection (- lb-tot 1)))))))
                
        (define (add-callback)
          (let ([dir (get-directory
                      (string-constant ml-cp-choose-a-collection-path)
                      (send parent get-top-level-window))])
            (when dir
              (send lb append (path->string dir) #f)
              (update-buttons))))
        
        (define (add-default-callback)
          (cond
            [(has-default?)
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
            (cond
              [(= n 0) #f]
              [(send lb get-data (- n 1)) #t]
              [else (loop (- n 1))])))
        
        (define (remove-callback)
          (let ([to-delete (send lb get-selection)])
            (send lb delete to-delete)
            (unless (zero? (send lb get-number))
              (send lb set-selection (min to-delete
                                          (- (send lb get-number) 1))))
            (update-buttons)))
        
        (define (lower-callback)
          (let* ([sel (send lb get-selection)]
                 [vec (get-lb-vector)]
                 [below (vector-ref vec (+ sel 1))])
            (vector-set! vec (+ sel 1) (vector-ref vec sel))
            (vector-set! vec sel below)
            (set-lb-vector vec)
            (send lb set-selection (+ sel 1))
            (update-buttons)))
        
        (define (raise-callback) 
          (let* ([sel (send lb get-selection)]
                 [vec (get-lb-vector)]
                 [above (vector-ref vec (- sel 1))])
            (vector-set! vec (- sel 1) (vector-ref vec sel))
            (vector-set! vec sel above)
            (set-lb-vector vec)
            (send lb set-selection (- sel 1))
            (update-buttons)))
        
        (define (get-lb-vector)
          (list->vector
           (let loop ([n 0])
             (cond
               [(= n (send lb get-number)) null]
               [else (cons (cons (send lb get-string n)
                                 (send lb get-data n))
                           (loop (+ n 1)))]))))
        
        (define (set-lb-vector vec)
          (send lb clear)
          (let loop ([n 0])
            (cond
              [(= n (vector-length vec)) (void)]
              [else (send lb append (car (vector-ref vec n)))
                    (send lb set-data n (cdr (vector-ref vec n)))
                    (loop (+ n 1))])))
        
        (define (get-collection-paths)
          (let loop ([n 0])
            (cond
              [(= n (send lb get-number)) null]
              [else
               (let ([data (send lb get-data n)])
                 (cons (if data
                           'default
                           (send lb get-string n))
                       (loop (+ n 1))))])))
        
        (define (install-collection-paths paths)
          (send lb clear)
          (for-each (λ (cp)
                      (if (symbol? cp)
                          (send lb append
                                (string-constant ml-cp-default-collection-path)
                                #t)
                          (send lb append cp #f)))
                    paths))
        
        (define (get-command-line-args)
          (let ([str (send args-text-box get-value)])
            (let ([read-res (parameterize ([read-accept-graph #f])
                              (with-handlers ([exn:fail:read? (λ (x) #())])
                                (read (open-input-string str))))])
              (cond
                [(and (vector? read-res)
                      (andmap string? (vector->list read-res)))
                 read-res]
                [else #()]))))
        
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
      
      ;; module-language-style-delta : (instanceof style-delta%)
      (define module-language-style-delta (make-object style-delta% 'change-family 'modern))
      
      ;; transform-module : (union #f string) syntax syntax -> (values symbol[name-of-module] syntax[module])
      ;; in addition to exporting everything, the result module's name
      ;; is the fully path-expanded name with a directory prefix, 
      ;; if the file has been saved
      (define (transform-module filename stx unexpanded-stx)
        (syntax-case stx (module)
          [(module name lang bodies ...)
           (let ([v-name (syntax name)])
             (when filename
               (check-filename-matches filename
                                       (syntax-object->datum (syntax name)) 
                                       unexpanded-stx))
             
             
             (values v-name
                     stx)
             
             ;; this isn't working ...
             #;
             (let ([new-name (if filename 
                                 (build-name filename)
                                 v-name)])
               (values new-name
                       #`(module #,new-name lang (#%plain-module-begin bodies ...)))))]
          [else
           (raise-syntax-error 'module-language
                               "only module expressions are allowed"
                               unexpanded-stx)]))
      
      ;; get-module-name-prefix : path -> string
      ;; returns the symbol that gets passed the current-module-name-prefix
      ;; while evaluating/expanding the module.
      (define (get-module-name-prefix path)
        (and path
             (let-values ([(base name dir) 
                           (split-path (normal-case-path (simplify-path (expand-path path) #f)))])
               (string->symbol (format ",~a" (bytes->string/latin-1 (path->bytes base)))))))
        
      ;; build-name : path -> symbol
      (define (build-name pre-path)
        (let ([path (normal-case-path (simplify-path (expand-path pre-path) #f))])
          (let-values ([(base name dir) (split-path path)])
            (string->symbol (format ",~a" 
                                    (bytes->string/latin-1
                                      (path->bytes
                                       (build-path 
                                        base
                                        (remove-suffix (path->string name))))))))))
      
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
      
      ;; check-filename-matches : string datum syntax -> void
      (define (check-filename-matches filename datum unexpanded-stx)
        (unless (symbol? datum)
          (raise-syntax-error 'module-language "unexpected object in name position of module" 
                              unexpanded-stx))
        (let-values ([(base name dir?) (split-path filename)])
          (let* ([expected (string->symbol (remove-suffix (path->string name)))])
            (unless (equal? expected datum)
              (raise-syntax-error
               'module-language
               (format "module name doesn't match saved filename, got ~s and expected ~a"
                       datum
                       expected)
               unexpanded-stx)))))
      
      (define re:check-filename-matches #rx"^(.*)\\.[^.]*$")
      (define (remove-suffix str)
        (let ([m (regexp-match re:check-filename-matches str)])
          (if m
              (cadr m)
              str)))
        

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
          
          (super-new)))))