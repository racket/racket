#lang scheme/base

(provide (all-defined-out))

(require string-constants/string-constant
         scheme/unit
         scheme/contract
         scheme/class
         drscheme/tool
         mzlib/list
         syntax/toplevel
         syntax/boundmap
         mrlib/bitmap-label
         (prefix-in drscheme:arrow: drscheme/arrow)
         (prefix-in fw: framework/framework)
         mred/mred
         setup/xref
         scribble/xref
         scribble/manual-struct
         net/url
         net/uri-codec
         browser/external
         (for-syntax scheme/base)
         "extra-stxcase.ss"
         "id-sets.ss"
         "extra-typed.ss"
         "utils.ss")


    ;                                                                                                             
    ;                                                                                                             
    ;                                                                                                             
    ;                                                                                                   ;         
    ;                                                                                                   ;         
    ;                         ;                       ;                                                 ;         
    ;    ;;;  ;     ; ; ;;   ;;;;  ;;;   ;     ;     ;;;;  ; ;  ;;;   ;     ;  ;;;   ; ;   ;;;   ;;;    ;    ;;;  
    ;   ;     ;     ; ;;  ;   ;   ;   ;   ;   ;       ;    ;;  ;   ;   ;   ;  ;   ;  ;;   ;     ;   ;   ;   ;     
    ;   ;;     ;   ;  ;   ;   ;       ;    ; ;        ;    ;       ;   ;   ; ;    ;  ;    ;;        ;   ;   ;;    
    ;    ;;    ;   ;  ;   ;   ;    ;;;;     ;         ;    ;    ;;;;    ; ;  ;;;;;;  ;     ;;    ;;;;   ;    ;;   
    ;      ;    ; ;   ;   ;   ;   ;   ;    ; ;        ;    ;   ;   ;    ; ;  ;       ;       ;  ;   ;   ;      ;  
    ;      ;    ; ;   ;   ;   ;   ;   ;   ;   ;       ;    ;   ;   ;     ;    ;      ;       ;  ;   ;   ;      ;  
    ;   ;;;      ;    ;   ;    ;;  ;;;;; ;     ;       ;;  ;    ;;;;;    ;     ;;;;  ;    ;;;    ;;;;;  ;   ;;;   
    ;            ;                                                                                                
    ;            ;                                                                                                
    ;           ;                                                                                                 
    
            
    
    ;; annotate-basic : syntax 
    ;;                  namespace
    ;;                  string[directory]
    ;;                  syntax[id]
    ;;                  id-set (six of them)
    ;;                  hash-table[require-spec -> syntax] (three of them)
    ;;               -> void
    (define (annotate-basic sexp user-namespace user-directory jump-to-id
                            low-binders high-binders 
                            low-varrefs high-varrefs 
                            low-tops high-tops
                            templrefs
                            requires require-for-syntaxes require-for-templates require-for-labels)
      (let ([tail-ht (make-hash-table)]
            [maybe-jump
             (λ (vars)
               (when jump-to-id
                 (for-each (λ (id)
                             (let ([binding (identifier-binding id)])
                               (when (pair? binding)
                                 (let ([nominal-source-id (list-ref binding 3)])
                                   (when (eq? nominal-source-id jump-to-id)
                                     (jump-to id))))))
                           (syntax->list vars))))])
        
        (let level-loop ([sexp sexp]
                         [high-level? #f])
          (let* ([loop (λ (sexp) (level-loop sexp high-level?))]
                 [varrefs (if high-level? high-varrefs low-varrefs)]
                 [binders (if high-level? high-binders low-binders)]
                 [tops (if high-level? high-tops low-tops)]
                 [collect-general-info
                  (λ (stx)
                    (add-origins stx varrefs)
                    (add-disappeared-bindings stx binders varrefs)
                    (add-disappeared-uses stx varrefs))])
            (collect-general-info sexp)
            (syntax-case* sexp (#%plain-lambda case-lambda if begin begin0 let-values letrec-values set!
                                 quote quote-syntax with-continuation-mark 
                                 #%plain-app #%top #%plain-module-begin
                                 define-values define-syntaxes define-values-for-syntax module
                                 #%require #%provide #%expression)
              (if high-level? free-transformer-identifier=? free-identifier=?)
              [(#%plain-lambda args bodies ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
                 (add-binders (syntax args) binders)
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              [(case-lambda [argss bodiess ...]...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each (λ (bodies/stx) (annotate-tail-position/last sexp 
                                                                        (syntax->list bodies/stx)
                                                                        tail-ht))
                           (syntax->list (syntax ((bodiess ...) ...))))
                 (for-each
                  (λ (args bodies)
                    (add-binders args binders)
                    (for-each loop (syntax->list bodies)))
                  (syntax->list (syntax (argss ...)))
                  (syntax->list (syntax ((bodiess ...) ...)))))]
              [(if test then else)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position sexp (syntax then) tail-ht)
                 (annotate-tail-position sexp (syntax else) tail-ht)
                 (loop (syntax test))
                 (loop (syntax else))
                 (loop (syntax then)))]
              [(begin bodies ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              ;; treat a single body expression specially, since this has
              ;; different tail behavior.
              [(begin0 body)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position sexp (syntax body) tail-ht)
                 (loop (syntax body)))]
              
              [(begin0 bodies ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              [(let-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x) (add-binders x binders))
                             (syntax->list (syntax ((xss ...) ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...))))))]
              [(letrec-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x) (add-binders x binders))
                             (syntax->list (syntax ((xss ...) ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...))))))]
              [(set! var e)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 
                 ;; tops are used here because a binding free use of a set!'d variable
                 ;; is treated just the same as (#%top . x).
                 (when (syntax-original? (syntax var))
                   (if (identifier-binding (syntax var))
                       (add-id varrefs (syntax var))
                       (add-id tops (syntax var))))
                 
                 (loop (syntax e)))]
              [(quote datum)
               ;(color-internal-structure (syntax datum) constant-style-name)
               (annotate-raw-keyword sexp varrefs)]
              [(quote-syntax datum)
               ;(color-internal-structure (syntax datum) constant-style-name)
               (annotate-raw-keyword sexp varrefs)
               (let loop ([stx #'datum])
                 (cond [(identifier? stx)
                        (when (syntax-original? stx)
                          (add-id templrefs stx))]
                       [(syntax? stx)
                        (loop (syntax-e stx))]
                       [(pair? stx)
                        (loop (car stx))
                        (loop (cdr stx))]
                       [(vector? stx)
                        (for-each loop (vector->list stx))]
                       [(box? stx)
                        (loop (unbox stx))]
                       [else (void)]))]
              [(with-continuation-mark a b c)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position sexp (syntax c) tail-ht)
                 (loop (syntax a))
                 (loop (syntax b))
                 (loop (syntax c)))]
              [(#%plain-app pieces ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each loop (syntax->list (syntax (pieces ...)))))]
              [(#%top . var)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (when (syntax-original? (syntax var))
                   (add-id tops (syntax var))))]
              [(define-values vars b)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (add-binders (syntax vars) binders)
                 (maybe-jump (syntax vars))
                 (loop (syntax b)))]
              [(define-syntaxes names exp)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (add-binders (syntax names) binders)
                 (maybe-jump (syntax names))
                 (level-loop (syntax exp) #t))]
              [(define-values-for-syntax names exp)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (add-binders (syntax names) high-binders)
                 (maybe-jump (syntax names))
                 (level-loop (syntax exp) #t))]
              [(module m-name lang (#%plain-module-begin bodies ...))
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 ((annotate-require-open user-namespace user-directory) (syntax lang))
                 
                  ;; temporarily removed until Matthew fixes whatever.
                 #;
                 (hash-table-put! requires 
                                  (syntax->datum (syntax lang))
                                  (cons (syntax lang)
                                        (hash-table-get requires 
                                                        (syntax->datum (syntax lang))
                                                        (λ () '()))))
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              ; top level or module top level only:
              [(#%require require-specs ...)
               (let ([at-phase
                      (lambda (stx requires)
                        (syntax-case stx ()
                          [(_ require-specs ...)
                           (let ([new-specs (map trim-require-prefix
                                                 (syntax->list (syntax (require-specs ...))))])
                             (annotate-raw-keyword sexp varrefs)
                             (for-each (annotate-require-open user-namespace user-directory) new-specs)
                             (for-each (add-require-spec requires)
                                       new-specs
                                       (syntax->list (syntax (require-specs ...)))))]))])
                 (for-each (lambda (spec)
                             (syntax-case* spec (for-syntax for-template for-label) (lambda (a b)
                                                                                      (eq? (syntax-e a) (syntax-e b)))
                               [(for-syntax specs ...)
                                (at-phase spec require-for-syntaxes)]
                               [(for-template specs ...)
                                (at-phase spec require-for-templates)]
                               [(for-label specs ...)
                                (at-phase spec require-for-labels)]
                               [else
                                (at-phase (list #f spec) requires)]))
                           (syntax->list #'(require-specs ...))))]
              
              ; module top level only:
              [(#%provide provide-specs ...)
               (let ([provided-varss (map extract-provided-vars
                                          (syntax->list (syntax (provide-specs ...))))])
                 (annotate-raw-keyword sexp varrefs)
                 (for-each (λ (provided-vars)
                             (for-each
                              (λ (provided-var)
                                (when (syntax-original? provided-var)
                                  (add-id varrefs provided-var)))
                              provided-vars))
                           provided-varss))]
              
              [(#%expression arg)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (loop #'arg))]
              [id
               (identifier? (syntax id))
               (when (syntax-original? sexp)
                 (add-id varrefs sexp))]
              [_
               (begin
                #;
                 (printf "unknown stx: ~e datum: ~e source: ~e\n"
                         sexp
                         (and (syntax? sexp)
                              (syntax->datum sexp))
                         (and (syntax? sexp)
                              (syntax-source sexp)))
                 (void))])))
        (add-tail-ht-links tail-ht)))
    
    ;; jump-to : syntax -> void
    (define (jump-to stx)
      (let ([src (find-source-editor stx)]
            [pos (syntax-position stx)]
            [span (syntax-span stx)])
        (when (and (is-a? src text%)
                   pos
                   span)
          (send src set-position (- pos 1) (+ pos span -1)))))
    
    
    ;; annotate-require-open : namespace string -> (stx -> void)
    ;; relies on current-module-name-resolver, which in turn depends on
    ;; current-directory and current-namespace
    (define (annotate-require-open user-namespace user-directory)
      (λ (require-spec)
        (when (syntax-original? require-spec)
          (let ([source (find-source-editor require-spec)])
            (when (and (is-a? source text%)
                       (syntax-position require-spec)
                       (syntax-span require-spec))
              (let ([defs-text (get-defs-text)])
                (when defs-text
                  (let* ([start (- (syntax-position require-spec) 1)]
                         [end (+ start (syntax-span require-spec))]
                         [file (get-require-filename (syntax->datum require-spec)
                                                     user-namespace
                                                     user-directory)])
                    (when file
                      (send defs-text syncheck:add-menu
                            source
                            start end 
                            #f
                            (make-require-open-menu file)))))))))))
    
    ;; hash-table[syntax -o> (listof syntax)] -> void
    (define (add-tail-ht-links tail-ht)
      (hash-table-for-each
       tail-ht
       (λ (stx-from stx-tos)
         (for-each (λ (stx-to) (add-tail-ht-link stx-from stx-to))
                   stx-tos))))
    
    ;; add-tail-ht-link : syntax syntax -> void
    (define (add-tail-ht-link from-stx to-stx)
      (let* ([to-src (find-source-editor to-stx)]
             [from-src (find-source-editor from-stx)]
             [defs-text (get-defs-text)])
        (when (and to-src from-src defs-text)
          (let ([from-pos (syntax-position from-stx)]
                [to-pos (syntax-position to-stx)])
            (when (and from-pos to-pos)
              (send defs-text syncheck:add-tail-arrow
                    from-src (- from-pos 1)
                    to-src (- to-pos 1)))))))
    
    ;; find-source : definitions-text source -> editor or false
    (define (find-source-editor stx)
      (let ([defs-text (get-defs-text)])
        (and defs-text 
             (let txt-loop ([text defs-text])
               (cond
                 [(and (is-a? text fw:text:basic<%>)
                       (send text port-name-matches? (syntax-source stx)))
                  text]
                 [else
                  (let snip-loop ([snip (send text find-first-snip)])
                    (cond
                      [(not snip)
                       #f]
                      [(and (is-a? snip editor-snip%)
                            (send snip get-editor))
                       (or (txt-loop (send snip get-editor))
                           (snip-loop (send snip next)))]
                      [else 
                       (snip-loop (send snip next))]))])))))
    
    ;; get-defs-text : -> text or false
    (define (get-defs-text)
      (let ([drs-frame (currently-processing-drscheme-frame)])
        (and drs-frame
             (send drs-frame get-definitions-text))))
    
    
    
    
    ;; record-renamable-var : rename-ht syntax -> void
    (define (record-renamable-var rename-ht stx)
      (let ([key (list (syntax-source stx) (syntax-position stx) (syntax-span stx))])
        (hash-table-put! rename-ht
                         key
                         (cons stx (hash-table-get rename-ht key (λ () '()))))))
    
    
    ;; connect-identifier : syntax
    ;;                      id-set 
    ;;                      (union #f hash-table)
    ;;                      (union #f hash-table)
    ;;                      (union identifier-binding identifier-transformer-binding)
    ;;                      (listof id-set)
    ;;                      namespace
    ;;                      directory
    ;;                      boolean
    ;;                   -> void
    ;; adds arrows and rename menus for binders/bindings
    (define (connect-identifier var rename-ht all-binders unused requires get-binding user-namespace user-directory actual?)
      (connect-identifier/arrow var all-binders unused requires get-binding user-namespace user-directory actual?)
      (when (and actual? (get-ids all-binders var))
        (record-renamable-var rename-ht var)))
    
    ;; connect-identifier/arrow : syntax
    ;;                            id-set 
    ;;                            (union #f hash-table)
    ;;                            (union #f hash-table)
    ;;                            (union identifier-binding identifier-transformer-binding)
    ;;                            boolean
    ;;                         -> void
    ;; adds the arrows that correspond to binders/bindings
    (define (connect-identifier/arrow var all-binders unused requires get-binding user-namespace user-directory actual?)
      (let ([binders (get-ids all-binders var)])
        (when binders
          (for-each (λ (x)
                      (when (syntax-original? x)
                        (connect-syntaxes x var actual?)))
                    binders))
        
        (when (and unused requires)
          (let ([req-path/pr (get-module-req-path (get-binding var))])
            (when req-path/pr
              (let* ([req-path (car req-path/pr)]
                     [id (cdr req-path/pr)]
                     [req-stxes (hash-table-get requires req-path (λ () #f))])
                (when req-stxes
                  (hash-table-remove! unused req-path)
                  (for-each (λ (req-stx) 
                              (when (id/require-match? (syntax->datum var) 
                                                       id 
                                                       (syntax->datum req-stx))
                                (when id
                                  (add-jump-to-definition
                                   var
                                   id
                                   (get-require-filename req-path user-namespace user-directory)))
                                (add-mouse-over var (fw:gui-utils:format-literal-label (string-constant cs-mouse-over-import)
                                                            (syntax-e var)
                                                            req-path))
                                (connect-syntaxes req-stx var actual?)))
                            req-stxes))))))))
    
    (define (id/require-match? var id req-stx)
      (cond
        [(and (pair? req-stx)
              (eq? (list-ref req-stx 0) 'prefix))
         (let ([prefix (list-ref req-stx 1)])
           (equal? (format "~a~a" prefix id)
                   (symbol->string var)))]
        [(and (pair? req-stx)
              (eq? (list-ref req-stx 0) 'prefix-all-except))
         (let ([prefix (list-ref req-stx 1)])
           (and (not (memq id (cdddr req-stx)))
                (equal? (format "~a~a" prefix id)
                        (symbol->string var))))]
        [(and (pair? req-stx)
              (eq? (list-ref req-stx 0) 'rename))
         (eq? (list-ref req-stx 2)
              var)]
        [else (eq? var id)]))
    
    
    ;; color/connect-top : namespace directory id-set syntax -> void
    (define (color/connect-top rename-ht user-namespace user-directory binders var)
      (let ([top-bound?
             (or (get-ids binders var)
                 (parameterize ([current-namespace user-namespace])
                   (let/ec k
                     (namespace-variable-value (syntax-e var) #t (λ () (k #f)))
                     #t)))])
        (if top-bound?
            (color var lexically-bound-variable-style-name)
            (color var error-style-name))
        (connect-identifier var rename-ht binders #f #f identifier-binding user-namespace user-directory #t)))
    
    
    ;; color-unused : hash-table[sexp -o> syntax] hash-table[sexp -o> #f] -> void
    (define (color-unused requires unused)
      (hash-table-for-each
       unused
       (λ (k v)
         (for-each (λ (stx) (color stx error-style-name))
                   (hash-table-get requires k)))))
    
       
    
    
    ;                                                          
    ;                                                          
    ;                                                          
    ;                                        ;                 
    ;                                        ;                 
    ;                                                          
    ;   ; ;;    ;;;   ;;;;    ;;;;  ;;;;;    ;    ;;;;    ;;;; 
    ;   ;;  ;  ;   ;  ;   ;  ;   ;  ; ; ;    ;    ;   ;  ;   ; 
    ;   ;   ;  ;;;;;  ;   ;  ;   ;  ; ; ;    ;    ;   ;  ;   ; 
    ;   ;      ;      ;   ;  ;   ;  ; ; ;    ;    ;   ;  ;   ; 
    ;   ;      ;   ;  ;   ;  ;  ;;  ; ; ;    ;    ;   ;  ;   ; 
    ;   ;       ;;;   ;   ;   ;; ;  ; ; ;    ;    ;   ;   ;;;; 
    ;                                                        ; 
    ;                                                    ;   ; 
    ;                                                     ;;;  
    
    
    ;; make-rename-menu : (cons stx[original,id] (listof stx[original,id])) (listof id-set) -> void
    (define (make-rename-menu stxs id-sets)
      (let ([defs-frame (currently-processing-drscheme-frame)])
        (when defs-frame
          (let* ([defs-text (send defs-frame get-definitions-text)]
                 [source (syntax-source (car stxs))]) ;; all stxs in the list must have the same source
            (when (and (send defs-text port-name-matches? source)
                       (send defs-text port-name-matches? source))
              (let* ([name-to-offer (format "~a" (syntax->datum (car stxs)))]
                     [start (- (syntax-position (car stxs)) 1)]
                     [fin (+ start (syntax-span (car stxs)))])
                (send defs-text syncheck:add-menu
                      defs-text
                      start 
                      fin 
                      (syntax-e (car stxs))
                      (λ (menu)
                        (instantiate menu-item% ()
                          (parent menu)
                          (label (fw:gui-utils:format-literal-label (string-constant cs-rename-var) name-to-offer))
                          (callback
                           (λ (x y)
                             (let ([frame-parent (find-menu-parent menu)])
                               (rename-callback name-to-offer
                                                defs-text
                                                stxs
                                                id-sets
                                                frame-parent)))))))))))))
    
    ;; find-parent : menu-item-container<%> -> (union #f (is-a?/c top-level-window<%>)
    (define (find-menu-parent menu)
      (let loop ([menu menu])
        (cond
          [(is-a? menu menu-bar%) (send menu get-frame)]
          [(is-a? menu popup-menu%)
           (let ([target (send menu get-popup-target)])
             (cond
               [(is-a? target editor<%>) 
                (let ([canvas (send target get-canvas)])
                  (and canvas
                       (send canvas get-top-level-window)))]
               [(is-a? target window<%>) 
                (send target get-top-level-window)]
               [else #f]))]
          [(is-a? menu menu-item<%>) (loop (send menu get-parent))]
          [else #f])))
    
    ;; rename-callback : string 
    ;;                   (and/c syncheck-text<%> definitions-text<%>)
    ;;                   (listof syntax[original])
    ;;                   (listof id-set) 
    ;;                   (union #f (is-a?/c top-level-window<%>)) 
    ;;                -> void
    ;; callback for the rename popup menu item
    (define (rename-callback name-to-offer defs-text stxs id-sets parent)
      (let ([new-str
             (fw:keymap:call/text-keymap-initializer
              (λ ()
                (get-text-from-user
                 (string-constant cs-rename-id)
                 (fw:gui-utils:format-literal-label (string-constant cs-rename-var-to) name-to-offer)
                 parent
                 name-to-offer)))])
        (when new-str
          (let ([new-sym (format "~s" (string->symbol new-str))])
            (let* ([to-be-renamed 
                    (remove-duplicates
                     (sort 
                      (apply 
                       append
                       (map (λ (id-set) 
                              (apply
                               append
                               (map (λ (stx) (or (get-ids id-set stx) '())) stxs)))
                            id-sets))
                      (λ (x y) 
                        ((syntax-position x) . >= . (syntax-position y)))))]
                   [do-renaming?
                    (or (not (name-duplication? to-be-renamed id-sets new-sym))
                        (equal?
                         (message-box/custom
                          (string-constant check-syntax)
                          (fw:gui-utils:format-literal-label (string-constant cs-name-duplication-error) 
                                  new-sym)
                          (string-constant cs-rename-anyway)
                          (string-constant cancel)
                          #f
                          parent
                          '(stop default=2))
                         1))])
              (when do-renaming?
                (unless (null? to-be-renamed)
                  (send defs-text begin-edit-sequence)
                  (for-each (λ (stx) 
                              (let ([source (syntax-source stx)])
                                (when (send defs-text port-name-matches? source)
                                  (let* ([start (- (syntax-position stx) 1)]
                                         [end (+ start (syntax-span stx))])
                                    (send defs-text delete start end #f)
                                    (send defs-text insert new-sym start start #f)))))
                            to-be-renamed)
                  (send defs-text invalidate-bitmap-cache)
                  (send defs-text end-edit-sequence))))))))
    
       ;; get-require-filename : sexp namespace string[directory] -> filename
    ;; finds the filename corresponding to the require in stx
    (define (get-require-filename datum user-namespace user-directory)
      (let ([mp
             (parameterize ([current-namespace user-namespace]
                            [current-directory user-directory]
                            [current-load-relative-directory user-directory])
               (with-handlers ([exn:fail? (λ (x) #f)])
                 ((current-module-name-resolver) datum #f #f)))])
        (and (resolved-module-path? mp)
             (resolved-module-path-name mp))))
    
    
    ;; make-require-open-menu : path -> menu -> void
    (define (make-require-open-menu file)
      (λ (menu)
        (let-values ([(base name dir?) (split-path file)])
          (instantiate menu-item% ()
            (label (fw:gui-utils:format-literal-label (string-constant cs-open-file) (path->string name)))
            (parent menu)
            (callback (λ (x y) (fw:handler:edit-file file))))
          (void))))