#lang racket/base

(require "colors.rkt"
         "intf.rkt"
         "annotate.rkt"
         "contract-traversal.rkt"
         string-constants
         racket/unit
         racket/contract
         racket/class
         racket/list
         racket/pretty
         drracket/tool
         syntax/toplevel
         syntax/boundmap
         mrlib/switchable-button
         (prefix-in drracket:arrow: drracket/arrow)
         (prefix-in fw: framework/framework)
         mred
         framework
         setup/xref
         scribble/xref
         scribble/manual-struct
         net/url
         net/uri-codec
         browser/external
         (for-syntax racket/base)
         "../../syncheck-drracket-button.rkt")


(provide make-traversal)    
    
    
    
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
    
    
    
    ;; make-traversal : -> (values (namespace syntax (union #f syntax) -> void)
    ;;                             (namespace string[directory] -> void))
    ;; returns a pair of functions that close over some state that
    ;; represents the top-level of a single program. The first value
    ;; is called once for each top-level expression and the second
    ;; value is called once, after all expansion is complete.
    (define (make-traversal)
      (let* ([tl-low-binders (make-id-set)]
             [tl-high-binders (make-id-set)]
             [tl-low-varrefs (make-id-set)]
             [tl-high-varrefs (make-id-set)]
             [tl-low-varsets (make-id-set)]
             [tl-high-varsets (make-id-set)]
             [tl-low-tops (make-id-set)]
             [tl-high-tops (make-id-set)]
             [tl-templrefs (make-id-set)]
             [tl-requires (make-hash)]
             [tl-require-for-syntaxes (make-hash)]
             [tl-require-for-templates (make-hash)]
             [tl-require-for-labels (make-hash)]
             [expanded-expression
              (λ (user-namespace user-directory sexp jump-to-id)
                (parameterize ([current-load-relative-directory user-directory])
                  (let ([is-module? (syntax-case sexp (module)
                                      [(module . rest) #t]
                                      [else #f])])
                    (cond
                      [is-module?
                       (let ([low-binders (make-id-set)]
                             [high-binders (make-id-set)]
                             [varrefs (make-id-set)]
                             [high-varrefs (make-id-set)]
                             [varsets (make-id-set)]
                             [high-varsets (make-id-set)]
                             [low-tops (make-id-set)]
                             [high-tops (make-id-set)]
                             [templrefs (make-id-set)]
                             [requires (make-hash)]
                             [require-for-syntaxes (make-hash)]
                             [require-for-templates (make-hash)]
                             [require-for-labels (make-hash)])
                          (annotate-basic sexp
                                          user-namespace user-directory jump-to-id
                                          low-binders high-binders
                                          varrefs high-varrefs
                                          varsets high-varsets
                                          low-tops high-tops
                                          templrefs
                                          requires require-for-syntaxes require-for-templates require-for-labels) 
                         (annotate-variables user-namespace
                                             user-directory
                                             low-binders
                                             high-binders
                                             varrefs
                                             high-varrefs
                                             varsets
                                             high-varsets
                                             low-tops
                                             high-tops
                                             templrefs
                                             requires
                                             require-for-syntaxes
                                             require-for-templates
                                             require-for-labels)
                         (annotate-contracts sexp low-binders varrefs))]
                      [else
                       (annotate-basic sexp
                                       user-namespace user-directory jump-to-id
                                       tl-low-binders tl-high-binders
                                       tl-low-varrefs tl-high-varrefs
                                       tl-low-varsets tl-high-varsets
                                       tl-low-tops tl-high-tops
                                       tl-templrefs
                                       tl-requires
                                       tl-require-for-syntaxes
                                       tl-require-for-templates
                                       tl-require-for-labels)]))))]
             [expansion-completed
              (λ (user-namespace user-directory)
                (parameterize ([current-load-relative-directory user-directory])
                  (annotate-variables user-namespace
                                      user-directory
                                      tl-low-binders
                                      tl-high-binders
                                      tl-low-varrefs
                                      tl-high-varrefs
                                      tl-low-varsets
                                      tl-high-varsets
                                      tl-low-tops
                                      tl-high-tops
                                      tl-templrefs
                                      tl-requires
                                      tl-require-for-syntaxes
                                      tl-require-for-templates
                                      tl-require-for-labels)))])
        (values expanded-expression expansion-completed)))
    
    
    ;; type req/tag = (make-req/tag syntax sexp boolean)
    (define-struct req/tag (req-stx req-sexp used?))
    
    ;; annotate-basic : syntax 
    ;;                  namespace
    ;;                  string[directory]
    ;;                  syntax[id]
    ;;                  id-set (8 of them)
    ;;                  hash-table[require-spec -> syntax] (three of them)
    ;;               -> void
    (define (annotate-basic sexp 
                            user-namespace user-directory jump-to-id
                            low-binders high-binders 
                            low-varrefs high-varrefs 
                            low-varsets high-varsets
                            low-tops high-tops
                            templrefs
                            requires require-for-syntaxes require-for-templates require-for-labels)
      
      (let ([tail-ht (make-hasheq)]
            [maybe-jump
             (λ (vars)
               (when jump-to-id
                 (for-each (λ (id)
                             (let ([binding (identifier-binding id 0)])
                               (when (pair? binding)
                                 (let ([nominal-source-id (list-ref binding 3)])
                                   (when (eq? nominal-source-id jump-to-id)
                                     (jump-to id))))))
                           (syntax->list vars))))])

        (let level-loop ([sexp sexp]
                         [high-level? #f])
          
          (let* ([loop (λ (sexp) (level-loop sexp high-level?))]
                 [varrefs (if high-level? high-varrefs low-varrefs)]
                 [varsets (if high-level? high-varsets low-varsets)]
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
                   (add-id varsets (syntax var))
                   (if (identifier-binding (syntax var) 0)
                       (add-id varrefs (syntax var))
                       (add-id tops (syntax var))))
                 
                 (loop (syntax e)))]
              [(quote datum)
               ;(color-internal-structure (syntax datum) constant-style-name 'default-mode)
               (annotate-raw-keyword sexp varrefs)]
              [(quote-syntax datum)
               ;(color-internal-structure (syntax datum) constant-style-name 'default-mode)
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
                 
                 (hash-cons! requires (syntax->datum (syntax lang)) (syntax lang))
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              ; top level or module top level only:
              [(#%require require-specs ...)
               (let ([at-phase
                      (lambda (stx requires)
                        (syntax-case stx ()
                          [(_ require-specs ...)
                           (with-syntax ([((require-specs ...) ...)
                                          (map (lambda (spec)
                                                 (syntax-case spec (just-meta)
                                                   [(just-meta m spec ...)
                                                    #'(spec ...)]
                                                   [else (list spec)]))
                                               (syntax->list #'(require-specs ...)))])
                             (let ([new-specs (map trim-require-prefix
                                                   (syntax->list (syntax (require-specs ... ...))))])
                               (annotate-raw-keyword sexp varrefs)
                               (for-each (annotate-require-open user-namespace
                                                                user-directory)
                                         new-specs)
                               (for-each (add-require-spec requires)
                                         new-specs
                                         (syntax->list (syntax (require-specs ... ...))))))]))])
                 (for-each (lambda (spec)
                             (let loop ([spec spec])
                               (syntax-case* spec (for-syntax for-template for-label for-meta just-meta) 
                                             (lambda (a b)
                                               (eq? (syntax-e a) (syntax-e b)))
                                 [(just-meta phase specs ...)
                                  (for-each loop (syntax->list #'(specs ...)))]
                                 [(for-syntax specs ...)
                                  (at-phase spec require-for-syntaxes)]
                                 [(for-meta 1 specs ...)
                                  (at-phase #'(for-syntax specs ...) require-for-syntaxes)]
                                 [(for-template specs ...)
                                  (at-phase spec require-for-templates)]
                                 [(for-meta -1 specs ...)
                                  (at-phase #'(for-template specs ...) require-for-templates)]
                                 [(for-label specs ...)
                                  (at-phase spec require-for-labels)]
                                 [(for-meta #f specs ...)
                                  (at-phase #'(for-label specs ...) require-for-labels)]
                                 [(for-meta 0 specs ...)
                                  (at-phase #'(for-run specs ...) requires)]
                                 [(for-meta . _) (void)]
                                 [else
                                  (at-phase (list #f spec) requires)])))
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
    
    (define (hash-cons! ht k v)
      (hash-set! ht k (cons v (hash-ref ht k '()))))
    
    ;; add-disappeared-bindings : syntax id-set -> void
    (define (add-disappeared-bindings stx binders disappaeared-uses)
      (let ([prop (syntax-property stx 'disappeared-binding)])
        (when prop
          (let loop ([prop prop])
            (cond
              [(pair? prop)
               (loop (car prop))
               (loop (cdr prop))]
              [(identifier? prop)
               (add-origins prop disappaeared-uses)
               (add-id binders prop)])))))
    
    ;; add-disappeared-uses : syntax id-set -> void
    (define (add-disappeared-uses stx id-set)
      (let ([prop (syntax-property stx 'disappeared-use)])
        (when prop
          (let loop ([prop prop])
            (cond
              [(pair? prop)
               (loop (car prop))
               (loop (cdr prop))]
              [(identifier? prop)
               (add-id id-set prop)])))))
    
    ;; add-require-spec : hash-table[sexp[require-spec] -o> (listof syntax)]
    ;;                 -> sexp[require-spec]
    ;;                    syntax
    ;;                 -> void
    (define (add-require-spec require-ht)
      (λ (raw-spec syntax)
        (when (syntax-original? syntax)
          (let ([key (syntax->datum raw-spec)])
            (hash-set! require-ht
                       key
                       (cons syntax
                             (hash-ref require-ht
                                       key
                                       (λ () '()))))))))
    
    ;; annotate-variables : namespace directory string id-set[four of them] (listof syntax) (listof syntax) -> void
    ;; colors in and draws arrows for variables, according to their classifications
    ;; in the various id-sets
    (define (annotate-variables user-namespace
                                user-directory
                                low-binders
                                high-binders
                                low-varrefs
                                high-varrefs
                                low-varsets
                                high-varsets
                                low-tops
                                high-tops
                                templrefs
                                requires
                                require-for-syntaxes
                                require-for-templates
                                require-for-labels)
      
      (let ([rename-ht
             ;; hash-table[(list source number number) -> (listof syntax)]
             (make-hash)]
            [unused-requires (make-hash)]
            [unused-require-for-syntaxes (make-hash)]
            [unused-require-for-templates (make-hash)]
            [unused-require-for-labels (make-hash)]
            [requires/phases (make-hash)]
            [unused/phases (make-hash)]
            ;; there is no define-for-template form, thus no for-template binders
            [template-binders (make-id-set)]
            [label-binders (make-id-set)]
            [id-sets (list low-binders high-binders low-varrefs high-varrefs low-tops high-tops)])
        
        (hash-set! requires/phases 0 requires)
        (hash-set! requires/phases 1 require-for-syntaxes)
        (hash-set! requires/phases -1 require-for-templates)
        (hash-set! requires/phases #f require-for-labels)

        (hash-set! unused/phases 0 unused-requires)
        (hash-set! unused/phases 1 unused-require-for-syntaxes)
        (hash-set! unused/phases -1 unused-require-for-templates)
        (hash-set! unused/phases #f unused-require-for-labels)
        
        (hash-for-each requires
                       (λ (k v) (hash-set! unused-requires k #t)))
        (hash-for-each require-for-syntaxes
                       (λ (k v) (hash-set! unused-require-for-syntaxes k #t)))
        (hash-for-each require-for-templates
                       (lambda (k v) (hash-set! unused-require-for-templates k #t)))
        (hash-for-each require-for-labels
                       (lambda (k v) (hash-set! unused-require-for-labels k #t)))
        
        (let ([handle-var-bind
               (λ (var varsets)
                 (when (syntax-original? var)
                   (color-variable var 0 varsets)
                   (document-variable var 0)
                   (record-renamable-var rename-ht var)))])
          (for-each (λ (vars) 
                      (for-each (λ (var) (handle-var-bind var high-varsets)) 
                                vars))
                    (get-idss high-binders))
          (for-each (λ (vars) 
                      (for-each (λ (var) (handle-var-bind var low-varsets)) 
                                vars))
                    (get-idss low-binders)))
        
        
        (let ([handle-var-ref
               (λ (var index binders varsets)
                 (color-variable var index varsets)
                 (when (syntax-original? var)
                   (document-variable var index))
                 (connect-identifier var
                                     rename-ht
                                     binders
                                     unused/phases
                                     requires/phases
                                     index
                                     user-namespace 
                                     user-directory
                                     #t))])
          (for-each (λ (vars) (for-each 
                               (λ (var) (handle-var-ref var 0 low-binders low-varsets))
                               vars))
                    (get-idss low-varrefs))
          
          (for-each (λ (vars) (for-each 
                               (λ (var) (handle-var-ref var 1 high-binders high-varsets))
                               vars))
                    (get-idss high-varrefs)))
        
        (for-each (lambda (vars) (for-each
                                  (lambda (var)
                                    ;; no color variable
                                    (connect-identifier var
                                                        rename-ht
                                                        low-binders
                                                        unused/phases
                                                        requires/phases
                                                        0
                                                        user-namespace
                                                        user-directory
                                                        #f)
                                    (connect-identifier var
                                                        rename-ht
                                                        high-binders
                                                        unused/phases
                                                        requires/phases
                                                        1
                                                        user-namespace
                                                        user-directory
                                                        #f)
                                    (connect-identifier var
                                                        rename-ht
                                                        template-binders ;; dummy; always empty
                                                        unused/phases
                                                        requires/phases
                                                        -1
                                                        user-namespace
                                                        user-directory
                                                        #f)
                                    (connect-identifier var
                                                        rename-ht
                                                        label-binders ;; dummy; always empty
                                                        unused/phases
                                                        requires/phases
                                                        #f
                                                        user-namespace
                                                        user-directory
                                                        #f))
                                  vars))
                  (get-idss templrefs))
        
        (for-each 
         (λ (vars) 
           (for-each
            (λ (var) 
              (color/connect-top rename-ht user-namespace user-directory low-binders var))
            vars))
         (get-idss low-tops))
        
        (for-each 
         (λ (vars) 
           (for-each
            (λ (var) 
              (color/connect-top rename-ht user-namespace user-directory high-binders var))
            vars))
         (get-idss high-tops))
        
        (color-unused require-for-labels unused-require-for-labels)
        (color-unused require-for-templates unused-require-for-templates)
        (color-unused require-for-syntaxes unused-require-for-syntaxes)
        (color-unused requires unused-requires)
        (hash-for-each rename-ht (lambda (k stxs) (make-rename-menu stxs id-sets)))))
    
        
    ;; record-renamable-var : rename-ht syntax -> void
    (define (record-renamable-var rename-ht stx)
      (let ([key (list (syntax-source stx) (syntax-position stx) (syntax-span stx))])
        (hash-set! rename-ht
                   key
                   (cons stx (hash-ref rename-ht key '())))))
    
    ;; color-unused : hash-table[sexp -o> syntax] hash-table[sexp -o> #f] -> void
    (define (color-unused requires unused)
      (hash-for-each
       unused
       (λ (k v)
         (for-each (λ (stx) (color stx error-style-name 'default-mode))
                   (hash-ref requires k)))))
    
    ;; connect-identifier : syntax
    ;;                      id-set 
    ;;                      (union #f hash-table)
    ;;                      (union #f hash-table)
    ;;                      integer or 'lexical or #f
    ;;                      (listof id-set)
    ;;                      namespace
    ;;                      directory
    ;;                      boolean
    ;;                   -> void
    ;; adds arrows and rename menus for binders/bindings
    (define (connect-identifier var rename-ht all-binders
                                unused/phases requires/phases
                                phase-level user-namespace user-directory actual?)
      (connect-identifier/arrow var all-binders 
                                unused/phases requires/phases
                                phase-level user-namespace user-directory actual?)
      (when (and actual? (get-ids all-binders var))
        (record-renamable-var rename-ht var)))
    
    ;; id-level : integer-or-#f-or-'lexical identifier -> symbol
    (define (id-level phase-level id)
      (define (self-module? mpi)
        (let-values ([(a b) (module-path-index-split mpi)])
          (and (not a) (not b))))
      (let ([binding (identifier-binding id phase-level)])
        (cond [(list? binding)
               (if (self-module? (car binding))
                   'top-level
                   'imported)]
              [(eq? binding 'lexical) 'lexical]
              [else 'top-level])))
    
    ;; connect-identifier/arrow : syntax
    ;;                            id-set 
    ;;                            (union #f hash-table)
    ;;                            (union #f hash-table)
    ;;                            (union identifier-binding identifier-transformer-binding)
    ;;                            boolean
    ;;                         -> void
    ;; adds the arrows that correspond to binders/bindings
    (define (connect-identifier/arrow var all-binders unused/phases requires/phases phase-level user-namespace user-directory actual?)
      (let ([binders (get-ids all-binders var)])
        (when binders
          (for-each (λ (x)
                      (when (syntax-original? x)
                        (connect-syntaxes x var actual? (id-level phase-level x))))
                    binders))
        
        (when (and unused/phases requires/phases)
          (let ([req-path/pr (get-module-req-path (identifier-binding var phase-level)
                                                  phase-level)]
                [source-req-path/pr (get-module-req-path (identifier-binding var phase-level)
                                                          phase-level
                                                          #:nominal? #f)])
            (when (and req-path/pr source-req-path/pr)
              (let* ([req-path (list-ref req-path/pr 0)]
                     [id (list-ref req-path/pr 1)]
                     [source-req-path (list-ref source-req-path/pr 3)]
                     [source-id (list-ref source-req-path/pr 1)]
                     [req-phase-level (list-ref req-path/pr 2)]
                     [unused (hash-ref unused/phases req-phase-level)]
                     [requires (hash-ref requires/phases req-phase-level)]
                     [req-stxes (hash-ref requires req-path (λ () #f))])
                (when req-stxes
                  (hash-remove! unused req-path)
                  (for-each (λ (req-stx) 
                              (when (id/require-match? (syntax->datum var) 
                                                       id 
                                                       (syntax->datum req-stx))
                                (when id
                                  (let ([filename (get-require-filename source-req-path user-namespace user-directory)])
                                    (when filename
                                      (add-jump-to-definition
                                       var
                                       source-id
                                       filename))))
                                (add-mouse-over var
                                                (fw:gui-utils:format-literal-label 
                                                 (string-constant cs-mouse-over-import)
                                                 (syntax-e var)
                                                 req-path))
                                (connect-syntaxes req-stx var actual?
                                                  (id-level phase-level var))))
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
    
    
    ;; get-module-req-path : binding number [#:nominal? boolean] -> (union #f (list require-sexp sym ?? module-path))
    ;; argument is the result of identifier-binding or identifier-transformer-binding
    (define (get-module-req-path binding phase-level #:nominal? [nominal-source-path? #t])
      (and (pair? binding)
           (or (not (number? phase-level))
               (= phase-level
                  (+ (list-ref binding 5)
                     (list-ref binding 6))))
           (let ([mod-path (if nominal-source-path? (list-ref binding 2) (list-ref binding 0))])
             (cond
               [(module-path-index? mod-path)
                (let-values ([(base offset) (module-path-index-split mod-path)])
                  (list base
                        (if nominal-source-path? (list-ref binding 3) (list-ref binding 1))
                        (list-ref binding 5)
                        mod-path))]
               [(symbol? mod-path)
                (list mod-path 
                      (if nominal-source-path? (list-ref binding 3) (list-ref binding 1))
                      (list-ref binding 5)
                      mod-path)]
               [else #f]))))
    
    ;; color/connect-top : namespace directory id-set syntax -> void
    (define (color/connect-top rename-ht user-namespace user-directory binders var)
      (let ([top-bound?
             (or (get-ids binders var)
                 (parameterize ([current-namespace user-namespace])
                   (let/ec k
                     (namespace-variable-value (syntax-e var) #t (λ () (k #f)))
                     #t)))])
        (if top-bound?
            (color var lexically-bound-variable-style-name 'default-mode)
            (color var error-style-name 'default-mode))
        (connect-identifier var rename-ht binders #f #f 0 user-namespace user-directory #t)))
    
    ;; color-variable : syntax phase-level module-identifier-mapping -> void
    (define (color-variable var phase-level varsets)
      (let* ([b (identifier-binding var phase-level)]
             [lexical? 
              (or (not b)
                  (eq? b 'lexical)
                  (and (pair? b)
                       (let ([path (caddr b)])
                         (and (module-path-index? path)
                              (let-values ([(a b) (module-path-index-split path)])
                                (and (not a)
                                     (not b)))))))])
        (cond
          [(get-ids varsets var)
           (color var set!d-variable-style-name 'default-mode)]
          [lexical? (color var lexically-bound-variable-style-name 'default-mode)]
          [(pair? b) (color var imported-variable-style-name 'default-mode)])))
    
    ;; add-var : hash-table -> syntax -> void
    ;; adds the variable to the hash table.
    (define (add-var ht)
      (λ (var)
        (let* ([key (syntax-e var)]
               [prev (hash-ref ht key (λ () null))])
          (hash-set! ht key (cons var prev)))))
    
    ;; connect-syntaxes : syntax[original] syntax[original] boolean symbol -> void
    ;; adds an arrow from `from' to `to', unless they have the same source loc. 
    (define (connect-syntaxes from to actual? level)
      (let ([from-source (find-source-editor from)] 
            [to-source (find-source-editor to)]
            [defs-text (get-defs-text)])
        (when (and from-source to-source defs-text)
          (let ([pos-from (syntax-position from)]
                [span-from (syntax-span from)]
                [pos-to (syntax-position to)]
                [span-to (syntax-span to)])
            (when (and pos-from span-from pos-to span-to)
              (let* ([from-pos-left (- (syntax-position from) 1)]
                     [from-pos-right (+ from-pos-left (syntax-span from))]
                     [to-pos-left (- (syntax-position to) 1)]
                     [to-pos-right (+ to-pos-left (syntax-span to))])
                (unless (= from-pos-left to-pos-left)
                  (send defs-text syncheck:add-arrow
                        from-source from-pos-left from-pos-right
                        to-source to-pos-left to-pos-right
                        actual? level))))))))
    
    ;; add-mouse-over : syntax[original] string -> void
    ;; registers the range in the editor so that a mouse over
    ;; this area shows up in the status line.
    (define (add-mouse-over stx str)
      (let* ([source (find-source-editor stx)]
             [defs-text (get-defs-text)])
        (when (and defs-text 
                   source
                   (syntax-position stx)
                   (syntax-span stx))
          (let* ([pos-left (- (syntax-position stx) 1)]
                 [pos-right (+ pos-left (syntax-span stx))])
            (send defs-text syncheck:add-mouse-over-status
                  source pos-left pos-right str)))))
    
    ;; add-jump-to-definition : syntax symbol path -> void
    ;; registers the range in the editor so that the
    ;; popup menu in this area allows the programmer to jump
    ;; to the definition of the id.
    (define (add-jump-to-definition stx id filename)
      (let ([source (find-source-editor stx)]
            [defs-text (get-defs-text)])
        (when (and source 
                   defs-text
                   (syntax-position stx)
                   (syntax-span stx))
          (let* ([pos-left (- (syntax-position stx) 1)]
                 [pos-right (+ pos-left (syntax-span stx))])
            (send defs-text syncheck:add-jump-to-definition
                  source
                  pos-left
                  pos-right
                  id
                  filename)))))
    
    ;; find-syncheck-text : text% -> (union #f (is-a?/c syncheck-text<%>))
    (define (find-syncheck-text text)
      (let loop ([text text])
        (cond
          [(is-a? text syncheck-text<%>) text]
          [else 
           (let ([admin (send text get-admin)])
             (and (is-a? admin editor-snip-editor-admin<%>)
                  (let* ([enclosing-editor-snip (send admin get-snip)]
                         [editor-snip-admin (send enclosing-editor-snip get-admin)]
                         [enclosing-editor (send editor-snip-admin get-editor)])
                    (loop enclosing-editor))))])))
    
    ;; annotate-tail-position/last : (listof syntax) -> void
    (define (annotate-tail-position/last orig-stx stxs tail-ht)
      (unless (null? stxs)
        (annotate-tail-position orig-stx (car (last-pair stxs)) tail-ht)))
    
    ;; annotate-tail-position : syntax -> void
    ;; colors the parens (if any) around the argument
    ;; to indicate this is a tail call.
    (define (annotate-tail-position orig-stx tail-stx tail-ht)
      (hash-set!
       tail-ht 
       orig-stx 
       (cons
        tail-stx
        (hash-ref 
         tail-ht
         orig-stx
         (λ () null)))))
    
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
    
    ;; get-require-filename : sexp-or-module-path-index namespace string[directory] -> filename or #f
    ;; finds the filename corresponding to the require in stx
    (define (get-require-filename datum user-namespace user-directory)
      (parameterize ([current-namespace user-namespace]
                     [current-directory user-directory]
                     [current-load-relative-directory user-directory])
        (let* ([rkt-path/mod-path
                (with-handlers ([exn:fail? (λ (x) #f)])
                  (cond
                    [(module-path-index? datum)
                     (resolved-module-path-name 
                      (module-path-index-resolve datum))]
                    [else
                     (resolved-module-path-name 
                      ((current-module-name-resolver) datum #f #f))]))]
               [rkt-path/f (and (path? rkt-path/mod-path) rkt-path/mod-path)])
          (let/ec k
            (unless (path? rkt-path/f) (k rkt-path/f))
            (when (file-exists? rkt-path/f) (k rkt-path/f))
            (let* ([bts (path->bytes rkt-path/f)]
                   [len (bytes-length bts)])
              (unless (and (len . >= . 4) 
                           (bytes=? #".rkt" (subbytes bts (- len 4))))
                (k rkt-path/f))
              (let ([ss-path (bytes->path (bytes-append (subbytes bts 0 (- len 4)) #".ss"))])
                (unless (file-exists? ss-path)
                  (k rkt-path/f))
                ss-path))))))
    
    ;; make-require-open-menu : path -> menu -> void
    (define (make-require-open-menu file)
      (λ (menu)
        (let-values ([(base name dir?) (split-path file)])
          (instantiate menu-item% ()
            (label (fw:gui-utils:format-literal-label (string-constant cs-open-file) (path->string name)))
            (parent menu)
            (callback (λ (x y) (fw:handler:edit-file file))))
          (void))))
    
    ;; possible-suffixes : (listof string)
    ;; these are the suffixes that are checked for the reverse 
    ;; module-path mapping.
    (define possible-suffixes '(".rkt" ".ss" ".scm" ""))
    
    ;; module-name-sym->filename : symbol -> (union #f string)
    (define (module-name-sym->filename sym)
      (let ([str (symbol->string sym)])
        (and ((string-length str) . > . 1)
             (char=? (string-ref str 0) #\,)
             (let ([fn (substring str 1 (string-length str))])
               (ormap (λ (x)
                        (let ([test (string->path (string-append fn x))])
                          (and (file-exists? test)
                               test)))
                      possible-suffixes)))))
    
    ;; add-origins : sexp id-set -> void
    (define (add-origins sexp id-set)
      (let ([origin (syntax-property sexp 'origin)])
        (when origin
          (let loop ([ct origin])
            (cond
              [(pair? ct) 
               (loop (car ct))
               (loop (cdr ct))]
              [(syntax? ct) 
               (when (syntax-original? ct)
                 (add-id id-set ct))]
              [else (void)])))))
    
    ;; FIXME: handle for-template and for-label
    ;; extract-provided-vars : syntax -> (listof syntax[identifier])
    (define (extract-provided-vars stx)
      (syntax-case* stx (rename struct all-from all-from-except all-defined-except) symbolic-compare?
        [identifier
         (identifier? (syntax identifier))
         (list (syntax identifier))]
        
        [(rename local-identifier export-identifier) 
         (list (syntax local-identifier))]
        
        ;; why do I even see this?!?
        [(struct struct-identifier (field-identifier ...))
         null]
        
        [(all-from module-name) null] 
        [(all-from-except module-name identifier ...)
         null]
        [(all-defined-except identifier ...)
         (syntax->list #'(identifier ...))]
        [_ 
         null]))
    
    
    ;; trim-require-prefix : syntax -> syntax
    (define (trim-require-prefix require-spec)
      (syntax-case* require-spec (only prefix all-except prefix-all-except rename just-meta) symbolic-compare?
        [(only module-name identifer ...)
         (syntax module-name)]
        [(prefix identifier module-name) 
         (syntax module-name)]
        [(all-except module-name identifer ...)
         (syntax module-name)]
        [(prefix-all-except module-name identifer ...)
         (syntax module-name)]
        [(rename module-name local-identifer exported-identifer)
         (syntax module-name)]
        [_ require-spec]))
    
    (define (symbolic-compare? x y) (eq? (syntax-e x) (syntax-e y)))
    
    ;; add-binders : syntax id-set -> void
    ;; transforms an argument list into a bunch of symbols/symbols
    ;; and puts them into the id-set
    ;; effect: colors the identifiers
    (define (add-binders stx id-set)
      (let loop ([stx stx])
        (let ([e (if (syntax? stx) (syntax-e stx) stx)])
          (cond
            [(cons? e)
             (let ([fst (car e)]
                   [rst (cdr e)])
               (if (syntax? fst)
                   (begin
                     (when (syntax-original? fst)
                       (add-id id-set fst))
                     (loop rst))
                   (loop rst)))]
            [(null? e) (void)]
            [else 
             (when (syntax-original? stx)
               (add-id id-set stx))]))))
    
    ;; annotate-raw-keyword : syntax id-map -> void
    ;; annotates keywords when they were never expanded. eg.
    ;; if someone just types `(λ (x) x)' it has no 'origin
    ;; field, but there still are keywords.
    (define (annotate-raw-keyword stx id-map)
      (let ([lst (syntax-e stx)])
        (when (pair? lst)
          (let ([f-stx (car lst)])
            (when (and (syntax-original? f-stx)
                       (identifier? f-stx))
              (add-id id-map f-stx))))))
    
    ;; color-internal-structure : syntax str -> void
    (define (color-internal-structure stx style-name mode)
      (let ([ht (make-hasheq)]) 
        ;; ht : stx -o> true
        ;; indicates if we've seen this syntax object before
        
        (let loop ([stx stx]
                   [datum (syntax->datum stx)])
          (unless (hash-ref ht datum (λ () #f))
            (hash-set! ht datum #t)
            (cond
              [(pair? stx) 
               (loop (car stx) (car datum))
               (loop (cdr stx) (cdr datum))]
              [(syntax? stx)
               (when (syntax-original? stx)
                 (color stx style-name mode))
               (let ([stx-e (syntax-e stx)]) 
                 (cond
                   [(cons? stx-e)
                    (loop (car stx-e) (car datum))
                    (loop (cdr stx-e) (cdr datum))]
                   [(null? stx-e)
                    (void)]
                   [(vector? stx-e)
                    (for-each loop
                              (vector->list stx-e)
                              (vector->list datum))]
                   [(box? stx-e)
                    (loop (unbox stx-e) (unbox datum))]
                   [else (void)]))])))))
    
    ;; jump-to : syntax -> void
    (define (jump-to stx)
      (let ([src (find-source-editor stx)]
            [pos (syntax-position stx)]
            [span (syntax-span stx)])
        (when (and (is-a? src text%)
                   pos
                   span)
          (send src set-position (- pos 1) (+ pos span -1)))))

    ;; hash-table[syntax -o> (listof syntax)] -> void
    (define (add-tail-ht-links tail-ht)
      (begin
        (collapse-tail-links tail-ht)
        (hash-for-each
         tail-ht
         (λ (stx-from stx-tos)
           (for-each (λ (stx-to) (add-tail-ht-link stx-from stx-to))
                     stx-tos)))))
    
    ;; hash-table[syntax -o> (listof syntax)] -> void
    ;; take something like a transitive closure, except
    ;; only when there are non-original links in between
    
    (define (collapse-tail-links tail-ht)
      (let loop ()
        (let ([found-one? #f])
          (hash-for-each
           tail-ht
           (λ (stx-from stx-tos)
             (for-each
              (λ (stx-to)
                (let ([stx-to-tos (hash-ref tail-ht stx-to '())])
                  (for-each
                   (λ (stx-to-to)
                     (unless (and (add-tail-link? stx-from stx-to) 
                                  (add-tail-link? stx-to stx-to-to))
                       (unless (memq stx-to-to (hash-ref tail-ht stx-from '()))
                         (set! found-one? #t)
                         (hash-cons! tail-ht stx-from stx-to-to))))
                   stx-to-tos)))
              stx-tos)))
          
          ;; this takes O(n^3) in general, so we just do
          ;; one iteration. This doesn't work for case 
          ;; expressions but it seems to for most others.
          ;; turning this on makes this function go from about
          ;; 55 msec to about 2400 msec on my laptop, 
          ;; (a 43x slowdown) when checking the syntax of this file.
        
          #;
          (when found-one?
            (loop)))))
    
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
    
    ;; add-tail-link? : syntax syntax -> boolean
    (define (add-tail-link? from-stx to-stx)
      (let* ([to-src (find-source-editor to-stx)]
             [from-src (find-source-editor from-stx)]
             [defs-text (get-defs-text)])
        (and to-src from-src defs-text
             (let ([from-pos (syntax-position from-stx)]
                   [to-pos (syntax-position to-stx)])
               (and from-pos to-pos)))))
    

 
    
;                                                                                             
;                                                                                             
;       ;                                                                                     
;       ;                                                                   ;                 
;       ;                                             ;             ;                         
;    ;; ;   ;;;    ;;;;  ;   ; ; ;; ;;  ;;;   ; ;;   ;;;;;  ;;;;   ;;;;;  ;;;     ;;;   ; ;;  
;   ;  ;;  ;   ;  ;      ;   ; ;; ;; ; ;   ;  ;;  ;   ;         ;   ;       ;    ;   ;  ;;  ; 
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;   ;  ;   ;   ;         ;   ;       ;    ;   ;  ;   ; 
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;;;;;  ;   ;   ;      ;;;;   ;       ;    ;   ;  ;   ; 
;   ;   ;  ;   ;  ;      ;   ; ;  ;  ; ;      ;   ;   ;     ;   ;   ;       ;    ;   ;  ;   ; 
;   ;  ;;  ;   ;  ;      ;  ;; ;  ;  ; ;      ;   ;   ;     ;  ;;   ;       ;    ;   ;  ;   ; 
;    ;; ;   ;;;    ;;;;   ;; ; ;  ;  ;  ;;;;  ;   ;    ;;;   ;;  ;   ;;;    ;     ;;;   ;   ; 
;                                                                                             
;                                                                                             
;                                                                                             

    
    ;; document-variable : stx[identifier,original] phase-level -> void
    (define (document-variable stx phase-level)
      (let ([defs-text (currently-processing-definitions-text)])
        (when defs-text
          (let ([binding-info (identifier-binding stx phase-level)])
            (when (and (pair? binding-info)
                       (syntax-position stx)
                       (syntax-span stx))
              (let* ([start (- (syntax-position stx) 1)]
                     [fin (+ start (syntax-span stx))]
                     [source-editor (find-source-editor stx)]
                     [xref (get-xref)])
                (when (and xref source-editor)
                  (let ([definition-tag (xref-binding->definition-tag xref binding-info #f)])
                    (when definition-tag
                      (let-values ([(path tag) (xref-tag->path+anchor xref definition-tag)])
                        (when path
                          (let ([index-entry (xref-tag->index-entry xref definition-tag)])
                            (when index-entry
                              (send defs-text syncheck:add-background-color
                                    source-editor "navajowhite" start fin (syntax-e stx))
                              (send defs-text syncheck:add-menu
                                    source-editor
                                    start 
                                    fin 
                                    (syntax-e stx)
                                    (λ (menu)
                                      (instantiate menu-item% ()
                                        (parent menu)
                                        (label (build-docs-label (entry-desc index-entry)))
                                        (callback
                                         (λ (x y)
                                           (let* ([url (path->url path)]
                                                  [url2 (if tag
                                                            (make-url (url-scheme url)
                                                                      (url-user url)
                                                                      (url-host url)
                                                                      (url-port url)
                                                                      (url-path-absolute? url)
                                                                      (url-path url)
                                                                      (url-query url)
                                                                      tag)
                                                            url)])
                                             (send-url (url->string url2)))))))))))))))))))))
    
    (define (build-docs-label desc)
      (let ([libs (exported-index-desc-from-libs desc)])
        (cond
          [(null? libs)
           (fw:gui-utils:format-literal-label
            (string-constant cs-view-docs)
            (exported-index-desc-name desc))]
          [else
           (fw:gui-utils:format-literal-label
            (string-constant cs-view-docs-from)
            (format 
             (string-constant cs-view-docs)
             (exported-index-desc-name desc))
            (apply string-append 
                   (add-between 
                    (map (λ (x) (format "~s" x)) libs) 
                    ", ")))])))
    
    
    
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
      (let ([defs-text (currently-processing-definitions-text)])
        (when defs-text
          (let* ([source (syntax-source (car stxs))] ;; all stxs in the list must have the same source
                 [source-editor (find-source-editor (car stxs))])
            (when (is-a? source-editor text%)
              (let* ([start (- (syntax-position (car stxs)) 1)]
                     [fin (+ start (syntax-span (car stxs)))])
                (send defs-text syncheck:add-menu
                      source-editor
                      start 
                      fin 
                      (syntax-e (car stxs))
                      (λ (menu)
                        (let ([name-to-offer (format "~a" (syntax->datum (car stxs)))])
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
                                                  frame-parent))))))))))))))
    
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
          (let* ([new-sym (format "~s" (string->symbol new-str))]
                 [to-be-renamed 
                  (remove-duplicates-stx
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
                (let ([txts (list defs-text)])
                  (send defs-text begin-edit-sequence)
                  (for-each (λ (stx) 
                              (let ([source-editor (find-source-editor/defs stx defs-text)])
                                (when (is-a? source-editor text%)
                                  (unless (memq source-editor txts)
                                    (send source-editor begin-edit-sequence)
                                    (set! txts (cons source-editor txts)))
                                  (let* ([start (- (syntax-position stx) 1)]
                                         [end (+ start (syntax-span stx))])
                                    (send source-editor delete start end #f)
                                    (send source-editor insert new-sym start start #f)))))
                            to-be-renamed)
                  (send defs-text invalidate-bitmap-cache)
                  (for-each
                   (λ (txt) (send txt end-edit-sequence))
                   txts))))))))
    
    ;; name-duplication? : (listof syntax) (listof id-set) symbol -> boolean
    ;; returns #t if the name chosen would be the same as another name in this scope.
    (define (name-duplication? to-be-renamed id-sets new-str)
      (let ([new-ids (map (λ (id) (datum->syntax id (string->symbol new-str)))
                          to-be-renamed)])
        (ormap (λ (id-set)
                 (ormap (λ (new-id) (get-ids id-set new-id)) 
                        new-ids))
               id-sets)))
    
    ;; remove-duplicates-stx : (listof syntax[original]) -> (listof syntax[original])
    ;; removes duplicates, based on the source locations of the identifiers
    (define (remove-duplicates-stx ids)
      (cond
        [(null? ids) null]
        [else (let loop ([fst (car ids)]
                         [rst (cdr ids)])
                (cond
                  [(null? rst) (list fst)]
                  [else (if (and (eq? (syntax-source fst)
                                      (syntax-source (car rst)))
                                 (= (syntax-position fst)
                                    (syntax-position (car rst))))
                            (loop fst (cdr rst))
                            (cons fst (loop (car rst) (cdr rst))))]))]))
    
    
    ;                                            
    ;                                            
    ;                                            
    ;   ;       ;                                
    ;           ;                                
    ;           ;                     ;          
    ;   ;    ;; ;        ;;;    ;;;  ;;;;   ;;;  
    ;   ;   ;  ;;       ;      ;   ;  ;    ;     
    ;   ;  ;    ;       ;;    ;    ;  ;    ;;    
    ;   ;  ;    ;        ;;   ;;;;;;  ;     ;;   
    ;   ;  ;    ;          ;  ;       ;       ;  
    ;   ;   ;  ;;          ;   ;      ;       ;  
    ;   ;    ;; ;       ;;;     ;;;;   ;;  ;;;   
    ;                                            
    ;                                            
    ;                                            
    
    ;; make-id-set : -> id-set
    (define (make-id-set) (make-module-identifier-mapping))
    
    ;; add-id : id-set identifier -> void
    (define (add-id mapping id)
      (let* ([old (module-identifier-mapping-get mapping id (λ () '()))]
             [new (cons id old)])
        (module-identifier-mapping-put! mapping id new)))
    
    ;; get-idss : id-set -> (listof (listof identifier))
    (define (get-idss mapping)
      (module-identifier-mapping-map mapping (λ (x y) y)))
    
    ;; get-ids : id-set identifier -> (union (listof identifier) #f)
    (define (get-ids mapping var)
      (module-identifier-mapping-get mapping var (λ () #f)))
    
    ;; for-each-ids : id-set ((listof identifier) -> void) -> void
    (define (for-each-ids mapping f)
      (module-identifier-mapping-for-each mapping (λ (x y) (f y))))
    
