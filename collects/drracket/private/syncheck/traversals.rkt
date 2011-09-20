#lang racket/base

(require "colors.rkt"
         "intf.rkt"
         "local-member-names.rkt"
         "annotate.rkt"
         "contract-traversal.rkt"
         string-constants
         racket/unit
         racket/set
         racket/class
         racket/list
         syntax/boundmap
         scribble/xref
         scribble/manual-struct)

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
    
    
    
    ;; make-traversal : namespace string[directory] -> (values (syntax (union #f syntax) -> void)
    ;;                                                         (-> void))
    ;; returns a pair of functions that close over some state that
    ;; represents the top-level of a single program. The first value
    ;; is called once for each top-level expression and the second
    ;; value is called once, after all expansion is complete.
    (define (make-traversal user-namespace user-directory)
      (let* ([tl-phase-to-binders (make-hash)]
             [tl-phase-to-varrefs (make-hash)]
             [tl-phase-to-varsets (make-hash)]
             [tl-phase-to-tops (make-hash)]
             [tl-binding-inits (make-id-set)]
             [tl-templrefs (make-id-set)]
             [tl-phase-to-requires (make-hash)]
             [tl-module-lang-requires (make-hash)]
             [expanded-expression
              (λ (sexp [visit-id void])
                (parameterize ([current-load-relative-directory user-directory])
                  (let ([is-module? (syntax-case sexp (module)
                                      [(module . rest) #t]
                                      [else #f])])
                    (cond
                      [is-module?
                       (let ([phase-to-binders (make-hash)]
                             [phase-to-varrefs (make-hash)]
                             [phase-to-varsets (make-hash)]
                             [phase-to-tops (make-hash)]
                             [phase-to-requires (make-hash)]
                             [binding-inits (make-id-set)]
                             [templrefs (make-id-set)]
                             [module-lang-requires (make-hash)]
                             [requires (make-hash)]
                             [require-for-syntaxes (make-hash)]
                             [require-for-templates (make-hash)]
                             [require-for-labels (make-hash)])
                          (annotate-basic sexp
                                          user-namespace user-directory visit-id
                                          phase-to-binders
                                          phase-to-varrefs
                                          phase-to-varsets
                                          phase-to-tops
                                          binding-inits
                                          templrefs
                                          module-lang-requires
                                          phase-to-requires) 
                         (annotate-variables user-namespace
                                             user-directory
                                             phase-to-binders
                                             phase-to-varrefs
                                             phase-to-varsets
                                             phase-to-tops
                                             templrefs
                                             module-lang-requires
                                             phase-to-requires)
                         (annotate-contracts sexp 
                                             (hash-ref phase-to-binders 0 (λ () (make-id-set)))
                                             binding-inits))]
                      [else
                       (annotate-basic sexp
                                       user-namespace user-directory visit-id
                                       tl-phase-to-binders
                                       tl-phase-to-varrefs
                                       tl-phase-to-varsets
                                       tl-phase-to-tops
                                       tl-binding-inits
                                       tl-templrefs
                                       tl-module-lang-requires
                                       tl-phase-to-requires)]))))]
             [expansion-completed
              (λ ()
                (parameterize ([current-load-relative-directory user-directory])
                  (annotate-variables user-namespace
                                      user-directory
                                      tl-phase-to-binders
                                      tl-phase-to-varrefs
                                      tl-phase-to-varsets
                                      tl-phase-to-tops
                                      tl-templrefs
                                      tl-module-lang-requires
                                      tl-phase-to-requires)))])
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
                            user-namespace user-directory visit-id
                            phase-to-binders 
                            phase-to-varrefs 
                            phase-to-varsets
                            phase-to-tops
                            binding-inits
                            templrefs
                            module-lang-requires
                            phase-to-requires)
      
      (let ([tail-ht (make-hasheq)]
            [maybe-jump (λ (vars) (visit-id vars))])

        (let level-loop ([sexp sexp]
                         [level 0])
          (let* ([loop (λ (sexp) (level-loop sexp level))]
                 [varrefs (lookup-phase-to-mapping phase-to-varrefs level)]
                 [varsets (lookup-phase-to-mapping phase-to-varsets level)]
                 [binders (lookup-phase-to-mapping phase-to-binders level)]
                 [tops (lookup-phase-to-mapping phase-to-tops level)]
                 [requires (hash-ref! phase-to-requires level (λ () (make-hash)))]
                 [collect-general-info
                  (λ (stx)
                    (add-origins stx varrefs)
                    (add-disappeared-bindings stx binders varrefs)
                    (add-disappeared-uses stx varrefs))])
            (collect-general-info sexp)
            (syntax-case* sexp (#%plain-lambda case-lambda if begin begin0 let-values letrec-values set!
                                               quote quote-syntax with-continuation-mark 
                                               #%plain-app #%top #%plain-module-begin
                                               define-values define-syntaxes begin-for-syntax module
                                               #%require #%provide #%expression)
              (λ (x y) (free-identifier=? x y level 0))
              [(#%plain-lambda args bodies ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (annotate-tail-position/last sexp (syntax->list (syntax (bodies ...))) tail-ht)
                 (add-binders (syntax args) binders #f #f)
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
                    (add-binders args binders #f #f)
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
                   (for-each (λ (x es) (add-binders x binders binding-inits es))
                             (syntax->list (syntax ((xss ...) ...)))
                             (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (bs ...))))))]
              [(letrec-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (annotate-tail-position/last sexp (syntax->list (syntax (bs ...))) tail-ht)
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x es) (add-binders x binders binding-inits es))
                             (syntax->list (syntax ((xss ...) ...)))
                             (syntax->list (syntax (es ...))))
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
                 (add-binders (syntax vars) binders binding-inits #'b)
                 (maybe-jump (syntax vars))
                 (loop (syntax b)))]
              [(define-syntaxes names exp)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (add-binders (syntax names) binders binding-inits #'exp)
                 (maybe-jump (syntax names))
                 (level-loop (syntax exp) (+ level 1)))]
              [(begin-for-syntax exp ...)
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (for-each (lambda (e) (level-loop e (+ level 1))) (syntax->list (syntax (exp ...)))))]
              [(module m-name lang (#%plain-module-begin bodies ...))
               (begin
                 (annotate-raw-keyword sexp varrefs)
                 (hash-set! module-lang-requires (syntax lang) #t)
                 ((annotate-require-open user-namespace user-directory) (syntax lang))
                 
                 (hash-cons! requires (syntax->datum (syntax lang)) (syntax lang))
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              ; top level or module top level only:
              [(#%require require-specs ...)
               (let ([at-phase
                      (lambda (stx level)
                        (define requires (hash-ref! phase-to-requires level (λ () (make-hash))))
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
                 (for ([spec (in-list (syntax->list #'(require-specs ...)))])
                   (let loop ([spec spec]
                              [level level])
                     (define (add-to-level n) (and n level (+ n level)))
                     (syntax-case* spec (for-syntax for-template for-label for-meta just-meta) 
                       (lambda (a b)
                         (eq? (syntax-e a) (syntax-e b)))
                       [(just-meta phase specs ...)
                        (for ([spec (in-list (syntax->list #'(specs ...)))])
                          (loop spec (add-to-level (syntax-e #'phase))))]
                       [(for-meta phase specs ...)
                        (for ([spec (in-list (syntax->list #'(specs ...)))])
                          (loop spec (add-to-level (syntax-e #'phase))))]
                       [(for-syntax specs ...)
                        (at-phase spec (add-to-level 1))]
                       [(for-template specs ...)
                        (at-phase spec (add-to-level -1))]
                       [(for-label specs ...)
                        (at-phase spec #f)]
                       [else
                        (at-phase (list #f spec) level)]))))]
              
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
                 (printf "unknown stx: ~.s datum: ~e source: ~e\n"
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
                                phase-to-binders
                                phase-to-varrefs
                                phase-to-varsets
                                phase-to-tops
                                templrefs
                                module-lang-requires
                                phase-to-requires)
      
      (let ([unused-requires (make-hash)]
            [unused-require-for-syntaxes (make-hash)]
            [unused-require-for-templates (make-hash)]
            [unused-require-for-labels (make-hash)]
            [unused/phases (make-hash)])
        
        (for ([(level hash) (in-hash phase-to-requires)])
          (define new-hash (make-hash))
          (hash-set! unused/phases level new-hash)
          (for ([(k v) (in-hash hash)])
            (hash-set! new-hash k #t)))
                
        (for ([(level binders) (in-hash phase-to-binders)])
          (for ([vars (in-list (get-idss binders))])
            (for ([var (in-list vars)])
              (when (syntax-original? var)
                (define varset (lookup-phase-to-mapping phase-to-varsets level))
                (color-variable var 0 varset)
                (document-variable var 0)))))
        
        (for ([(level varrefs) (in-hash phase-to-varrefs)])
          (define binders (lookup-phase-to-mapping phase-to-binders level))
          (define varsets (lookup-phase-to-mapping phase-to-varsets level)) 
          (for ([vars (in-list (get-idss varrefs))])
            (for ([var (in-list vars)])
              (color-variable var level varsets)
              (when (syntax-original? var)
                (document-variable var level))
              (connect-identifier var
                                  binders
                                  unused/phases
                                  phase-to-requires
                                  level
                                  user-namespace 
                                  user-directory
                                  #t))))
        
        (for ([vars (in-list (get-idss templrefs))])
          (for ([var (in-list vars)])
            
            ;; build a set of all of the known phases
            (define phases (set))
            (for ([phase (in-list (hash-keys phase-to-binders))])
              (set! phases (set-add phases phase)))
            (for ([phase (in-list (hash-keys phase-to-requires))])
              (set! phases (set-add phases phase)))
            
            ;; connect every identifier inside a quote-syntax to each binder at any phase
            (for ([phase (in-set phases)])
              (connect-identifier var
                                  (lookup-phase-to-mapping phase-to-binders phase)
                                  unused/phases
                                  phase-to-requires
                                  phase
                                  user-namespace
                                  user-directory
                                  #f))))
        
        (for ([(level tops) (in-hash phase-to-tops)])
          (define binders (lookup-phase-to-mapping phase-to-binders level))
          (for ([vars (in-list (get-idss tops))])
            (for ([var (in-list vars)])
              (color/connect-top user-namespace user-directory binders var))))
        
        (for ([(level require-hash) (in-hash phase-to-requires)])
          (define unused-hash (hash-ref unused/phases level))
          (color-unused require-hash unused-hash module-lang-requires))

        (make-rename-menus (list phase-to-binders phase-to-varrefs phase-to-tops))))
    
    ;; color-unused : hash-table[sexp -o> syntax] hash-table[sexp -o> #f] hash-table[syntax -o> #t] -> void
    (define (color-unused requires unused module-lang-requires)
      (hash-for-each
       unused
       (λ (k v)
         (for-each (λ (stx) 
                     (unless (hash-ref module-lang-requires stx #f)
                       (define defs-text (current-annotations))
                       (define source-editor (find-source-editor stx))
                       (when (and defs-text source-editor)
                         (define pos (syntax-position stx))
                         (define span (syntax-span stx))
                         (when (and pos span)
                           (define start (- pos 1))
                           (define fin (+ start span))
                           (send defs-text syncheck:add-background-color
                                 source-editor start fin "firebrick")))
                       (color stx unused-require-style-name 'default-mode)))
                   (hash-ref requires k 
                             (λ ()
                               (error 'syncheck/traversals.rkt "requires doesn't have a mapping for ~s" k)))))))
    
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
    
    ;; connect-identifier : syntax
    ;;                      id-set 
    ;;                      (union #f hash-table)
    ;;                      (union #f hash-table)
    ;;                      (union identifier-binding identifier-transformer-binding)
    ;;                      boolean
    ;;                   -> void
    ;; adds the arrows that correspond to binders/bindings
    (define (connect-identifier var all-binders unused/phases phase-to-requires
                                phase-level user-namespace user-directory actual?)
      (let ([binders (get-ids all-binders var)])
        (when binders
          (for-each (λ (x)
                      (when (syntax-original? x)
                        (connect-syntaxes x var actual? (id-level phase-level x))))
                    binders))
        
        (when (and unused/phases phase-to-requires)
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
                     [unused (hash-ref! unused/phases req-phase-level (λ () (make-hash)))]
                     [requires (hash-ref! phase-to-requires req-phase-level (λ () (make-hash)))]
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
                                                (format 
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
    (define (color/connect-top user-namespace user-directory binders var)
      (let ([top-bound?
             (or (get-ids binders var)
                 (parameterize ([current-namespace user-namespace])
                   (let/ec k
                     (namespace-variable-value (syntax-e var) #t (λ () (k #f)))
                     #t)))])
        (if top-bound?
            (color var lexically-bound-variable-style-name 'default-mode)
            (color var free-variable-style-name 'default-mode))
        (connect-identifier var binders #f #f 0 user-namespace user-directory #t)))
    
    ;; color-variable : syntax phase-level identifier-mapping -> void
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
            [defs-text (current-annotations)])
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
             [defs-text (current-annotations)])
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
            [defs-text (current-annotations)])
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
            (when (and source
                       (syntax-position require-spec)
                       (syntax-span require-spec))
              (let ([defs-text (current-annotations)])
                (when defs-text
                  (let* ([start (- (syntax-position require-spec) 1)]
                         [end (+ start (syntax-span require-spec))]
                         [file (get-require-filename (syntax->datum require-spec)
                                                     user-namespace
                                                     user-directory)])
                    (when file
                      (send defs-text syncheck:add-require-open-menu
                            source start end file))))))))))
    
    ;; get-require-filename : sexp-or-module-path-index namespace string[directory] -> filename or #f
    ;; finds the filename corresponding to the require in stx
    (define (get-require-filename datum user-namespace user-directory)
      (parameterize ([current-namespace user-namespace]
                     [current-directory (or user-directory (current-directory))]
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
    
    ;; add-binders : syntax id-set (or/c #f id-set) (or/c #f syntax) -> void
    ;; transforms an argument list into a bunch of symbols/symbols
    ;; and puts them into the id-set
    ;; effect: colors the identifiers
    (define (add-binders stx id-set binding-to-init init-exp)
      (let loop ([stx stx])
        (let ([e (if (syntax? stx) (syntax-e stx) stx)])
          (cond
            [(cons? e)
             (let ([fst (car e)]
                   [rst (cdr e)])
               (if (syntax? fst)
                   (begin
                     (when (syntax-original? fst)
                       (when binding-to-init
                         (add-init-exp binding-to-init fst init-exp))
                       (add-id id-set fst))
                     (loop rst))
                   (loop rst)))]
            [(null? e) (void)]
            [else 
             (when (syntax-original? stx)
               (when binding-to-init
                 (add-init-exp binding-to-init stx init-exp))
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
             [defs-text (current-annotations)])
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
             [defs-text (current-annotations)])
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
      (let ([defs-text (current-annotations)])
        (when defs-text
          (let ([binding-info (identifier-binding stx phase-level)])
            (when (and (pair? binding-info)
                       (syntax-position stx)
                       (syntax-span stx))
              (let* ([start (- (syntax-position stx) 1)]
                     [fin (+ start (syntax-span stx))]
                     [source-editor (find-source-editor stx)])
                (when source-editor
                  (let ([xref (get-xref)])
                    (when xref
                      (let ([definition-tag (xref-binding->definition-tag xref binding-info #f)])
                        (when definition-tag
                          (let-values ([(path tag) (xref-tag->path+anchor xref definition-tag)])
                            (when path
                              (let ([index-entry (xref-tag->index-entry xref definition-tag)])
                                (when index-entry
                                  (send defs-text syncheck:add-background-color
                                        source-editor start fin "navajowhite")
                                  (send defs-text syncheck:add-docs-menu
                                        source-editor
                                        start 
                                        fin 
                                        (syntax-e stx)
                                        (build-docs-label (entry-desc index-entry))
                                        path
                                        tag))))))))))))))))
    
    (define (build-docs-label desc)
      (let ([libs (exported-index-desc-from-libs desc)])
        (cond
          [(null? libs)
           (format
            (string-constant cs-view-docs)
            (exported-index-desc-name desc))]
          [else
           (format
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
    
    
    ;; make-rename-menus : (listof phase-to-mapping) -> void
    (define (make-rename-menus phase-tos)
      (define id-to-sets (make-free-identifier-mapping))
      (let ([defs-text (current-annotations)])
        (when defs-text
          (for ([phase-to-mapping (in-list phase-tos)])
            (for ([(level id-set) (in-hash phase-to-mapping)])
              (for-each-ids 
               id-set
               (λ (vars)
                 (for ([var (in-list vars)])
                   (define ed (find-source-editor var))
                   (when ed
                     (define pos (syntax-position var))
                     (define span (syntax-span var))
                     (when (and pos span)
                       (define start (- pos 1))
                       (define fin (+ start span))
                       (define loc (list ed start fin))
                       (free-identifier-mapping-put!
                        id-to-sets
                        var
                        (set-add (free-identifier-mapping-get id-to-sets var set)
                                 loc)))))))))
          (free-identifier-mapping-for-each
           id-to-sets
           (λ (id locs)
             (define (name-dup? new-str) 
               (and (for/or ([phase-to-map (in-list phase-tos)])
                      (for/or ([(level id-set) (in-hash phase-to-map)])
                        (for/or ([id (in-list (or (get-ids id-set id) '()))])
                          (let ([new-id (datum->syntax id (string->symbol new-str))])
                            (for/or ([phase-to-map (in-list phase-tos)])
                              (for/or ([(level id-set) (in-hash phase-to-map)])
                                (get-ids id-set new-id)))))))
                    #t))
             (define loc-lst (set->list locs))
             (define id-as-sym (syntax-e id))
             (send defs-text syncheck:add-rename-menu
                   id-as-sym
                   loc-lst
                   name-dup?))))))
    
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
    
    
    (define (lookup-phase-to-mapping phase-to n)
      (hash-ref! phase-to n (λ () (make-id-set))))
    
    ;; make-id-set : -> id-set
    (define (make-id-set) (make-free-identifier-mapping))
    
    ;; add-init-exp : id-set identifier stx -> void
    (define (add-init-exp mapping id init-exp)
      (let* ([old (free-identifier-mapping-get mapping id (λ () '()))]
             [new (cons init-exp old)])
        (free-identifier-mapping-put! mapping id new)))
    
    ;; add-id : id-set identifier -> void
    (define (add-id mapping id)
      (let* ([old (free-identifier-mapping-get mapping id (λ () '()))]
             [new (cons id old)])
        (free-identifier-mapping-put! mapping id new)))
    
    ;; get-idss : id-set -> (listof (listof identifier))
    (define (get-idss mapping)
      (free-identifier-mapping-map mapping (λ (x y) y)))
    
    ;; get-ids : id-set identifier -> (union (listof identifier) #f)
    (define (get-ids mapping var)
      (free-identifier-mapping-get mapping var (λ () #f)))
    
    ;; for-each-ids : id-set ((listof identifier) -> void) -> void
    (define (for-each-ids mapping f)
      (free-identifier-mapping-for-each mapping (λ (x y) (f y))))
