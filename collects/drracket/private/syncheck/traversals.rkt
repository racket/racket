#lang racket/base

(require "colors.rkt"
         "intf.rkt"
         "local-member-names.rkt"
         "annotate.rkt"
         "contract-traversal.rkt"
         "xref.rkt"
         string-constants
         racket/unit
         racket/match
         racket/set
         racket/class
         racket/list
         syntax/boundmap
         framework/preferences
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
                (parameterize ([current-directory (or user-directory (current-directory))]
                               [current-load-relative-directory user-directory])
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
                (parameterize ([current-directory (or user-directory (current-directory))]
                               [current-load-relative-directory user-directory])
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
    (define (annotate-basic stx-obj 
                            user-namespace user-directory visit-id
                            phase-to-binders 
                            phase-to-varrefs 
                            phase-to-varsets
                            phase-to-tops
                            binding-inits
                            templrefs
                            module-lang-requires
                            phase-to-requires)
      
      (let ([maybe-jump (λ (vars) (visit-id vars))])
        (let level+tail-loop ([stx-obj stx-obj]
                              [level 0]
                              [tail-parent-src #f]
                              [tail-parent-pos #f])
          (define-values (next-tail-parent-src next-tail-parent-pos) 
            (let ([child-src (find-source-editor stx-obj)]
                  [child-pos (syntax-position stx-obj)]
                  [defs-text (current-annotations)])
              (cond
                [(and child-src child-pos defs-text)
                 (when (and tail-parent-src tail-parent-pos)
                   (unless (and (eq? tail-parent-src child-src)
                                (equal? tail-parent-pos child-pos)) 
                     (send defs-text syncheck:add-tail-arrow
                           tail-parent-src (- tail-parent-pos 1)
                           child-src (- child-pos 1))))
                 (values child-src child-pos)]
                [else 
                 (values tail-parent-src tail-parent-pos)])))
          (let* ([level-loop (λ (sexp level) (level+tail-loop sexp level #f #f))]
                 [tail-loop (λ (sexp) (level+tail-loop sexp level next-tail-parent-src next-tail-parent-pos))]
                 [loop (λ (sexp) (level+tail-loop sexp level #f #f))]
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
            (collect-general-info stx-obj)
            
            (define (list-loop/tail-last bodies)
              (unless (null? bodies)
                (let body-loop ([fst (car bodies)]
                                [bodies (cdr bodies)])
                  (cond
                    [(null? bodies)
                     (tail-loop fst)]
                    [else
                     (loop fst)
                     (body-loop (car bodies) (cdr bodies))]))))
            
            (syntax-case* stx-obj (#%plain-lambda case-lambda if begin begin0 let-values letrec-values set!
                                                  quote quote-syntax with-continuation-mark 
                                                  #%plain-app #%top #%plain-module-begin
                                                  define-values define-syntaxes begin-for-syntax 
                                                  module module*
                                                  #%require #%provide #%expression)
              (λ (x y) (free-identifier=? x y level 0))
              [(#%plain-lambda args bodies ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (add-binders (syntax args) binders #f #f)
                 (list-loop/tail-last (syntax->list (syntax (bodies ...)))))]
              [(case-lambda [argss bodiess ...]...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (for-each
                  (λ (args bodies)
                    (add-binders args binders #f #f)
                    (list-loop/tail-last (syntax->list bodies)))
                  (syntax->list (syntax (argss ...)))
                  (syntax->list (syntax ((bodiess ...) ...)))))]
              [(if test then else)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (loop (syntax test))
                 (tail-loop (syntax then))
                 (tail-loop (syntax else)))]
              [(begin bodies ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (list-loop/tail-last (syntax->list (syntax (bodies ...)))))]
              
              ;; treat a single body expression specially, since this has
              ;; different tail behavior.
              [(begin0 body)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (tail-loop (syntax body)))]
              
              [(begin0 bodies ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              [(let-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x es) (add-binders x binders binding-inits es))
                             (syntax->list (syntax ((xss ...) ...)))
                             (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (list-loop/tail-last (syntax->list (syntax (bs ...))))))]
              [(letrec-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x es) (add-binders x binders binding-inits es))
                             (syntax->list (syntax ((xss ...) ...)))
                             (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (list-loop/tail-last (syntax->list (syntax (bs ...))))))]
              [(set! var e)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 
                 ;; tops are used here because a binding free use of a set!'d variable
                 ;; is treated just the same as (#%top . x).
                 (add-id varsets (syntax var))  
                 (if (identifier-binding (syntax var) 0)
                     (add-id varrefs (syntax var))
                     (add-id tops (syntax var)))
                 
                 (loop (syntax e)))]
              [(quote datum)
               (annotate-raw-keyword stx-obj varrefs)]
              [(quote-syntax datum)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (let loop ([stx #'datum])
                   (cond [(identifier? stx)
                          (add-id templrefs stx)]
                         [(syntax? stx)
                          (loop (syntax-e stx))]
                         [(pair? stx)
                          (loop (car stx))
                          (loop (cdr stx))]
                         [(vector? stx)
                          (for-each loop (vector->list stx))]
                         [(box? stx)
                          (loop (unbox stx))]
                         [else (void)])))]
              [(with-continuation-mark a b c)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (loop (syntax a))
                 (loop (syntax b))
                 (tail-loop (syntax c)))]
              [(#%plain-app pieces ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (for-each loop (syntax->list (syntax (pieces ...)))))]
              [(#%top . var)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (add-id tops (syntax var)))]
              [(define-values vars b)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (add-binders (syntax vars) binders binding-inits #'b)
                 (maybe-jump (syntax vars))
                 (loop (syntax b)))]
              [(define-syntaxes names exp)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (add-binders (syntax names) binders binding-inits #'exp)
                 (maybe-jump (syntax names))
                 (level-loop (syntax exp) (+ level 1)))]
              [(begin-for-syntax exp ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (for-each (lambda (e) (level-loop e (+ level 1))) (syntax->list (syntax (exp ...)))))]
              [(module m-name lang (#%plain-module-begin bodies ...))
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (hash-set! module-lang-requires (syntax lang) #t)
                 (annotate-require-open user-namespace user-directory (syntax lang))
                 (hash-cons! requires (syntax->datum (syntax lang)) (syntax lang))
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              [(module* m-name lang (#%plain-module-begin bodies ...))
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (when (syntax-e #'lang)
                   (hash-set! module-lang-requires (syntax lang) #t)
                   (annotate-require-open user-namespace user-directory (syntax lang))
                   (hash-cons! requires (syntax->datum (syntax lang)) (syntax lang)))
                 
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              
              ; top level or module top level only:
              [(#%require raw-require-specs ...)
               (let ()
                 (define (handle-raw-require-spec spec)
                   (let loop ([spec spec]
                              [level level])
                     (define (add-to-level n) (and n level (+ n level)))
                     (syntax-case* spec (for-meta for-syntax for-template for-label just-meta) symbolic-compare?
                       [(for-meta phase specs ...)
                        (for ([spec (in-list (syntax->list #'(specs ...)))])
                          (loop spec (add-to-level (syntax-e #'phase))))]
                       [(for-syntax specs ...)
                        (for ([spec (in-list (syntax->list #'(specs ...)))])
                          (loop spec (add-to-level 1)))]
                       [(for-template specs ...)
                        (for ([spec (in-list (syntax->list #'(specs ...)))])
                          (loop spec (add-to-level -1)))]
                       [(for-label specs ...)
                        (for ([spec (in-list (syntax->list #'(specs ...)))])
                          (loop spec #f))]
                       [(just-meta phase specs ...)
                        (for ([spec (in-list (syntax->list #'(specs ...)))])
                          (handle-phaseless-spec spec (add-to-level (syntax-e #'phase))))]
                       [else
                        (handle-phaseless-spec spec level)])))
                 (define (handle-phaseless-spec stx level)
                   (define require-ht (hash-ref! phase-to-requires level
                                                 (λ () 
                                                   (define h (make-hash))
                                                   (hash-set! phase-to-requires level h)
                                                   h)))
                   (define raw-module-path (phaseless-spec->raw-module-path stx))
                   (annotate-require-open user-namespace user-directory raw-module-path)
                   (when (syntax-original? raw-module-path)
                     (define key (syntax->datum raw-module-path))
                     (hash-set! require-ht 
                                key
                                (cons stx (hash-ref require-ht key '())))))
                 
                 (for ([spec (in-list (syntax->list #'(raw-require-specs ...)))])
                   (handle-raw-require-spec spec)))]
              
              ; module top level only:
              [(#%provide provide-specs ...)
               (let ([provided-varss (map extract-provided-vars
                                          (syntax->list (syntax (provide-specs ...))))])
                 (annotate-raw-keyword stx-obj varrefs)
                 (for ([provided-vars (in-list provided-varss)])
                   (for ([provided-var (in-list provided-vars)])
                     (add-id varrefs provided-var))))]
              
              [(#%expression arg)
               (begin
                 (annotate-raw-keyword stx-obj varrefs)
                 (tail-loop #'arg))]
              [id
               (identifier? (syntax id))
               (add-id varrefs stx-obj)]
              [_
               (begin
                 #;
                 (printf "unknown stx: ~.s datum: ~e source: ~e\n"
                         sexp
                         (and (syntax? sexp)
                              (syntax->datum sexp))
                         (and (syntax? sexp)
                              (syntax-source sexp)))
                 (void))])))))
    
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
      
      (define unused-requires (make-hash))
      (define unused-require-for-syntaxes (make-hash))
      (define unused-require-for-templates (make-hash))
      (define unused-require-for-labels (make-hash))
      (define unused/phases (make-hash))

      ;; hash[(list (list src pos pos) (list src pos pos)) -o> #t   ;; indicates if this arrow has been recorded
      ;;      (list src pos pos) -o> (cons number number)]          ;; indicates the number of defs and uses at this spot
      (define connections (make-hash))

      (for ([(level requires) (in-hash phase-to-requires)])
        (define new-hash (make-hash))
        (hash-set! unused/phases level new-hash)
        (for ([(k v) (in-hash requires)])
          (hash-set! new-hash k #t)))
      
      (for ([(level binders) (in-hash phase-to-binders)])
        (for ([vars (in-list (get-idss binders))])
          (for ([var (in-list vars)])
            (define varset (lookup-phase-to-mapping phase-to-varsets level))
            (color-variable var 0 varset)
            (document-variable var 0))))
      
      (for ([(level varrefs) (in-hash phase-to-varrefs)])
        (define binders (lookup-phase-to-mapping phase-to-binders level))
        (define varsets (lookup-phase-to-mapping phase-to-varsets level)) 
        (for ([vars (in-list (get-idss varrefs))])
          (for ([var (in-list vars)])
            (color-variable var level varsets)
            (document-variable var level)
            (connect-identifier var
                                binders
                                unused/phases
                                phase-to-requires
                                level
                                user-namespace 
                                user-directory
                                #t
                                connections))))
      
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
            (document-variable var phase)
            (connect-identifier var
                                (lookup-phase-to-mapping phase-to-binders phase)
                                unused/phases
                                phase-to-requires
                                phase
                                user-namespace
                                user-directory
                                #f
                                connections))))
      
      (for ([(level tops) (in-hash phase-to-tops)])
        (define binders (lookup-phase-to-mapping phase-to-binders level))
        (for ([vars (in-list (get-idss tops))])
          (for ([var (in-list vars)])
            (color/connect-top user-namespace user-directory binders var connections))))
      
      (for ([(level require-hash) (in-hash phase-to-requires)])
        (define unused-hash (hash-ref unused/phases level))
        (color-unused require-hash unused-hash module-lang-requires))
      
      (annotate-counts connections)
      
      (make-rename-menus (list phase-to-binders phase-to-varrefs phase-to-tops)))
    
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
                       (color stx unused-require-style-name)))
                   (hash-ref requires k 
                             (λ ()
                               (error 'syncheck/traversals.rkt "requires doesn't have a mapping for ~s" k)))))))
    
    ;; id-level : integer-or-#f-or-'lexical identifier -> symbol
    (define (id-level phase-level id)
      (let ([binding (identifier-binding id phase-level)])
        (cond [(list? binding)
               (if (self-module? (car binding))
                   'top-level
                   'imported)]
              [(eq? binding 'lexical) 'lexical]
              [else 'top-level])))
    
    (define (self-module? mpi)
      (let-values ([(a b) (module-path-index-split mpi)])
        (and (not a) (not b))))
    
    ;; connect-identifier : syntax
    ;;                      id-set 
    ;;                      (union #f hash-table)
    ;;                      (union #f hash-table)
    ;;                      (union identifier-binding identifier-transformer-binding)
    ;;                      boolean
    ;;                      connections-table (see its defn)
    ;;                   -> void
    ;; adds the arrows that correspond to binders/bindings
    (define (connect-identifier var all-binders unused/phases phase-to-requires
                                phase-level user-namespace user-directory actual?
                                connections)
      (define binders (get-ids all-binders var))
      (when binders
        (for ([x (in-list binders)])
          (connect-syntaxes x var actual? (id-level phase-level x) connections)))
        
      (when (and unused/phases phase-to-requires)
        (define req-path/pr (get-module-req-path var phase-level))
        (define source-req-path/pr (get-module-req-path var phase-level #:nominal? #f))
        (when (and req-path/pr source-req-path/pr)
          (define req-path (list-ref req-path/pr 0))
          (define id (list-ref req-path/pr 1))
          (define source-req-path (list-ref source-req-path/pr 3))
          (define source-id (list-ref source-req-path/pr 1))
          (define req-phase-level (list-ref req-path/pr 2))
          (define require-ht (hash-ref! phase-to-requires req-phase-level #f))
          (when require-ht
            (define req-stxes (hash-ref require-ht req-path #f))
            (when req-stxes
              (define unused (hash-ref! unused/phases req-phase-level #f))
              (when unused (hash-remove! unused req-path))
              (for ([req-stx (in-list req-stxes)])
                (when (id/require-match? (syntax->datum var) 
                                         id 
                                         (syntax->datum req-stx))
                  (when id
                    (define filename (get-require-filename source-req-path user-namespace user-directory))
                    (when filename
                      (add-jump-to-definition
                       var
                       source-id
                       filename)))
                  (define raw-module-path (phaseless-spec->raw-module-path req-stx))
                  (add-mouse-over var
                                  (format 
                                   (string-constant cs-mouse-over-import)
                                   (syntax-e var)
                                   req-path))
                  (connect-syntaxes (if (syntax-source raw-module-path)
                                        raw-module-path
                                        req-stx)
                                    var actual?
                                    (id-level phase-level var)
                                    connections))))))))
             
    (define (id/require-match? var id req-stx)
      (match req-stx
        [`(only ,_ . ,ids)
         (and (memq id ids)
              (eq? var id))]
        [`(prefix ,prefix ,_)
         (equal? (format "~a~a" prefix id)
                 (symbol->string var))]
        [`(all-except ,_ . ,ids)
         (and (eq? var id)
              (not (member var ids)))]
        [`(prefix-all-except ,prefix ,_ . ,rest)
         (and (not (memq id rest))
              (equal? (format "~a~a" prefix id)
                      (symbol->string var)))]
        [`(rename ,_ ,local-id ,exported-id)
         (eq? local-id var)]
        [else (eq? var id)]))
    
    (define (phaseless-spec->raw-module-path stx)
      (syntax-case* stx (only prefix all-except prefix-all-except rename) symbolic-compare?
        [(only raw-module-path id ...) #'raw-module-path]
        [(prefix prefix-id raw-module-path) #'raw-module-path]
        [(all-except raw-module-path id ...) #'raw-module-path]
        [(prefix-all-except prefix-id raw-module-path id ...) #'raw-module-path]
        [(rename raw-module-path local-id exported-id) #'raw-module-path]
        [_ stx]))
    
    
    ;; get-module-req-path : binding number [#:nominal? boolean] -> (union #f (list require-sexp sym ?? module-path))
    ;; argument is the result of identifier-binding or identifier-transformer-binding
    (define (get-module-req-path var phase-level #:nominal? [nominal-source-path? #t])
      (define binding (identifier-binding var phase-level))
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
    
    ;; color/connect-top : namespace directory id-set syntax connections[see defn for ctc] -> void
    (define (color/connect-top user-namespace user-directory binders var connections)
      (let ([top-bound?
             (or (get-ids binders var)
                 (parameterize ([current-namespace user-namespace])
                   (let/ec k
                     (namespace-variable-value (syntax-e var) #t (λ () (k #f)))
                     #t)))])
        (cond
          [top-bound?
           (color var lexically-bound-variable-style-name)]
          [else
           (add-mouse-over var (format "~s is a free variable" (syntax-e var)))
           (color var free-variable-style-name)])
        (connect-identifier var binders #f #f 0 user-namespace user-directory #t connections)))
    
    ;; annotate-counts : connections[see defn] -> void
    ;; this function doesn't try to show the number of uses at
    ;; a use site, as it is not obvious how to compute that.
    ;; in particular, you could think of following arrows from
    ;; the use site back to the definition and then counting
    ;; the number of arrows originating there, but consider this example:
    ;; (define-syntax-rule (m x y z)
    ;;   (list (let ([y 1]) x x)
    ;;         (let ([z 1]) x)))
    ;; (m w w w)
    ;; if you do that here, then which def site do you pick? 
    ;; and note that picking both of them leads to double counting
    ;; it seems possible to have a different datastructure (one that
    ;; records the src locs of each 'end' position of each arrow)
    ;; to do this, but maybe lets leave that for another day.
    (define (annotate-counts connections)
      (for ([(key val) (in-hash connections)])
        (when (pair? val) 
          (define start (car val))
          (define end (cdr val))
          (define (show-starts)
            (add-mouse-over/loc (list-ref key 0) (list-ref key 1) (list-ref key 2) 
                                (cond
                                  [(zero? start)
                                   (string-constant cs-zero-varrefs)]
                                  [(= 1 start)
                                   (string-constant cs-one-varref)]
                                  [else
                                   (format (string-constant cs-n-varrefs) start)])))
          (define (show-ends)
            (unless (= 1 end)
              (add-mouse-over/loc (list-ref key 0) (list-ref key 1) (list-ref key 2) 
                                  (format (string-constant cs-binder-count) end))))
          (cond
            [(zero? end)   ;; assume this is a binder, show uses
             (show-starts)]
            [(zero? start) ;; assume this is a use, show bindings (usually just one, so do nothing)
             (show-ends)]
            [else          ;; crazyness, show both
             (show-starts)
             (show-ends)]))))
    
    ;; color-variable : syntax phase-level identifier-mapping -> void
    (define (color-variable var phase-level varsets)
      (define b (identifier-binding var phase-level))
      (define lexical? (is-lexical? b))
      (cond
        [(get-ids varsets var)
         (add-mouse-over var (string-constant cs-set!d-variable))
         (color var set!d-variable-style-name)]
        [lexical? (color var lexically-bound-variable-style-name)]
        [(pair? b) (color var imported-variable-style-name)]))
    
    (define (is-lexical? b)
      (or (not b)
          (eq? b 'lexical)
          (and (pair? b)
               (let ([path (caddr b)])
                 (and (module-path-index? path)
                      (self-module? path))))))
    
    ;; connect-syntaxes : syntax[original] syntax[original] boolean symbol connections -> void
    ;; adds an arrow from `from' to `to', unless they have the same source loc. 
    (define (connect-syntaxes from to actual? level connections)
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
                  (define connections-start (list from-source from-pos-left from-pos-right))
                  (define connections-end (list to-source to-pos-left to-pos-right))
                  (define connections-key (list connections-start connections-end))
                  (unless (hash-ref connections connections-key #f)
                    (hash-set! connections connections-key #t)
                    (define start-before (or (hash-ref connections connections-start #f) (cons 0 0)))
                    (define end-before (or (hash-ref connections connections-end #f) (cons 0 0)))
                    (hash-set! connections connections-start (cons (+ (car start-before) 1) (cdr start-before)))
                    (hash-set! connections connections-end (cons (car end-before) (+ 1 (cdr end-before)))))
                  (send defs-text syncheck:add-arrow
                        from-source from-pos-left from-pos-right
                        to-source to-pos-left to-pos-right
                        actual? level))))))))
    
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
    
    ;; annotate-require-open : namespace string -> (stx -> void)
    ;; relies on current-module-name-resolver, which in turn depends on
    ;; current-directory and current-namespace
    (define (annotate-require-open user-namespace user-directory require-spec)
      (when (syntax-original? require-spec)
        (define source (find-source-editor require-spec))
        (when (and source
                   (syntax-position require-spec)
                   (syntax-span require-spec))
          (define defs-text (current-annotations))
          (when defs-text
            (define start (- (syntax-position require-spec) 1))
            (define end (+ start (syntax-span require-spec)))
            (define file (get-require-filename (syntax->datum require-spec)
                                               user-namespace
                                               user-directory))
            (when file
              (send defs-text syncheck:add-require-open-menu
                    source start end file))))))
    
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
               (add-id id-set ct)]
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
        [(only module-name identifier ...)
         (syntax module-name)]
        [(prefix identifier module-name)
         (syntax module-name)]
        [(all-except module-name identifier ...)
         (syntax module-name)]
        [(prefix-all-except module-name identifier ...)
         (syntax module-name)]
        [(rename module-name local-identifier exported-identifier)
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
                     (when binding-to-init
                       (add-init-exp binding-to-init fst init-exp))
                     (add-id id-set fst)
                     (loop rst))
                   (loop rst)))]
            [(null? e) (void)]
            [else 
             (when binding-to-init
               (add-init-exp binding-to-init stx init-exp))
             (add-id id-set stx)]))))    
    
    ;; annotate-raw-keyword : syntax id-map -> void
    ;; annotates keywords when they were never expanded. eg.
    ;; if someone just types `(λ (x) x)' it has no 'origin
    ;; field, but there still are keywords.
    (define (annotate-raw-keyword stx id-map)
      (let ([lst (syntax-e stx)])
        (when (pair? lst)
          (let ([f-stx (car lst)])
            (when (identifier? f-stx)
              (add-id id-map f-stx))))))
    
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
      (define defs-text (current-annotations))
      (when defs-text
        (define binding-info (identifier-binding stx phase-level))
        (when (and (pair? binding-info)
                   (syntax-position stx)
                   (syntax-span stx))
          (define start (- (syntax-position stx) 1))
          (define fin (+ start (syntax-span stx)))
          (define source-editor (find-source-editor stx))
          (when source-editor
            (define info (get-index-entry-info binding-info))
            (when info
              (define-values (entry-desc path definition-tag tag) (apply values info))
              (send defs-text syncheck:add-background-color
                    source-editor start fin 
                    "palegreen")
              (send defs-text syncheck:add-docs-menu
                    source-editor
                    start 
                    fin 
                    (syntax-e stx)
                    (build-docs-label entry-desc)
                    path
                    definition-tag
                    tag))))))
    
    (define (build-docs-label entry-desc)
      (let ([libs (exported-index-desc-from-libs entry-desc)])
        (cond
          [(null? libs)
           (format
            (string-constant cs-view-docs)
            (exported-index-desc-name entry-desc))]
          [else
           (format
            (string-constant cs-view-docs-from)
            (format 
             (string-constant cs-view-docs)
             (exported-index-desc-name entry-desc))
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
      
      ;; table : symbol -o> (listof (pair (non-empty-listof identifier?)
      ;;                                  (non-empty-setof (list ed start fin))))
      ;; this table maps the names of identifiers to information that tells how to build
      ;; the rename menus.
      ;;
      ;; In the simple case that every identifier in the file has a different
      ;; name, then each of the symbols in the table will map to a singleton list where the
      ;; listof identifiers is also a singleton list and each of the '(list ed start fin)'
      ;; corresponds to the locations of that identifier in the file.
      ;;
      ;; In the more common case, there will be multiple, distinct uses of an identifier that
      ;; is spelled the same way in the file, eg (+ (let ([x 1]) x) (let ([x 2]) x)). In
      ;; this case, the 'x' entry in the table will point to a list of length two,
      ;; with each of the corresponding list of identifiers in the pair still being a 
      ;; singleton list.
      ;;
      ;; In the bizarro case, some macro will have taken an identifier from its input and
      ;; put it into two distinct binding locations, eg:
      ;; (define-syntax-rule (m x) (begin (define x 1) (lambda (x) x)))
      ;; In this case, there is only one 'x' in the original program, but there are two
      ;; distinct identifiers (according to free-identifier=?) in the program. To cope
      ;; with this, the code below recognizes that two distinct identifiers come from the
      ;; same source location and then puts those two identifiers into the first list into
      ;; the same 'pair' in the table, unioning the corresponding sets of source locations
      ;; 
      
      (define table (make-hash))
      (struct pair (ids locs) #:transparent)
      
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
                       (define var-sym (syntax-e var))
                       
                       (define current-pairs (hash-ref table var-sym '()))
                       (define free-id-matching-pair #f)
                       (define added-source-loc-sets '())
                       (define new-pairs
                         (for/list ([a-pair (in-list current-pairs)])
                           (define ids (pair-ids a-pair))
                           (define loc-set (pair-locs a-pair))
                           (cond
                             [(ormap (λ (this-id) (free-identifier=? this-id var)) ids)
                              (define new-pair (pair ids (set-add loc-set loc)))
                              (set! free-id-matching-pair new-pair)
                              new-pair]
                             [(set-member? loc-set loc)
                              ;; here we are in the biazarro case;
                              ;; we found this source location in a set that corresponds to
                              ;; some other identifier. so, we know we need to do some kind of a merger
                              ;; just keep track of the set for now, the merger happens after this loop
                              (set! added-source-loc-sets (cons a-pair added-source-loc-sets))
                              a-pair]
                             [else
                              a-pair])))
                       
                       ;; first step in updating the table; put the new set in.
                       (cond
                         [free-id-matching-pair
                          (hash-set! table var-sym new-pairs)]
                         [else
                          (set! free-id-matching-pair (pair (list var) (set loc)))
                          (hash-set! table var-sym (cons free-id-matching-pair new-pairs))])
                       
                       (unless (null? added-source-loc-sets)
                         ;; here we are in the bizarro case; we need to union the sets
                         ;; in the added-source-loc-sets list.
                         (define pairs-to-merge (cons free-id-matching-pair added-source-loc-sets))
                         (define removed-sets (filter (λ (x) (not (memq x pairs-to-merge))) 
                                                      (hash-ref table var-sym)))
                         (define new-pair (pair (apply append (map pair-ids pairs-to-merge))
                                                (apply set-union (map pair-locs pairs-to-merge))))
                         (hash-set! table var-sym (cons new-pair removed-sets))))))))))
          
          (hash-for-each
           table
           (λ (id-as-sym pairs)
             (for ([a-pair (in-list pairs)])
               (define loc-lst (set->list (pair-locs a-pair)))
               (define ids (pair-ids a-pair))
               (define (name-dup? new-str) 
                 (and (for/or ([phase-to-map (in-list phase-tos)])
                        (for/or ([(level id-set) (in-hash phase-to-map)])
                          (for/or ([id (in-list ids)])
                            (for/or ([corresponding-id (in-list (or (get-ids id-set id) '()))])
                              (let ([new-id (datum->syntax corresponding-id (string->symbol new-str))])
                                (for/or ([phase-to-map (in-list phase-tos)])
                                  (for/or ([(level id-set) (in-hash phase-to-map)])
                                    (get-ids id-set new-id))))))))
                      #t))
               (send defs-text syncheck:add-rename-menu
                     id-as-sym
                     loc-lst
                     name-dup?)))))))
    
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
      (when (syntax-original? id)
        (let* ([old (free-identifier-mapping-get mapping id (λ () '()))]
               [new (cons init-exp old)])
          (free-identifier-mapping-put! mapping id new))))
    
    ;; add-id : id-set identifier -> void
    (define (add-id mapping id)
      (when (syntax-original? id)
        (let* ([old (free-identifier-mapping-get mapping id (λ () '()))]
               [new (cons id old)])
          (free-identifier-mapping-put! mapping id new))))
    
    ;; get-idss : id-set -> (listof (listof identifier))
    (define (get-idss mapping)
      (free-identifier-mapping-map mapping (λ (x y) y)))
    
    ;; get-ids : id-set identifier -> (union (listof identifier) #f)
    (define (get-ids mapping var)
      (free-identifier-mapping-get mapping var (λ () #f)))
    
    ;; for-each-ids : id-set ((listof identifier) -> void) -> void
    (define (for-each-ids mapping f)
      (free-identifier-mapping-for-each mapping (λ (x y) (f y))))
