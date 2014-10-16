#lang racket/base

(require "colors.rkt"
         "syncheck-intf.rkt"
         "syncheck-local-member-names.rkt"
         "annotate.rkt"
         "contract-traversal.rkt"
         "xref.rkt"
         string-constants
         racket/unit
         racket/match
         racket/set
         racket/class
         racket/list
         racket/contract
         racket/pretty
         syntax/boundmap
         scribble/manual-struct
         (for-syntax racket/base))

(define-logger check-syntax)

(provide make-traversal
         current-max-to-send-at-once
         build-trace%)

(define current-max-to-send-at-once (make-parameter +inf.0))


    ;; make-traversal : namespace string[directory] -> (values (syntax (union #f syntax) -> void)
    ;;                                                         (-> void))
    ;; returns a pair of functions that close over some state that
    ;; represents the top-level of a single program. The first value
    ;; is called once for each top-level expression and the second
    ;; value is called once, after all expansion is complete.
    (define (make-traversal user-namespace user-directory [print-extra-info? #f])
      (let* ([tl-phase-to-binders (make-hash)]
             [tl-phase-to-varrefs (make-hash)]
             [tl-phase-to-varsets (make-hash)]
             [tl-phase-to-tops (make-hash)]
             [tl-binding-inits (make-id-set)]
             [tl-templrefs (make-id-set)]
             [tl-module-lang-requires (make-hash)]
             [tl-phase-to-requires (make-hash)]
             [tl-sub-identifier-binding-directives (make-hash)]
             [expanded-expression
              (λ (sexp [ignored void])
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
                             [require-for-labels (make-hash)]
                             [sub-identifier-binding-directives (make-hash)])
                          (annotate-basic sexp
                                          user-namespace user-directory
                                          phase-to-binders
                                          phase-to-varrefs
                                          phase-to-varsets
                                          phase-to-tops
                                          binding-inits
                                          templrefs
                                          module-lang-requires
                                          phase-to-requires
                                          sub-identifier-binding-directives)
                         (annotate-variables user-namespace
                                             user-directory
                                             phase-to-binders
                                             phase-to-varrefs
                                             phase-to-varsets
                                             phase-to-tops
                                             templrefs
                                             module-lang-requires
                                             phase-to-requires
                                             sub-identifier-binding-directives)
                         (annotate-contracts sexp 
                                             (hash-ref phase-to-binders 0 (λ () (make-id-set)))
                                             binding-inits)
                         (when print-extra-info?
                           (print-extra-info (list (list 'phase-to-binders phase-to-binders)
                                                   (list 'phase-to-varrefs phase-to-varrefs)
                                                   (list 'phase-to-varsets phase-to-varsets)
                                                   (list 'phase-to-tops phase-to-tops)
                                                   (list 'phase-to-requires phase-to-requires)
                                                   (list 'binding-inits binding-inits)
                                                   (list 'templrefs templrefs)
                                                   (list 'module-lang-requires module-lang-requires)
                                                   (list 'requires requires)
                                                   (list 'require-for-syntaxes require-for-syntaxes)
                                                   (list 'require-for-templates require-for-templates)
                                                   (list 'require-for-labels require-for-templates)
                                                   (list 'sub-identifier-binding-directives
                                                         sub-identifier-binding-directives)))
                           (define vars (set))
                           (let loop ([thing (list phase-to-varrefs phase-to-binders)])
                             (cond
                               [(pair? thing) (loop (car thing)) (loop (cdr thing))]
                               [(hash? thing) (for ([(k v) (in-hash thing)])
                                                (loop k)
                                                (loop v))]
                               [(free-identifier-mapping? thing)
                                (free-identifier-mapping-for-each
                                 thing
                                 (λ (k v) (loop v)))]
                               [(syntax? thing) (set! vars (set-add vars thing))]))
                           (printf "--- vars\n")
                           (for ([x (in-set vars)])
                             (for ([y (in-set vars)])
                               (printf "  ~s\n  ~s\n  ~s\n" x y (free-identifier=? x y))))
                           (printf "--- vars\n\n")))]
                      [else
                       (annotate-basic sexp
                                       user-namespace user-directory
                                       tl-phase-to-binders
                                       tl-phase-to-varrefs
                                       tl-phase-to-varsets
                                       tl-phase-to-tops
                                       tl-binding-inits
                                       tl-templrefs
                                       tl-module-lang-requires
                                       tl-phase-to-requires
                                       tl-sub-identifier-binding-directives)]))))]
             [expansion-completed
              (λ ()
                (parameterize ([current-directory (or user-directory (current-directory))]
                               [current-load-relative-directory user-directory])
                  (when print-extra-info?
                    (print-extra-info (list (list 'tl-phase-to-binders tl-phase-to-binders)
                                            (list 'tl-phase-to-varrefs tl-phase-to-varrefs)
                                            (list 'tl-phase-to-varsets tl-phase-to-varsets)
                                            (list 'tl-phase-to-tops tl-phase-to-tops)
                                            (list 'tl-templrefs tl-templrefs)
                                            (list 'tl-module-lang-requires tl-module-lang-requires)
                                            (list 'tl-phase-to-requires tl-module-lang-requires)
                                            (list 'tl-sub-identifier-binding-directives 
                                                  tl-sub-identifier-binding-directives))))
                  (annotate-variables user-namespace
                                      user-directory
                                      tl-phase-to-binders
                                      tl-phase-to-varrefs
                                      tl-phase-to-varsets
                                      tl-phase-to-tops
                                      tl-templrefs
                                      tl-module-lang-requires
                                      tl-phase-to-requires
                                      tl-sub-identifier-binding-directives)))])
        (values expanded-expression expansion-completed)))
    
    
(define (print-extra-info stuff)
  (for ([info (in-list stuff)])
    (printf "~s\n" (car info))
    (pretty-print
     (let loop ([info (cadr info)])
       (cond
         [(hash? info)
          (for/hash ([(k v) (in-hash info)])
            (values (loop k) (loop v)))]
         [(free-identifier-mapping? info)
          (free-identifier-mapping-map
           info
           (λ (k v) (list (loop k) '=> (loop v))))]
         [(pair? info) (cons (loop (car info)) (loop (cdr info)))]
         [else info])))
    (newline)))

    ;; type req/tag = (make-req/tag syntax sexp boolean)
    (define-struct req/tag (req-stx req-sexp used?))
    
    ;; annotate-basic : syntax 
    ;;                  namespace
    ;;                  string[directory]
    ;;                  id-set (8 of them)
    ;;                  hash-table[require-spec -> syntax] (three of them)
    ;;               -> void
    (define (annotate-basic stx-obj
                            user-namespace user-directory
                            phase-to-binders
                            phase-to-varrefs
                            phase-to-varsets
                            phase-to-tops
                            binding-inits
                            templrefs
                            module-lang-requires
                            phase-to-requires
                            sub-identifier-binding-directives)
      
      (let level+tail+mod-loop ([stx-obj stx-obj]
                                [level 0]
                                [level-of-enclosing-module 0]
                                [tail-parent-src #f]
                                [tail-parent-pos #f]
                                ;; mods: (or/c #f   ; => outside a module
                                ;;             '()  ; => inside the main module in this file
                                ;;             '(name names ...) ; => inside some submodules 
                                ;;                                    named by name & names
                                [mods #f])
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
          (define (sub-mods mod) (if mods (cons mod mods) '()))
          (let* ([level-loop (λ (sexp level) (level+tail+mod-loop sexp level level-of-enclosing-module
                                                                  #f #f
                                                                  mods))]
                 [tail-loop (λ (sexp) (level+tail+mod-loop sexp level level-of-enclosing-module
                                                           next-tail-parent-src next-tail-parent-pos
                                                           mods))]
                 [mod-loop (λ (sexp mod) (level+tail+mod-loop sexp 0
                                                              (+ level level-of-enclosing-module)
                                                              #f #f
                                                              (if mod 
                                                                  (sub-mods mod)
                                                                  mods)))]
                 [loop (λ (sexp) (level+tail+mod-loop sexp level level-of-enclosing-module
                                                      #f #f mods))]
                 [varrefs (lookup-phase-to-mapping phase-to-varrefs
                                                   (list (+ level level-of-enclosing-module) mods))]
                 [varsets (lookup-phase-to-mapping phase-to-varsets
                                                   (+ level level-of-enclosing-module))]
                 [binders (lookup-phase-to-mapping phase-to-binders
                                                   (+ level level-of-enclosing-module))]
                 [tops (lookup-phase-to-mapping phase-to-tops (+ level level-of-enclosing-module))]
                 [requires (hash-ref! phase-to-requires 
                                      (list (+ level level-of-enclosing-module) mods)
                                      (λ () (make-hash)))]
                 [collect-general-info
                  (λ (stx)
                    (add-origins stx varrefs level-of-enclosing-module)
                    (add-disappeared-bindings stx binders varrefs level-of-enclosing-module)
                    (add-disappeared-uses stx varrefs level-of-enclosing-module)
                    (add-mouse-over-tooltips stx)
                    (add-sub-range-binders stx 
                                           sub-identifier-binding-directives
                                           level
                                           level-of-enclosing-module
                                           mods))])
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
            
            (define (add-module-lang-require stx) 
              (define key (list (syntax-source stx)
                                (syntax-position stx)
                                (syntax-span stx)))
              (hash-set! module-lang-requires key #t))
            (syntax-case* stx-obj (#%plain-lambda case-lambda if begin begin0 let-values letrec-values
                                                  set! quote quote-syntax with-continuation-mark
                                                  #%plain-app #%top #%plain-module-begin
                                                  define-values define-syntaxes begin-for-syntax
                                                  module module*
                                                  #%require #%provide #%declare #%expression)
              (λ (x y) (free-identifier=? x y level 0))
              [(#%plain-lambda args bodies ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (add-binders (syntax args) binders #f #f level-of-enclosing-module)
                 (list-loop/tail-last (syntax->list (syntax (bodies ...)))))]
              [(case-lambda [argss bodiess ...]...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (for-each
                  (λ (args bodies)
                    (add-binders args binders #f #f level-of-enclosing-module)
                    (list-loop/tail-last (syntax->list bodies)))
                  (syntax->list (syntax (argss ...)))
                  (syntax->list (syntax ((bodiess ...) ...)))))]
              [(if test then else)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (loop (syntax test))
                 (tail-loop (syntax then))
                 (tail-loop (syntax else)))]
              [(begin bodies ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (list-loop/tail-last (syntax->list (syntax (bodies ...)))))]
              
              ;; treat a single body expression specially, since this has
              ;; different tail behavior.
              [(begin0 body)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (tail-loop (syntax body)))]
              
              [(begin0 bodies ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (for-each loop (syntax->list (syntax (bodies ...)))))]
              
              [(let-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x es) (add-binders x binders binding-inits es 
                                                    level-of-enclosing-module))
                             (syntax->list (syntax ((xss ...) ...)))
                             (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (list-loop/tail-last (syntax->list (syntax (bs ...))))))]
              [(letrec-values (bindings ...) bs ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (for-each collect-general-info (syntax->list (syntax (bindings ...))))
                 (with-syntax ([(((xss ...) es) ...) (syntax (bindings ...))])
                   (for-each (λ (x es) (add-binders x binders binding-inits es
                                                    level-of-enclosing-module))
                             (syntax->list (syntax ((xss ...) ...)))
                             (syntax->list (syntax (es ...))))
                   (for-each loop (syntax->list (syntax (es ...))))
                   (list-loop/tail-last (syntax->list (syntax (bs ...))))))]
              [(set! var e)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 
                 ;; tops are used here because a binding free use of a set!'d variable
                 ;; is treated just the same as (#%top . x).
                 (add-id varsets (syntax var) level-of-enclosing-module)
                 (if (identifier-binding (syntax var) 0)
                     (add-id varrefs (syntax var) level-of-enclosing-module)
                     (add-id tops (syntax var) level-of-enclosing-module))
                 
                 (loop (syntax e)))]
              [(quote datum)
               (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)]
              [(quote-syntax datum)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (let loop ([stx #'datum])
                   (cond [(identifier? stx)
                          (add-id templrefs stx level-of-enclosing-module)]
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
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (loop (syntax a))
                 (loop (syntax b))
                 (tail-loop (syntax c)))]
              [(#%plain-app pieces ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (for-each loop (syntax->list (syntax (pieces ...)))))]
              [(#%top . var)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (add-id tops (syntax var) level-of-enclosing-module))]
              [(define-values vars b)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (add-binders (syntax vars) binders binding-inits #'b level-of-enclosing-module)
                 (add-definition-target (syntax vars) mods)
                 (loop (syntax b)))]
              [(define-syntaxes names exp)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (add-binders (syntax names) binders binding-inits #'exp level-of-enclosing-module)
                 (add-definition-target (syntax names) mods)
                 (level-loop (syntax exp) (+ level 1)))]
              [(begin-for-syntax exp ...)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (for ([e (in-list (syntax->list (syntax (exp ...))))])
                   (level-loop e (+ level 1))))]
              [(module m-name lang (#%plain-module-begin bodies ...))
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (add-module-lang-require (syntax lang))
                 (annotate-require-open user-namespace user-directory (syntax lang))
                 (define module-name (syntax-e #'m-name))
                 (define sub-requires
                   (hash-ref! phase-to-requires 
                              (list (+ level level-of-enclosing-module) (sub-mods module-name))
                              (λ () (make-hash))))
                 (hash-cons! sub-requires (syntax->datum (syntax lang)) (syntax lang))
                 (for ([body (in-list (syntax->list (syntax (bodies ...))))])
                   (mod-loop body module-name)))]
              [(module* m-name lang (#%plain-module-begin bodies ...))
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (define module-name (syntax-e #'m-name))
                 (when (syntax-e #'lang)
                   (add-module-lang-require (syntax lang))
                   (annotate-require-open user-namespace user-directory (syntax lang))
                   (define sub-requires
                     (hash-ref! phase-to-requires 
                                (list (+ level level-of-enclosing-module) (sub-mods module-name))
                                (λ () (make-hash))))
                   (hash-cons! sub-requires (syntax->datum (syntax lang)) (syntax lang)))
                 
                 (for ([body (in-list (syntax->list (syntax (bodies ...))))])
                   (if (syntax-e #'lang)
                       (mod-loop body module-name)
                       (mod-loop body #f))))]
              
              
              ; top level or module top level only:
              [(#%require raw-require-specs ...)
               (let ()
                 (define (handle-raw-require-spec spec)
                   (let loop ([spec spec]
                              [level level])
                     (define (add-to-level n) (and n level (+ n level)))
                     (syntax-case* spec (for-meta for-syntax for-template for-label just-meta)
                       symbolic-compare?
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
                   (define adjusted-level (and level (+ level level-of-enclosing-module)))
                   (define require-ht (hash-ref! phase-to-requires
                                                 (list adjusted-level mods)
                                                 (λ () (make-hash))))
                   (define raw-module-path (phaseless-spec->raw-module-path stx))
                   (annotate-require-open user-namespace user-directory raw-module-path)
                   (when (original-enough? raw-module-path)
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
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (for ([provided-vars (in-list provided-varss)])
                   (for ([provided-var (in-list provided-vars)])
                     (add-id varrefs provided-var level-of-enclosing-module))))]

              ; module top level only:
              [(#%declare declare-specs ...)
               (void)]
              
              [(#%expression arg)
               (begin
                 (annotate-raw-keyword stx-obj varrefs level-of-enclosing-module)
                 (tail-loop #'arg))]
              [id
               (identifier? (syntax id))
               (add-id varrefs stx-obj level-of-enclosing-module)]
              [_
               (begin
                 #;
                 (printf "unknown stx: ~.s datum: ~e source: ~e\n"
                         sexp
                         (and (syntax? sexp)
                              (syntax->datum sexp))
                         (and (syntax? sexp)
                              (syntax-source sexp)))
                 (void))]))))
    
    (define (hash-cons! ht k v)
      (hash-set! ht k (cons v (hash-ref ht k '()))))

    (define sub-range-binder-prop?
      (vector/c #:flat? #t
                syntax? exact-nonnegative-integer? exact-nonnegative-integer?
                syntax? exact-nonnegative-integer? exact-nonnegative-integer?))
    (define (add-sub-range-binders stx 
                                   sub-identifier-binding-directives 
                                   level
                                   level-of-enclosing-module
                                   mods)
      (let loop ([prop (syntax-property stx 'sub-range-binders)])
        (cond
          [(pair? prop)
           (loop (car prop))
           (loop (cdr prop))]
          [(sub-range-binder-prop? prop) 
           (define new-entry
             (vector (syntax-shift-phase-level (vector-ref prop 0) level-of-enclosing-module)
                     (vector-ref prop 1)
                     (vector-ref prop 2)
                     (syntax-shift-phase-level (vector-ref prop 3) level-of-enclosing-module)
                     (vector-ref prop 4)
                     (vector-ref prop 5)))
           (define key (list level mods)) 
           (hash-set! sub-identifier-binding-directives
                      key
                      (cons new-entry
                            (hash-ref sub-identifier-binding-directives key '())))]
          [(vector? prop)
           (log-check-syntax-debug
            "found a vector in a 'sub-range-binders property that is ill-formed ~s"
            prop)])))

(define mouse-over-tooltip-prop?
  (vector/c #:flat? #t syntax? exact-nonnegative-integer? exact-nonnegative-integer? string?))
(define (add-mouse-over-tooltips stx)
  (let loop ([prop (syntax-property stx 'mouse-over-tooltips)])
    (cond
      [(pair? prop)
       (loop (car prop))
       (loop (cdr prop))]
      [(mouse-over-tooltip-prop? prop)
       (add-mouse-over/loc (find-source-editor (vector-ref prop 0))
                           (vector-ref prop 1)
                           (vector-ref prop 2)
                           (vector-ref prop 3))])))

    ;; add-disappeared-bindings : syntax id-set integer -> void
    (define (add-disappeared-bindings stx binders disappaeared-uses level-of-enclosing-module)
      (let ([prop (syntax-property stx 'disappeared-binding)])
        (when prop
          (let loop ([prop prop])
            (cond
              [(pair? prop)
               (loop (car prop))
               (loop (cdr prop))]
              [(identifier? prop)
               (add-origins prop disappaeared-uses level-of-enclosing-module)
               (add-id binders prop level-of-enclosing-module)])))))
    
    ;; add-disappeared-uses : syntax id-set integer -> void
    (define (add-disappeared-uses stx id-set level-of-enclosing-module)
      (let ([prop (syntax-property stx 'disappeared-use)])
        (when prop
          (let loop ([prop prop])
            (cond
              [(pair? prop)
               (loop (car prop))
               (loop (cdr prop))]
              [(identifier? prop)
               (add-id id-set prop level-of-enclosing-module)])))))
    
    ;; annotate-variables : namespace directory string id-set[four of them] 
    ;;                      (listof syntax) (listof syntax) 
    ;;                      hash[phase -o> sub-identifier-binding-directive]
    ;;                   -> void
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
                                phase-to-requires
                                sub-identifier-binding-directives)
      
      (define unused/phases (make-hash))

      ;; hash[(list (list src pos pos) (list src pos pos)) -o> #t
      ;;       above indicates if this arrow has been recorded
      ;;       below indicates the number of defs and uses at this spot
      ;;      (list src pos pos) -o> (cons number number)]
      (define connections (make-hash))

      (for ([(level+mods requires) (in-hash phase-to-requires)])
        (define new-hash (make-hash))
        (hash-set! unused/phases level+mods new-hash)
        (for ([(k v) (in-hash requires)])
          (hash-set! new-hash k #t)))
      
      (for ([(level binders) (in-hash phase-to-binders)])
        (for ([vars (in-list (get-idss binders))])
          (for ([var (in-list vars)])
            (define varset (lookup-phase-to-mapping phase-to-varsets level))
            (color-variable var 0 varset)
            (document-variable var 0))))
      
      (for ([(level+mods varrefs) (in-hash phase-to-varrefs)])
        (define level (list-ref level+mods 0))
        (define mods (list-ref level+mods 1))
        (define binders (lookup-phase-to-mapping phase-to-binders level))
        (define varsets (lookup-phase-to-mapping phase-to-varsets level)) 
        (for ([vars (in-list (get-idss varrefs))])
          (for ([var (in-list vars)])
            (color-variable var level varsets)
            (document-variable var level)
            (connect-identifier var
                                mods
                                binders
                                unused/phases
                                phase-to-requires
                                level
                                user-namespace 
                                user-directory
                                #t
                                connections))))
      
      
      ;; build a set of all of the known phases
      (define phases (set))
      (define all-mods (set))
      (for ([(phase _) (in-hash phase-to-binders)])
        (set! phases (set-add phases phase)))
      (for ([(phase+mod _) (in-hash phase-to-requires)])
        (define phase (list-ref phase+mod 0))
        (define mod (list-ref phase+mod 1))
        (set! phases (set-add phases phase))
        (set! all-mods (set-add all-mods mod)))
      
      (for ([vars (in-list (get-idss templrefs))])
        (for ([var (in-list vars)])
          
          ;; connect every identifier inside a quote-syntax to
          ;; each binder, at any phase, in any submodule
          (for* ([phase (in-set phases)]
                 [mod (in-set all-mods)])
            (document-variable var phase)
            (connect-identifier var
                                mod
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
      
      (for ([(phase+mods require-hash) (in-hash phase-to-requires)])
        (define unused-hash (hash-ref unused/phases phase+mods))
        (color-unused require-hash unused-hash module-lang-requires))
      
      (annotate-counts connections)
      
      (for ([(level+mods directives) (in-hash sub-identifier-binding-directives)])
        (define phase-level (list-ref level+mods 0))
        (define mods (list-ref level+mods 1))
        (for ([directive (in-list directives)])
          (match-define (vector binding-id to-start to-span new-binding-id from-start from-span)
            directive)
          (define all-varrefs (lookup-phase-to-mapping phase-to-varrefs (list phase-level mods)))
          (define all-binders (lookup-phase-to-mapping phase-to-binders phase-level))
          (define varrefs (get-ids all-varrefs binding-id))
          (when varrefs
            (for ([varref (in-list varrefs)])
              (connect-syntaxes new-binding-id varref #t all-binders 
                                (id-level phase-level new-binding-id)
                                connections #f
                                #:from-start from-start #:from-width from-span
                                #:to-start to-start #:to-width to-span))))))
    
    ;; color-unused : hash-table[sexp -o> syntax] hash-table[sexp -o> #f] hash-table[syntax -o> #t]
    ;;             -> void
    (define (color-unused requires unused module-lang-requires)
      (for ([(k v) (in-hash unused)])
        (define requires-stxes
          (hash-ref requires k
                    (λ ()
                      (error 'syncheck/traversals.rkt 
                             "requires doesn't have a mapping for ~s"
                             k))))
        (for ([stx (in-list requires-stxes)])
          (unless (hash-ref module-lang-requires (list (syntax-source stx)
                                                       (syntax-position stx)
                                                       (syntax-span stx)) #f)
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
            (color stx unused-require-style-name)))))
    
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
    ;;                      (or/c #f (listof symbol)) -- name of enclosing sub-modules
    ;;                      id-set 
    ;;                      (union #f hash-table)
    ;;                      (union #f hash-table)
    ;;                      (union identifier-binding identifier-transformer-binding)
    ;;                      boolean
    ;;                      connections-table (see its defn)
    ;;                   -> void
    ;; adds the arrows that correspond to binders/bindings
    (define (connect-identifier var mods all-binders unused/phases phase-to-requires
                                phase-level user-namespace user-directory actual?
                                connections)
      (define binders (get-ids all-binders var))
      (when binders
        (for ([x (in-list binders)])
          (connect-syntaxes x var actual? all-binders (id-level phase-level x) connections #f)))
      
      (when (and unused/phases phase-to-requires)
        (define req-path/pr (get-module-req-path var phase-level))
        (define source-req-path/pr (get-module-req-path var phase-level #:nominal? #f))
        (when (and req-path/pr source-req-path/pr)
          (define req-path (list-ref req-path/pr 0))
          (define id (list-ref req-path/pr 1))
          (define source-req-path (list-ref source-req-path/pr 3))
          (define source-id (list-ref source-req-path/pr 1))
          (define req-phase-level (list-ref req-path/pr 2))
          (define require-hash-key (list req-phase-level mods))
          (define require-ht (hash-ref phase-to-requires require-hash-key #f))
          (when require-ht
            (define req-stxes (hash-ref require-ht req-path #f))
            (when req-stxes
              (define unused (hash-ref unused/phases require-hash-key #f))
              (when unused (hash-remove! unused req-path))
              (for ([req-stx (in-list req-stxes)])
                (when (id/require-match? (syntax->datum var)
                                         id
                                         (syntax->datum req-stx))
                  (when id
                    (define-values (filename submods) 
                      (get-require-filename source-req-path user-namespace user-directory))
                    (when filename
                      (add-jump-to-definition
                       var
                       source-id
                       filename
                       submods)))
                  (define raw-module-path (phaseless-spec->raw-module-path req-stx))
                  (add-mouse-over var
                                  (format
                                   (string-constant cs-mouse-over-import)
                                   (syntax-e var)
                                   req-path))
                  (connect-syntaxes (if (syntax-source raw-module-path)
                                        raw-module-path
                                        req-stx)
                                    var actual? all-binders
                                    (id-level phase-level var)
                                    connections
                                    #t))))))))
             
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
    
    
    ;; get-module-req-path : identifier number [#:nominal? boolean] 
    ;;                    -> (union #f (list require-sexp sym ?? module-path))
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
        (connect-identifier var #f binders #f #f 0 user-namespace user-directory #t connections)))
    
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
    (define (connect-syntaxes from to actual? all-binders level connections require-arrow?
                              #:from-start [from-start 0] 
                              #:from-width [from-width (syntax-span from)]
                              #:to-start [to-start 0] 
                              #:to-width [to-width (syntax-span to)])
      (let ([from-source (find-source-editor from)] 
            [to-source (find-source-editor to)]
            [defs-text (current-annotations)])
        (when (and from-source to-source defs-text)
          (let ([pos-from (syntax-position from)]
                [span-from (syntax-span from)]
                [pos-to (syntax-position to)]
                [span-to (syntax-span to)])
            (when (and pos-from span-from pos-to span-to)
              (let* ([from-pos-left (+ (syntax-position from) -1 from-start)]
                     [from-pos-right (+ from-pos-left from-width)]
                     [to-pos-left (+ (syntax-position to) -1 to-start)]
                     [to-pos-right (+ to-pos-left to-width)])
                (unless (= from-pos-left to-pos-left)
                  (define connections-start (list from-source from-pos-left from-pos-right))
                  (define connections-end (list to-source to-pos-left to-pos-right))
                  (define connections-key (list connections-start connections-end))
                  (unless (hash-ref connections connections-key #f)
                    (hash-set! connections connections-key #t)
                    (define start-before (or (hash-ref connections connections-start #f) (cons 0 0)))
                    (define end-before (or (hash-ref connections connections-end #f) (cons 0 0)))
                    (hash-set! connections connections-start (cons (+ (car start-before) 1) 
                                                                   (cdr start-before)))
                    (hash-set! connections connections-end (cons (car end-before)
                                                                 (+ 1 (cdr end-before)))))
                  (define (name-dup? str)
                    (define sym (string->symbol str))
                    (define id1 (datum->syntax from sym))
                    (define id2 (datum->syntax to sym)) ;; do I need both?
                    (define ans #f)
                    (for-each-ids 
                     all-binders
                     (λ (ids)
                       (set! ans (or ans
                                     (for/or ([id (in-list ids)])
                                       (or (free-identifier=? id1 id)
                                           (free-identifier=? id2 id)))))))
                    ans)
                  (send defs-text syncheck:add-arrow/name-dup
                        from-source from-pos-left from-pos-right
                        to-source to-pos-left to-pos-right
                        actual? level require-arrow? name-dup?))))))))
    
    ;; add-jump-to-definition : syntax symbol path -> void
    ;; registers the range in the editor so that the
    ;; popup menu in this area allows the programmer to jump
    ;; to the definition of the id.
    (define (add-jump-to-definition stx id filename submods)
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
                  filename
                  submods)))))
    
    ;; annotate-require-open : namespace string -> (stx -> void)
    ;; relies on current-module-name-resolver, which in turn depends on
    ;; current-directory and current-namespace
    (define (annotate-require-open user-namespace user-directory require-spec)
      (when (original-enough? require-spec)
        (define source (find-source-editor require-spec))
        (when (and source
                   (syntax-position require-spec)
                   (syntax-span require-spec))
          (define defs-text (current-annotations))
          (when defs-text
            (define start (- (syntax-position require-spec) 1))
            (define end (+ start (syntax-span require-spec)))
            (define-values (file submods)
              (get-require-filename (syntax->datum require-spec)
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
        (define mpi
          (with-handlers ([exn:fail? (λ (x) #f)])
            (cond
              [(module-path-index? datum)
               (module-path-index-resolve datum)]
              [else
               ((current-module-name-resolver) datum #f #f #t)])))
        (define rkt-path/mod-path (and mpi (resolved-module-path-name mpi)))
        (define rkt-path/f (cond
                             [(path? rkt-path/mod-path) rkt-path/mod-path]
                             [(and (pair? rkt-path/mod-path)
                                   (path? (car rkt-path/mod-path)))
                              (car rkt-path/mod-path)]
                             [else #f]))
        (define rkt-submods (cond
                              [(not rkt-path/mod-path) #f]
                              [(or (symbol? rkt-path/mod-path) (path? rkt-path/mod-path)) '()]
                              [(pair? rkt-path/mod-path) (cdr rkt-path/mod-path)]))
        (define cleaned-up-path
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
                ss-path))))
        (values cleaned-up-path rkt-submods)))
    
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
    
    ;; add-origins : sexp id-set integer -> void
    (define (add-origins sexp id-set level-of-enclosing-module)
      (let ([origin (syntax-property sexp 'origin)])
        (when origin
          (let loop ([ct origin])
            (cond
              [(pair? ct)
               (loop (car ct))
               (loop (cdr ct))]
              [(syntax? ct)
               (add-id id-set ct level-of-enclosing-module)]
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
      (syntax-case* require-spec (only prefix all-except prefix-all-except rename just-meta) 
        symbolic-compare?
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
    
    ;; add-binders : syntax id-set (or/c #f id-set) (or/c #f syntax) integer -> void
    ;; transforms an argument list into a bunch of symbols/symbols
    ;; and puts them into the id-set
    ;; effect: colors the identifiers
    (define (add-binders stx id-set binding-to-init init-exp level-of-enclosing-module)
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
                     (add-id id-set fst level-of-enclosing-module)
                     (loop rst))
                   (loop rst)))]
            [(null? e) (void)]
            [else
             (when binding-to-init
               (add-init-exp binding-to-init stx init-exp))
             (add-id id-set stx level-of-enclosing-module)]))))
    
    ;; add-definition-target : syntax[(sequence of identifiers)] (listof symbol) -> void
    (define (add-definition-target stx mods)
      (when mods
        (define defs-text (current-annotations))
        (for ([id (in-list (syntax->list stx))])
          (define source (syntax-source id))
          (when (and source
                     defs-text
                     (syntax-position id)
                     (syntax-span id))
            (let* ([pos-left (- (syntax-position id) 1)]
                   [pos-right (+ pos-left (syntax-span id))])
              (send defs-text syncheck:add-definition-target
                    source
                    pos-left
                    pos-right
                    (syntax-e id)
                    mods))))))
    
    ;; annotate-raw-keyword : syntax id-map integer -> void
    ;; annotates keywords when they were never expanded. eg.
    ;; if someone just types `(λ (x) x)' it has no 'origin
    ;; field, but there still are keywords.
    (define (annotate-raw-keyword stx id-map level-of-enclosing-module)
      (let ([lst (syntax-e stx)])
        (when (pair? lst)
          (let ([f-stx (car lst)])
            (when (identifier? f-stx)
              (add-id id-map f-stx level-of-enclosing-module))))))
    
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
      (when (original-enough? id)
        (let* ([old (free-identifier-mapping-get mapping id (λ () '()))]
               [new (cons init-exp old)])
          (free-identifier-mapping-put! mapping id new))))
    
    ;; add-id : id-set identifier -> void
    (define (add-id mapping id level-of-enclosing-module)
      (when (original-enough? id)
        (let* ([id (syntax-shift-phase-level id level-of-enclosing-module)]
               [old (free-identifier-mapping-get mapping id (λ () '()))]
               [new (cons id old)])
          (free-identifier-mapping-put! mapping id new))))
    
    (define (original-enough? x)
      (or (syntax-original? x)
          (syntax-property x 'original-for-check-syntax)))
    
    ;; get-idss : id-set -> (listof (listof identifier))
    (define (get-idss mapping)
      (free-identifier-mapping-map mapping (λ (x y) y)))
    
    ;; get-ids : id-set identifier -> (union (listof identifier) #f)
    (define (get-ids mapping var)
      (free-identifier-mapping-get mapping var (λ () #f)))
    
    ;; for-each-ids : id-set ((listof identifier) -> void) -> void
    (define (for-each-ids mapping f)
      (free-identifier-mapping-for-each mapping (λ (x y) (f y))))




(define build-trace%
  (class (annotations-mixin object%)
    (init-field src)
    (define trace '())

    (define/override (syncheck:find-source-object stx)
      (and (equal? src (syntax-source stx))
           src))
    
    ;; send over the non _ variables in the message to the main drracket place
    (define-syntax (log stx)
      (syntax-case stx ()
        [(_ name args ...)
         (with-syntax ([(wanted-args ...)
                        (filter (λ (x) (not (regexp-match #rx"^_" (symbol->string (syntax-e x)))))
                                (syntax->list #'(args ...)))])
           #'(define/override (name args ...)
               (add-to-trace (vector 'name wanted-args ...))))]))

    (log syncheck:add-tail-arrow _from-text from-pos _to-text to-pos)
    (log syncheck:add-arrow/name-dup
         _start-text start-pos-left start-pos-right
         _end-text end-pos-left end-pos-right
         actual? level require-arrow? name-dup?)
    (log syncheck:add-mouse-over-status _text pos-left pos-right str)
    (log syncheck:add-background-color _text color start fin)
    (log syncheck:add-jump-to-definition _text start end id filename submods)
    (log syncheck:add-definition-target _text start-pos end-pos id mods)
    (log syncheck:add-require-open-menu _text start-pos end-pos file)
    (log syncheck:add-docs-menu _text start-pos end-pos key the-label path definition-tag tag)
    (log syncheck:add-id-set to-be-renamed/poss dup-name?)
    
    (define/public (get-trace) (reverse trace))
    (define/public (add-to-trace thing) 
      (set! trace (cons thing trace)))
    (super-new)))
