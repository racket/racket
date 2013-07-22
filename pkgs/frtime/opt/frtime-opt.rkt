
;; This module serves as a language module for an optimized version of FrTime.
;; The only thing it exports is a #%module-begin macro that knows how to 
;; perform optimization of FrTime functions.  The rest of the language 
;; (i.e. all the functions needed to actually write FrTime programs) is 
;; provided by the frtime-opt-lang module, which is automatically imported.
(module frtime-opt racket
  (provide (rename-out [my-module-begin #%module-begin])
           #%app #%top #%datum optimize-expr optimize-module dont-optimize)
  
  (require (for-syntax frtime/opt/lowered-equivs)
           (for-syntax (only-in srfi/1 lset-union lset-difference every))
           (for-syntax racket/list))
  (require (only-in frtime/core/frp super-lift undefined undefined?))
  (require (rename-in (except-in frtime/lang-ext undefined undefined? deep-value-now) [lift frtime:lift])
           (rename-in frtime/lang-core [if frtime:if])
           (only-in frtime/lang-core frp:copy-list))
;  (require mzlib/unit mzlib/unitsig)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Helper functions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; prevent a subexpression from being optimized.  Calls to this function
  ;; are specifically recognized, and handled specially.
  (define (dont-optimize x) x)
  
  ;; like module-identifier=?, but returns #f for non-identifier arguments
  ;; instead of throwing an exception
  (define-for-syntax (safe-module-identifier=? id1 id2)
    (and (identifier? id1)
         (identifier? id2)
         (free-identifier=? id1 id2)))
  
  ;; Convert a syntax-object to a datum and back again.  This replaces all
  ;; the context information.  It's necessary to get generated require 
  ;; statements to work, for some reason.
  ;; See http://list.cs.brown.edu/pipermail/plt-scheme/2006-July/014163.html
  (define-for-syntax (so->d->so ref-stx stx)
    (datum->syntax ref-stx (syntax->datum stx)))
  
  ;; Convert a syntactic module reference to a module-path-index
  (define-for-syntax (module-stx-to-path-index mod-stx)
    (let ([mod (syntax->datum mod-stx)])
      (if (symbol? mod) 
          mod
          (module-path-index-join mod #f))))
  
  ;; Convert a syntactic module reference to a module-path (this is 
  ;; subtly different from a module-path-index)
  (define-for-syntax (module-stx-to-path mod-stx)
    (syntax->datum mod-stx))
  
  ;; Does the given module export the given id?
  (define-for-syntax (module-exports-id? mod-stx id-stx)
    (not (module-provide-protected? (module-stx-to-path-index mod-stx) 
                                    (syntax-e id-stx))))

  ;; Returns all the identifiers exported by a module
  (define-for-syntax (all-provided-ids mod-stx ref-stx)
    ;; we get exn:fail:filesystem if the module doesn't exist.
    ;; we get exn:fail:contract during compilation if compiling a module that 
    ;; imports mred.
    (with-handlers ([exn:fail:filesystem? (lambda (exn) null)]
                    [exn:fail:contract? (lambda (exn) null)])
      (let ([mod-path (module-stx-to-path mod-stx)]
            [mod-path-index (module-stx-to-path-index mod-stx)])
        ;; instantiate the module so we can call module->namespace on it
        (dynamic-require mod-path #f)
        ;; get the list of provided symbols
        (let* ([all-symbols (namespace-mapped-symbols (module->namespace mod-path))]
               [exported-symbols (filter 
                                  (lambda (id) (not (module-provide-protected? mod-path-index id)))
                                  all-symbols)]
               [exported-ids (map
                              (lambda (symbol) (datum->syntax ref-stx symbol))
                              exported-symbols)])
          exported-ids))))
  
  ;; This macro takes a list of variables and an expression.
  ;; The variables are projected before evaluating the expression,
  ;; and the result is then injected into the dataflow graph as a
  ;; single node.
  (require (only-in frtime/core/frp proc->signal value-now))
  (define-syntax (dip stx)
    (syntax-case stx (begin)
      ;; special case: don't dip lone identifiers
      [(_ (VAR ...) VAR2)
       (identifier? #'VAR2)
       #'VAR2]
      
      ;; special case: strip off unnecessary begins
      [(_ (VAR ...) (begin E))
       #'(dip (VAR ...) E)]

      ;; special case: don't bother dipping if there are no dependencies
      [(_ () EXPR)
       #'EXPR]
      
      ;; general case: wrap the subexpression in a lambda, and lift it
      [(_ (VAR ...) EXPR)
       #'(frtime:lift 
          #t
          (lambda (VAR ...) EXPR)
          VAR ...)]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Module-level optimization
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

  ;; Wrap the entire module body in an optimize-module macro.
  (define-syntax (my-module-begin stx)
    (syntax-case stx ()
      [(_ FORMS ...)
       (let* (;; get a list of all the symbols provided by the frtime-opt-lang module
              [lang-symbols (all-provided-ids #'frtime/opt/frtime-opt-lang stx)]
              ;; convert those symbols into an equiv-map by pairing up functions
              ;; with their lowered equivalents
              [lang-equiv-map (symbol-list-to-equiv-map lang-symbols)]
              ;; convert the equiv-map to syntax so we can embed it in the call
              ;; to optimize-module
              [equiv-map-stx (equiv-map-to-stx lang-equiv-map)])
         #`(#%plain-module-begin 
            (require-for-syntax #,(so->d->so stx #`mzscheme))
            (require #,(so->d->so stx #`frtime/opt/frtime-opt-lang))
            (optimize-module #,equiv-map-stx FORMS ...)))]))
  
  
  ;; Expand a module body until it's just top-level definitions, and then
  ;; separately optimize each kind of top-level definition.
  (define-syntax (optimize-module stx)
    (syntax-case stx ()
      [(_ EQUIV-MAP)
       #`(begin)]

      [(_ EQUIV-MAP FORM FORMS ...)
       (let ([expanded-form 
              (local-expand #'FORM 'module 
                            (list #'begin #'begin0 #'#%provide #'#%require #'#%declare
                                  #'define-syntaxes #'define-values-for-syntax
                                  #'define-values #'#%app #'unit #'unit/sig))])
         (syntax-case expanded-form (begin begin0 #%provide #%require #%declare
                                           define-syntaxes define-values-for-syntax
                                           define-values #%app)
           ;; explode top-level begin statements
           [(begin MORE-FORMS ...)
            #`(optimize-module EQUIV-MAP MORE-FORMS ... FORMS ...)]

           ;; require
           [(#%require . __)
            #`(optimize-require EQUIV-MAP #,expanded-form FORMS ...)]
           
           ;; provide
           [(#%provide . __)
            ;; TBD: provide lowered equivs as well.
            ;; TBD: support frtime-specific provide specs (lifted, etc)
            #`(begin #,expanded-form
                     (optimize-module EQUIV-MAP FORMS ...))]

           ;; declare
           [(#%declare . __)
            #`(begin #,expanded-form
                     (optimize-module EQUIV-MAP FORMS ...))]

           ;; syntax definitions
           [(define-syntaxes . __)
            #`(begin #,expanded-form
                     (optimize-module EQUIV-MAP FORMS ...))]
           [(define-values-for-syntax . __)
            #`(begin #,expanded-form
                     (optimize-module EQUIV-MAP FORMS ...))]
           
           ;; top-level variable definitions
           [(define-values (ID ...) VAL)
            #`(begin
                (optimize-definition EQUIV-MAP #,expanded-form FORMS ...))]
           
           ;; expressions
           [else
            #`(begin
                (optimize-expr EQUIV-MAP #,expanded-form)
                (optimize-module EQUIV-MAP FORMS ...))]))]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Require/Provide handling
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  
  ;; Rewrite require forms so that they import not only regular identifiers,
  ;; but also lowered equivalents, and add those lowered equivs to the equiv map.
  (define-syntax (optimize-require stx)
    
    ;; Take a module and a list of identifiers, and return the list of 
    ;; associated lowered equivalents (for those identifiers that have one).
    (define (module-lowered-equivs mod-stx id-stx-list)
      (map make-lowered-equiv-id 
           (filter (lambda (id-stx)
                     (module-exports-id? mod-stx (make-lowered-equiv-id id-stx)))
                   id-stx-list)))
    
    (syntax-case stx (#%require)
      [(_ EQUIV-MAP (#%require SPEC) FORMS ...)
       ;; In the code below, we must convert syntax objects to datums, and then
       ;; back again.  Otherwise the generated require statement doesn't 
       ;; seem to work properly (it doesn't import the correct identifiers).
       ;; See http://list.cs.brown.edu/pipermail/plt-scheme/2006-July/014163.html
       (syntax-case #'SPEC (only prefix all-except prefix-all-except rename 
                                 lifted lifted:nonstrict as-is as-is:unchecked)
         [(only MOD ID ...)
          ;; Add lowered equivs to the list of ids to import
          (let ([lowered-equivs
                 (module-lowered-equivs #'MOD (syntax->list #'(ID ...)))]
                [new-equiv-map #'EQUIV-MAP])
            #`(begin
                (require #,(so->d->so #'MOD #`(only MOD ID ... . #,lowered-equivs)))
                (optimize-module #,new-equiv-map FORMS ...)))]
      
         [(prefix PFX MOD)
          ;; Requiring the entire module with a prefix will automatically import
          ;; the lowered-equiv bindings with the same prefix.
          (let ([new-equiv-map #'EQUIV-MAP])
            #`(begin
                (require #,(so->d->so #'MOD #`(prefix PFX MOD)))
                (optimize-module #,new-equiv-map FORMS ...)))]
      
         [(all-except MOD ID ...)
          ;; Add the lowered-equiv ids to the exclude list.  If they aren't actually
          ;; provided by the module then they will be silently ignored.
          (let ([lowered-equivs
                 (module-lowered-equivs #'MOD (syntax->list #'(ID ...)))]
                [new-equiv-map #'EQUIV-MAP])
            #`(begin
                (require #,(so->d->so #'MOD #`(all-except MOD ID ... . #,lowered-equivs)))
                (optimize-module #,new-equiv-map FORMS ...)))]
      
         [(prefix-all-except PFX MOD ID ...)
          ;; Add the lowered-equiv bindings to the exclude list.
          (let ([lowered-equivs
                 (module-lowered-equivs #'MOD (syntax->list #'(ID ...)))]
                [new-equiv-map #'EQUIV-MAP])
            #`(begin
                (require #,(so->d->so #'MOD #`(prefix-all-except PFX MOD ID ... . #,lowered-equivs)))
                (optimize-module #,new-equiv-map FORMS ...)))]
      
         [(rename MOD LOCAL-ID EXPORTED-ID)
          ;; Rename the lowered-equiv binding as well.
          (let* ([exported-lowered-equiv-id (make-lowered-equiv-id #'EXPORTED-ID)]
                 [local-lowered-equiv-id (make-lowered-equiv-id #'LOCAL-ID)]
                 [has-lowered-equiv (module-exports-id? #'MOD
                                                        exported-lowered-equiv-id)])
            (if has-lowered-equiv
                #`(begin
                    (require #,(so->d->so #'MOD #`(rename MOD LOCAL-ID EXPORTED-ID)))
                    (require #,(so->d->so #'MOD #`(rename MOD 
                                                          #,local-lowered-equiv-id
                                                          #,exported-lowered-equiv-id)))
                    (optimize-module ((LOCAL-ID #,local-lowered-equiv-id) . EQUIV-MAP) FORMS ...))
                #`(begin
                    (require #,(so->d->so #'MOD #`(rename MOD LOCAL-ID EXPORTED-ID)))
                    (optimize-module EQUIV-MAP FORMS ...))))]
         
         [(LIFTED MOD ID)
          ;; import the identifier itself as the lowered equiv, and define a new, lifted, version.
          (and (identifier? #'LIFTED)
               (or (free-identifier=? #'LIFTED #'lifted)
                   (free-identifier=? #'LIFTED #'lifted:nonstrict)))
          (let* ([lowered-equiv-id (make-lowered-equiv-id #'ID)]
                 [strict? (datum->syntax #'MOD (free-identifier=? #'LIFTED #'lifted))])
            #`(begin
                (require #,(so->d->so #'MOD #`(rename MOD #,lowered-equiv-id ID)))
                (define (ID . args) (apply frtime:lift #,strict? #,lowered-equiv-id args))
                (optimize-require ((ID #,lowered-equiv-id) . EQUIV-MAP) FORMS ...)))]
         [(LIFTED MOD ID IDS ...)
          ;; only import one lifted identifier at a time
          (and (identifier? #'LIFTED)
               (or (free-identifier=? #'LIFTED #'lifted)
                   (free-identifier=? #'LIFTED #'lifted:nonstrict)))
          #`(optimize-require EQUIV-MAP 
                              (require (LIFTED MOD ID)) 
                              (require (LIFTED MOD IDS ...)) 
                              FORMS ...)]
         
         [(AS-IS MOD IDS ...)
          ;; as-is and as-is:unchecked are treated just like (require (only ...))
          (and (identifier? #'AS-IS)
               (or (free-identifier=? #'AS-IS #'as-is)
                   (free-identifier=? #'AS-IS #'as-is:unchecked)))
          #`(optimize-require EQUIV-MAP 
                              (require #,(so->d->so #'MOD #`(only MOD IDS ...)))
                              FORMS ...)]
         
         [MOD
          ;; Requiring an entire module will automatically import the lowered-equiv
          ;; bindings, so we don't need to change the require directive itself, just
          ;; the equiv map.
          ;; Vote: it's very important to use #'SPEC as the ref-stx here.  If you 
          ;; use #'MOD or stx instead, then the identifiers returned by all-provided-ids
          ;; will not be module-identifier=? to the identifiers in the actual code.
          ;; TBD: make the initial import of frtime-opt-lang use this same mechanism.
          (let* ([additional-equiv-map (symbol-list-to-equiv-map 
                                        (all-provided-ids #'MOD #'SPEC))]
                 [new-equiv-map-stx (equiv-map-to-stx
                                     (union-equiv-maps additional-equiv-map
                                                       (stx-to-equiv-map #'EQUIV-MAP)))])
            #`(begin (require MOD)
                     (optimize-module #,new-equiv-map-stx FORMS ...)))]
         )]
      
      [(_ EQUIV-MAP (#%require SPECS ...) FORMS ...)
       ;; Process each require spec individually.
       #`(optimize-require EQUIV-MAP (require SPECS) ... FORMS ...)]
      ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Expression optimization
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

  ;; Helper functions for working with dipped expressions
  (define-for-syntax (dipped? stx)
    (syntax-case stx (dip)
      [(dip . _) #t]
      [ELSE #f]))
  (define-for-syntax (get-dipped-expr stx)
    (syntax-case stx (dip)
      [(dip DEPS EXPR) 
       #'EXPR]))
  (define-for-syntax (get-dipped-deps stx)
    (syntax-case stx (dip)
      [(dip (DEP ...) EXPR) 
       (syntax->list #'(DEP ...))]))
  (define-for-syntax (union-id-lists . id-lists)
    (foldl (lambda (l1 l2)
             (lset-union bound-identifier=? l1 l2))
           '()
           id-lists))
  (define-for-syntax (diff-id-lists id-list1 id-list2)
    (lset-difference bound-identifier=? id-list1 id-list2))
  
  ;; return the list of identifiers in an argument list.
  ;; #'(a b c) => (list #'a #'b #'c)
  ;; #'(x y . rest) => (list #'x #'y #'rest)
  ;; #'args => (list #'args)
  (define-for-syntax (extract-args stx)
    (syntax-case stx ()
      [ID
       (identifier? #'ID)
       (list #'ID)]
      [(ID . REST)
       (append (extract-args #'ID)
               (extract-args #'REST))]
      [()
       '()]
      [ELSE
       (raise-syntax-error #f "doesn't look like an arg list" stx)]))
  
  ;; return the rest arg from an argument list.
  ;; #'(a b . rest) => #'rest
  ;; #'args => #'args
  (define-for-syntax (extract-rest-arg stx)
    (syntax-case stx ()
      [ID
       (identifier? #'ID)
       #'ID]
      [(ID . REST)
       (identifier? #'REST)
       #'REST]
      [(ID . REST)
       (extract-rest-arg #'REST)]
      [ELSE
       (raise-syntax-error #f "cannot extract rest arg" stx)]))

  ;; search and replace an identifier in a syntax object
  (define-for-syntax (replace-id stx old-id new-id)
    (syntax-case stx ()
      [ID
       (and (identifier? #'ID)
            (free-identifier=? old-id #'ID))
       new-id]
      [(X . Y)
       (datum->syntax 
        stx
        (cons (replace-id #'X old-id new-id)
              (replace-id #'Y old-id new-id)))]
      [()
       '()]
      [ELSE
       stx]))

  ;; return #t if a syntax object refers to a given id
  (define-for-syntax (refers-to-id? stx id)
    (syntax-case stx ()
      [ID
       (and (identifier? #'ID)
            (free-identifier=? id #'ID))
       #t]
      [(X . Y)
       (or (refers-to-id? #'X id)
           (refers-to-id? #'Y id))]
      [ELSE #f]))
  
  ;; Optimize an expression by dipping subexpressions wherever possible.
  ;; 
  ;; Note: this function does not intercept syntax errors due to misused
  ;; certificates.  It's up to the caller to handle that.
  (define-for-syntax (recursively-optimize-expr stx equiv-map lower-lambda)
    (syntax-case stx (#%top #%app quote begin begin0 lambda case-lambda 
                      let-values letrec-values letrec-syntaxes+values 
                      unit unit/sig if super-lift undefined? undefined rename
                      frp:copy-list frp:->boolean dont-optimize)
      
      [(#%top . X)
       #`(dip (X) X)]
      
      [X
       (identifier? #'X)
       #`(dip (X) X)]
      
      [(quote EXPR)
       #`(dip () #,stx)]

      [(BEGIN EXPR ...)
       (and (identifier? #'BEGIN)
            (or (free-identifier=? #'BEGIN #'begin)
                (free-identifier=? #'BEGIN #'begin0)))
       (let* ([optimized-exprs (map (lambda (expr)
                                      (recursively-optimize-expr expr equiv-map #f))
                                    (syntax->list #'(EXPR ...)))]
              [all-exprs-were-dipped (every dipped? optimized-exprs)])
         (if all-exprs-were-dipped
             (let ([deps (apply union-id-lists (map get-dipped-deps optimized-exprs))]
                   [lowered-exprs (map get-dipped-expr optimized-exprs)])
               #`(dip #,deps (begin . #,lowered-exprs)))
             ;; we can't dip the entire subexpression, but we may have been able
             ;; to optimize some of the exprs.
             #`(begin . #,optimized-exprs)))]
      
      [(lambda ARGS (let-values (((REST) (#%app frp:copy-list REST_))) EXPRS ...))
       ;; If frtime sees a lambda with a rest arg, it inserts a call to
       ;; frp:copy-list.  Recognize this pattern and ignore it if we're lowering
       ;; instead of dipping.
       (let* ([rest-arg (extract-rest-arg #`ARGS)]
              ;; have to replace REST to avoid "identifier used out of context" errors
              [body (replace-id #`(begin EXPRS ...) #'REST rest-arg)]
              [optimized-body (recursively-optimize-expr body equiv-map #f)])
         (if (and lower-lambda (dipped? optimized-body))
             (let* ([lowered-body (get-dipped-expr optimized-body)]
                    [body-deps (get-dipped-deps optimized-body)]
                    [new-deps (diff-id-lists body-deps (extract-args #`ARGS))])
               #`(dip #,new-deps (lambda ARGS #,lowered-body)))
             (if (refers-to-id? optimized-body rest-arg)
                 #`(lambda ARGS
                     (let-values (((#,rest-arg) (frp:copy-list #,rest-arg)))
                       #,optimized-body))
                 #`(lambda ARGS #,optimized-body))))]
      
      [(lambda ARGS EXPRS ...)
       ;; If "lower-lambda" is true, then we know that it's safe to lower the
       ;; body of the lambda.  "lower-lambda" should only be true if we know 
       ;; that the result will be immediately bound to a variable that is
       ;; guaranteed never to be called except in a lowered context.  In contrast,
       ;; if we always lowered the body of a lambda, the closure might escape
       ;; to where it could be called with time-varying arguments.
       (let* ([body #`(begin EXPRS ...)]
              [optimized-body (recursively-optimize-expr body equiv-map #f)])
         (if (and lower-lambda (dipped? optimized-body))
             (let* ([lowered-body (get-dipped-expr optimized-body)]
                    [body-deps (get-dipped-deps optimized-body)]
                    [new-deps (diff-id-lists body-deps (extract-args #'ARGS))])
               #`(dip #,new-deps (lambda ARGS #,lowered-body)))
             #`(lambda ARGS #,optimized-body)))]

      [(case-lambda (FORMALS EXPR ...) ...)
       ;; dip each clause separately.  TODO: pay attention to lower-lambda
       (let* ([bodies (syntax->list #`((begin EXPR ...) ...))]
              [optimized-bodies (map (lambda (expr)
                                       (recursively-optimize-expr expr equiv-map #f))
                                     bodies)]
              [args (syntax->list #`(FORMALS ...))]
              [clauses (map list args optimized-bodies)])
         #`(case-lambda . #,clauses))]

      ;; special case: recognize the expanded version of "let loop" and allow
      ;; it to be fully lowered.  Otherwise it ends up calling itself, and since
      ;; we don't define a fully lowered equivalent of the loop itself, then
      ;; whole thing ends up using signal:switching, which is slow.
      ;; TBD: "let loop" embeds its letrec-values in an #%app -- we could 
      ;; recognize this and avoid defining the upper version at all.
      [(#%app (letrec-values (((LOOP) BODY))
                LOOP_) ARG ...)
       (and (identifier? #'LOOP)
            (identifier? #'LOOP_)
            (free-identifier=? #'LOOP #'LOOP_))
       (let* ([optimized-args (map (lambda (e)
                                     (recursively-optimize-expr e equiv-map #f))
                                   (syntax->list #'(ARG ...)))]
              [loop-lowered-id (make-lowered-equiv-id #'LOOP)]
              [extended-equiv-map (add-equiv-map equiv-map #'LOOP loop-lowered-id)]
              ;; allow lambdas to be lowered, because "let loop" generates a lambda
              [lowered-body (recursively-optimize-expr #'BODY extended-equiv-map #t)])
         ;; if the body can be fully lowered, and the lower version doesn't refer to
         ;; the upper version, then we can define both an upper and a lower definition, 
         ;; and return the upper one. 
         (if (dipped? lowered-body)
             (if (every dipped? optimized-args)
                 ;; all the args were dippable -- we can lower the *entire* thing, and
                 ;; do away with the upper version of the loop.
                 (let* ([lowered-expr (get-dipped-expr lowered-body)]
                        [lowered-deps (get-dipped-deps lowered-body)]
                        [deps (apply union-id-lists lowered-deps 
                                     (map get-dipped-deps optimized-args))])
                   #`(dip #,deps 
                          (#%app (letrec-values (((#,loop-lowered-id) #,lowered-expr))
                                   #,loop-lowered-id)
                                 #,@(map get-dipped-expr optimized-args))))
                 ;; at least one arg wasn't dippable, so we have to keep the upper version,
                 ;; and we can't dip the entire expression.
                 (let* ([optimized-body (recursively-optimize-expr #'BODY extended-equiv-map #f)]
                        [lowered-expr (get-dipped-expr lowered-body)]
                        [lowered-deps (get-dipped-deps lowered-body)]
                        [deps (diff-id-lists lowered-deps (list #'LOOP))])
                   #`(#%app (dip #,deps 
                                 (letrec-values (((#,loop-lowered-id) #,lowered-expr)
                                                 ((LOOP) #,optimized-body))
                                   LOOP))
                            #,@optimized-args)))
             (let* ([optimized-body (recursively-optimize-expr #'BODY equiv-map #f)])
               #`(#%app (letrec-values (((LOOP) #,optimized-body)) 
                          LOOP)
                        #,@optimized-args))))]
       
      [(LET-VALUES ((VARS VALS) ...) EXPR ...)
       (and (identifier? #'LET-VALUES)
            (or (free-identifier=? #'LET-VALUES #'let-values)
                (free-identifier=? #'LET-VALUES #'letrec-values)))
       (let* ([bindings (syntax->list #'(VARS ...))]
              [flattened-bindings (apply append (map syntax->list bindings))]
              [body #`(begin EXPR ...)]
              [optimized-body (recursively-optimize-expr body equiv-map lower-lambda)]
              ;; TBD: consider defining lowered equivs for local bindings
              [optimized-vals (map (lambda (arg)
                                     (recursively-optimize-expr arg equiv-map #f))
                                   (syntax->list #'(VALS ...)))]
              [all-exprs-were-dipped (and (dipped? optimized-body)
                                          (every dipped? optimized-vals))])
         (if all-exprs-were-dipped
             (let* ([val-deps (apply append (map get-dipped-deps optimized-vals))]
                    [new-deps (union-id-lists
                               (diff-id-lists (get-dipped-deps optimized-body) 
                                              flattened-bindings)
                               val-deps)]
                    [lowered-body (get-dipped-expr optimized-body)]
                    [lowered-vals (map get-dipped-expr optimized-vals)])
               #`(dip #,new-deps
                      (LET-VALUES #,(map list bindings lowered-vals)
                                  #,lowered-body)))
             #`(LET-VALUES #,(map list bindings optimized-vals)
                           #,optimized-body)))]

      [(letrec-syntaxes+values SYNTAX-STUFF ((IDS VALS) ...) EXPR ...)
       (let* ([optimized-vals
               (map (lambda (e)
                      (recursively-optimize-expr e equiv-map #f))
                    (syntax->list #'(VALS ...)))]
              [optimized-bindings
               (map list (syntax->list #'(IDS ...)) optimized-vals)]
              [body #`(begin EXPR ...)]
              [optimized-body (recursively-optimize-expr body equiv-map #f)])
         #`(letrec-syntaxes+values SYNTAX-STUFF #,optimized-bindings #,optimized-body))]
      
      [(if . ARGS)
       (let* ([optimized-args (map (lambda (expr)
                                     (recursively-optimize-expr expr equiv-map #f))
                                   (syntax->list #'ARGS))]
              [all-args-were-dipped (every dipped? optimized-args)])
         (if all-args-were-dipped
             ;; we can dip the entire subexpression
             (let ([deps (apply union-id-lists (map get-dipped-deps optimized-args))]
                   [lowered-args (map get-dipped-expr optimized-args)])
               #`(dip #,deps (if . #,lowered-args)))
             ;; we can't dip the entire subexpression, but we may have been able
             ;; to optimize some of the args.
             #`(if . #,optimized-args)))]
      
      ;; frtime's if expands into a complicated expression involving super-lift.  
      ;; recognize this pattern, and treat it like an if statement.
      [(#%app super-lift
              (lambda (_B) (if (#%app undefined? __B)
                               (begin undefined)
                               (if ___B
                                   TRUE-CASE
                                   FALSE-CASE)))
              (#%app _TO_BOOLEAN CONDITIONAL))
       (let* ([optimized-condition  (recursively-optimize-expr #'CONDITIONAL equiv-map #f)]
              [optimized-true-case  (recursively-optimize-expr #'TRUE-CASE equiv-map #f)]
              [optimized-false-case (recursively-optimize-expr #'FALSE-CASE equiv-map #f)])
         (if (and (dipped? optimized-condition)
                  (dipped? optimized-true-case)
                  (dipped? optimized-false-case))
             (let ([lowered-condition  (get-dipped-expr optimized-condition)]
                   [lowered-true-case  (get-dipped-expr optimized-true-case)]
                   [lowered-false-case (get-dipped-expr optimized-false-case)]
                   [deps (union-id-lists (get-dipped-deps optimized-condition)
                                         (get-dipped-deps optimized-true-case)
                                         (get-dipped-deps optimized-false-case))])
               #`(dip #,deps (if #,lowered-condition #,lowered-true-case #,lowered-false-case)))
             #`(frtime:if #,optimized-condition
                          #,optimized-true-case
                          #,optimized-false-case)))]
      
      [(#%app dont-optimize EXPR)
       #'EXPR]
      
      [(#%app FUNC . ARGS)
       (identifier? #'FUNC)
       (let* ([lowered-equiv (lookup-lowered-equiv equiv-map #'FUNC)]
              [optimized-args (map (lambda (expr)
                                     (recursively-optimize-expr expr equiv-map #f))
                                   (syntax->list #'ARGS))]
              [all-args-were-dipped (every dipped? optimized-args)])
         (if (and lowered-equiv all-args-were-dipped)
             ;; we can dip the entire subexpression
             (let ([deps (apply union-id-lists (map get-dipped-deps optimized-args))]
                   [lowered-args (map get-dipped-expr optimized-args)])
               #`(dip #,deps (#%app #,lowered-equiv #,@lowered-args)))
             ;; we can't dip the entire subexpression, but we may have been able
             ;; to optimize some of the args.
             #`(#%app FUNC #,@optimized-args)))]
      
      ;; first-class functions -- we have no idea what function is being called,
      ;; so we can't optimize it.  But try to optimize the subexpressions.
      [(#%app FUNC ARG ...)
       #`(#%app #,(recursively-optimize-expr #'FUNC equiv-map #f)
                #,@(map (lambda (expr)
                          (recursively-optimize-expr expr equiv-map #f))
                        (syntax->list #'(ARG ...))))]
       
      ;; units expand into complicated code that uses set! to define identifiers.
      ;; Since we don't support set!, we optimize units based on their *unexpanded*
      ;; syntax, which is much easier to work with.
      [(unit IMPORTS EXPORTS EXPRS ...)
       #`(unit IMPORTS EXPORTS 
               (optimize-module #,(equiv-map-to-stx equiv-map) EXPRS ...))]
      [(unit/sig SIG IMPORTS (rename RENAMES ...) EXPRS ...)
       #`(unit/sig SIG IMPORTS (rename RENAMES ...)
           (optimize-module #,(equiv-map-to-stx equiv-map) EXPRS ...))]
      [(unit/sig SIG IMPORTS EXPRS ...)
       #`(unit/sig SIG IMPORTS 
           (optimize-module #,(equiv-map-to-stx equiv-map) EXPRS ...))]

      [ELSE
       (raise-syntax-error #f
                           (format "recursively-lower-expr: unrecognized syntax: ~a" (syntax->datum stx))
                           stx)]))
  
  ;; Optimize a single expression.  Raises exn:fail:syntax if the optimized version
  ;; would use a protected identifier in an uncertified context.
  (define-for-syntax (optimize-expr-helper stx equiv-map allow-lambda)
    ;; Expand everything but references to identifiers, and units.
    ;; References to identifiers can't be expanded because they might not be 
    ;; defined yet, in which case we'll get a syntax error.  Units can't be 
    ;; expanded because they expand into highly indirect code that uses set! 
    ;; to define structures, and we don't support set! -- so instead we handle 
    ;; units explicitly.
    (let* ([expanded-stx (local-expand stx 'top-level (list #'#%top #'unit #'unit/sig))]
           [optimized-stx (recursively-optimize-expr expanded-stx equiv-map allow-lambda)])
      ;; expand the result so that if we happen to have a reference to a
      ;; protected identifier in an uncertified context, then we can 
      ;; trigger an exception, thus giving us the chance to rollback our
      ;; changes and return the original code unmodified.
      (local-expand optimized-stx 'top-level (list #'#%top)) ;; expand units, too
      ;; return the unexpanded result, so that callers can figure out whether
      ;; the expression was completely dipped or not.
      optimized-stx))

  ;; Macro to optimize a single expression.  The expression will be dipped
  ;; wherever possible, and its observable semantics will remain unchanged.
  (define-syntax (optimize-expr stx)
    (syntax-case stx ()
      [(_ EQUIV-MAP EXPR)
       (with-handlers ([exn:fail:syntax?
                            (lambda (exn)
                              #`(begin #,(exn-message exn) EXPR))])
         (let* ([equiv-map (stx-to-equiv-map #'EQUIV-MAP)])
            (optimize-expr-helper #'EXPR equiv-map #f)))]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Definitions
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; returns multiple values:
  ;;   * new equiv map
  ;;   * optimized value
  ;;   * lowered equiv id (or #f if unlowerable)
  ;;   * lowered equiv value (or #f if unlowerable)
  (define-for-syntax (optimize-definition-helper equiv-map id val)
    (with-handlers 
        ([exn:fail:syntax? (lambda (exn) 
                             (values equiv-map
                                     #`(begin #,(exn-message exn) #,val)
                                     #f
                                    #f))])
      (let* ([lowered-equiv-id (make-lowered-equiv-id id)]
             [new-equiv-map (add-equiv-map equiv-map id lowered-equiv-id)]
             [lowered-val   (optimize-expr-helper val new-equiv-map #t)])
        (if (and (dipped? lowered-val)
                 (null? (get-dipped-deps lowered-val)))
            ;; use new-equiv-map in order to allow the dipped version to call 
            ;; the lowered version for recursive calls.
            (let ([optimized-val (optimize-expr-helper val new-equiv-map #f)])
              (values new-equiv-map 
                      optimized-val
                      lowered-equiv-id
                      (get-dipped-expr lowered-val)))
            ;; use the old equiv-map, since there is no lowered version so the
            ;; dipped version can't call it
            (let ([optimized-val (optimize-expr-helper val equiv-map #f)])
              (values equiv-map
                      optimized-val
                      #f
                      #f))))))
  
  ;; Optimize a top-level variable binding, by lowering its value.
  (define-syntax (optimize-definition stx)
    (syntax-case stx (define-values values)
      [(_ EQUIV-MAP (define-values (ID) VAL) FORMS ...)
       ;; if this is a lowered equiv definition, add it to our equiv map
       (lowered-equiv-id? #'ID)
       (let ([lifted-id (lowered-equiv-id->lifted-id #'ID)])
         #`(begin
             (define-values (ID) VAL)
             (optimize-module ((#,lifted-id ID) . EQUIV-MAP) FORMS ...)))]

      [(_ EQUIV-MAP (define-values (ID) VAL) FORMS ...)
       ;; We're defining a single identifier
       (let-values ([(new-equiv-map optimized-val lowered-equiv-id lowered-val)
                     (optimize-definition-helper (stx-to-equiv-map #'EQUIV-MAP)
                                                 #'ID 
                                                 #'VAL)])
         (if (and lowered-equiv-id lowered-val)
             #`(begin
                 (define ID #,optimized-val)
                 (define #,lowered-equiv-id #,lowered-val)
                 (optimize-module #,(equiv-map-to-stx new-equiv-map) FORMS ...))
             #`(begin
                 (define ID #,optimized-val)
                 (optimize-module #,(equiv-map-to-stx new-equiv-map) FORMS ...))))]
      
      [(_ EQUIV-MAP (define-values (ID ...) (values VAL ...)) FORMS ...)
       (= (length (syntax->list #'(ID ...)))
          (length (syntax->list #'(VAL ...))))
       ;; Very common special case: the multiple identifiers are immediately combined
       ;; with multiple values.  Common enough that it's worth handling specially.
       (let ([new-equiv-map (stx-to-equiv-map #'EQUIV-MAP)]
             [ids null]
             [vals null])
         (for-each (lambda (id val)
                     (let-values ([(em optimized-val lowered-equiv-id lowered-val)
                                   (optimize-definition-helper new-equiv-map id val)])
                       (set! new-equiv-map em)
                       (when (and lowered-equiv-id lowered-val)
                             (set! ids (cons lowered-equiv-id ids))
                             (set! vals (cons lowered-val vals)))
                       (set! ids (cons id ids))
                       (set! vals (cons optimized-val vals))))
                   (syntax->list #'(ID ...)) 
                   (syntax->list #'(VAL ...)))
         #`(begin
             (define-values #,ids (values . #,vals))
             (optimize-module #,(equiv-map-to-stx new-equiv-map) FORMS ...)))]
       
      [(_ EQUIV-MAP (define-values (ID ...) EXPR) FORMS ...)
       ;; We're binding multiple identifiers with some arbitrary set of values,
       ;; so we can't define lowered-equivs because we can't tease apart the
       ;; various values.  So just optimize the value expression as a whole.
       #`(begin
           (define-values (ID ...) (optimize-expr EQUIV-MAP EXPR))
           (optimize-module EQUIV-MAP FORMS ...))]))
)
