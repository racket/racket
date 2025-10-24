#lang racket/base
(require "../common/promise.rkt"
         "../common/struct-star.rkt"
         "../common/performance.rkt"
         "../common/parameter-like.rkt"
         "../syntax/syntax.rkt"
         "../syntax/debug.rkt"
         "../syntax/property.rkt"
         "../syntax/scope.rkt"
         "../syntax/match.rkt"
         "../syntax/track.rkt"
         "../common/phase.rkt"
         "../syntax/track.rkt"
         "../syntax/error.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../syntax/binding.rkt"
         "../eval/protect.rkt"
         "dup-check.rkt"
         "free-id-set.rkt"
         "stop-ids.rkt"
         "require+provide.rkt"
         "../common/module-path.rkt"
         "lift-context.rkt"
         "lift-key.rkt"
         "../namespace/core.rkt"
         "context.rkt"
         "use-site.rkt"
         "main.rkt"
         "require.rkt"
         "provide.rkt"
         "def-id.rkt"
         "prepare.rkt"
         "log.rkt"
         "syntax-id-error.rkt"
         "portal-syntax.rkt"
         "../compile/main.rkt"
         "../compile/linklet.rkt"
         "../eval/top.rkt"
         "../eval/module.rkt"
         "cross-phase.rkt"
         "parsed.rkt"
         "expanded+parsed.rkt"
         "append.rkt"
         "save-and-restore.rkt"
         "module-prompt.rkt"
         "configure.rkt")

(add-core-form!
 'module
 (lambda (s ctx)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (log-expand ctx 'prim-module #f)
     (raise-syntax-error #f "allowed only at the top level or in a module top-level" s))
   (performance-region
    ['expand 'module]
    (expand-module s ctx #f))))

(add-core-form!
 'module*
 (lambda (s ctx)
   (log-expand ctx 'prim-module #f)
   (raise-syntax-error #f "illegal use (not in a module top-level)" s)))

(add-core-form!
 '#%module-begin
 (lambda (s ctx)
   (log-expand ctx 'prim-module-begin #f)
   (unless (eq? (expand-context-context ctx) 'module-begin)
     (raise-syntax-error #f "not in a module-definition context" s))
   (unless (expand-context-module-begin-k ctx)
     (raise-syntax-error #f "not currently transforming a module" s))
   ;; This `#%module-begin` must be in a `module`; the
   ;; `module-begin-k` function continues that module's
   ;; expansion
   ((expand-context-module-begin-k ctx)
    s
    (struct*-copy expand-context ctx
                  [module-begin-k #f]))))

(add-core-form!
 '#%declare
 (lambda (s ctx)
   (log-expand ctx 'prim-declare #f)
   ;; The `#%module-begin` expander handles `#%declare`
   (raise-syntax-error #f "not allowed outside of a module top-level" s)))

;; ----------------------------------------

(define (expand-module s init-ctx enclosing-self
                       #:always-produce-compiled? [always-produce-compiled? #f]
                       #:keep-enclosing-scope-at-phase [keep-enclosing-scope-at-phase #f]
                       #:enclosing-is-cross-phase-persistent? [enclosing-is-cross-phase-persistent? #f]
                       #:enclosing-requires+provides [enclosing-r+p #f]
                       #:mpis-for-enclosing-reset [mpis-for-enclosing-reset #f]
                       ;; For cross-linklet inlining among submodules compiled together:
                       #:modules-being-compiled [modules-being-compiled (make-hasheq)])
   (log-expand init-ctx 'prim-module s)
   (define-match m s '(module id:module-name initial-require body ...))

   (define rebuild-s (keep-as-needed init-ctx s #:keep-for-parsed? #t #:keep-for-error? #t))

   (define initial-require (syntax->datum (m 'initial-require)))
   (unless (or keep-enclosing-scope-at-phase
               (module-path? initial-require))
     (raise-syntax-error #f "not a module path" s (m 'initial-require)))

   ;; All module bodies start at phase 0
   (define phase 0)
   
   (define module-name-sym (syntax-e (m 'id:module-name)))
   
   (define outside-scope (new-scope 'module))
   (define inside-scope (new-multi-scope module-name-sym))

   (define self (make-self-module-path-index (if enclosing-self
                                                 module-name-sym
                                                 (string->uninterned-symbol
                                                  (symbol->string module-name-sym)))
                                             enclosing-self))
   
   (define enclosing-mod (and enclosing-self
                              (module-path-index-join '(submod "..") self)))
   (when (and #;enclosing-mod mpis-for-enclosing-reset)
     (set-box! mpis-for-enclosing-reset
               (cons enclosing-mod (unbox mpis-for-enclosing-reset))))
   
   (define apply-module-scopes
     (make-apply-module-scopes outside-scope inside-scope 
                               init-ctx keep-enclosing-scope-at-phase
                               self enclosing-self enclosing-mod))

   ;; Initial require name provides the module's base scopes
   (define initial-require-s (apply-module-scopes (m 'initial-require)))
   (define all-scopes-s initial-require-s)

   (define root-ctx (make-root-expand-context
                     #:self-mpi self
                     #:initial-scopes (if keep-enclosing-scope-at-phase
                                          (root-expand-context-module-scopes init-ctx)
                                          null)
                     #:outside-scope outside-scope
                     #:post-expansion-scope inside-scope
                     #:all-scopes-stx all-scopes-s))
   
   ;; Extract combined scopes
   (define new-module-scopes (root-expand-context-module-scopes root-ctx))

   ;; A frame-id is used to determine when use-site scopes are needed
   (define frame-id (root-expand-context-frame-id root-ctx))

   ;; Record portal syntax to be extracted without running the module
   (define portal-syntaxes (make-hasheqv))

   ;; Make a namespace for module expansion
   (define (make-m-ns ns #:for-submodule? [for-submodule? (and enclosing-self #t)])
     (make-module-namespace ns
                            #:mpi self
                            #:root-expand-context root-ctx
                            #:for-submodule? for-submodule?
                            #:portal-syntaxes portal-syntaxes))
   (define m-ns (make-m-ns (expand-context-namespace init-ctx)))
   
   ;; Initial context for all body expansions:
   (define ctx (struct*-copy expand-context (copy-root-expand-context init-ctx root-ctx)
                             [allow-unbound? #f]
                             [namespace m-ns]
                             [post-expansion #:parent root-expand-context (lambda (s) (add-scope s inside-scope))]
                             [phase phase]
                             [just-once? #f]))
   
   ;; Add the module's scope to the body forms; use `s` and
   ;; re-match to extract the body forms, because that improves sharing
   (define bodys (let ([scoped-s (apply-module-scopes s)])
                   (define-match m scoped-s '(_ _ _ body ...))
                   (m 'body)))
   
   ;; To keep track of all requires and provides
   (define requires+provides (make-requires+provides self #:portal-syntaxes portal-syntaxes))

   ;; Table of symbols picked for each binding in this module:
   (define defined-syms (root-expand-context-defined-syms root-ctx)) ; phase -> sym -> id

   ;; So that compilations of submodules can be preserved for
   ;; inclusion in an overall compiled module:
   (define compiled-submodules (make-hasheq))

   ;; If we compile the module for use by `module*` submodules, keep that
   ;; compiled form to possibly avoid compiling again.
   (define compiled-module-box (box #f))
   
   ;; Accumulate module path indexes used by submodules to refer to this module
   (define mpis-to-reset (box null))

   ;; For recording `portal` via `#%require`:
   (define add-defined-portal (make-add-defined-portal self requires+provides
                                                       portal-syntaxes defined-syms
                                                       all-scopes-s frame-id m-ns))

   ;; Initial require
   (define (initial-require! #:bind? bind?)
     (cond
      [(not keep-enclosing-scope-at-phase)
       ;; Install the initial require
       (define initial-mpi (build-initial-require-mpi initial-require self))
       (perform-initial-require! initial-mpi self
                                 all-scopes-s
                                 m-ns
                                 requires+provides
                                 #:bind? bind?
                                 #:who 'module)
       initial-mpi]
      [else
       ;; For `(module* name #f ....)`, just register the enclosing module
       ;; as an import and visit it
       (add-required-module! requires+provides
                             enclosing-mod
                             keep-enclosing-scope-at-phase
                             enclosing-is-cross-phase-persistent?)
       (add-enclosing-module-defined-and-required! requires+provides
                                                   #:enclosing-requires+provides enclosing-r+p
                                                   enclosing-mod
                                                   keep-enclosing-scope-at-phase)
       (namespace-module-visit! m-ns enclosing-mod
                                keep-enclosing-scope-at-phase)
       #f]))
   (log-expand init-ctx 'prepare-env)
   (define initial-mpi (initial-require! #:bind? #t))
   (log-expand init-ctx 'rename-one bodys)

   (define-values (paramz exit-paramz)
     (cond
       [initial-mpi
        (define-values (enter exit)
          (load-configure-expand initial-mpi (namespace-root-namespace m-ns)))
        (values (enter-configure-parameterization enter) exit)]
       [else
        (values (current-parameterization)
                current-parameterization)]))

   ;; To detect whether the body is expanded multiple times:
   (define again? #f)

   ;; The primitive `#%module-body` form calls this function to expand the
   ;; current module's body
   (define (module-begin-k mb-s mb-init-ctx)
     ;; In case the module body is expanded multiple times, we clear
     ;; the requires, provides and definitions information each time.
     ;; Don't discard accumulated requires, though, since those may be
     ;; needed by pieces from a previous expansion. Also, be careful
     ;; not to change the current bindings when re-establishing the
     ;; requires.
     (when again?
       (requires+provides-reset! requires+provides)
       (initial-require! #:bind? #f)
       (hash-clear! compiled-submodules)
       (set-box! compiled-module-box #f))
     (set! again? #t)

     ;; In case a nested `#%module-begin` expansion is forced, save
     ;; and restore the module-expansion state:
     (define ctx (struct*-copy expand-context mb-init-ctx
                               [module-begin-k 
                                (lambda (s ctx)
                                  (define new-requires+provides
                                    ;; Copy old `require` dependencies, which allows a
                                    ;; synthesized nested `#%module-begin` to use pieces
                                    ;; that depend on bindings introduced outside the
                                    ;; synthesized part --- a questionable practice,
                                    ;; but support for backward compatibility, at least.
                                    (make-requires+provides self
                                                            #:copy-requires requires+provides))
                                  (with-save-and-restore ([requires+provides new-requires+provides]
                                                          [compiled-submodules (make-hasheq)]
                                                          [compiled-module-box (box #f)]
                                                          [defined-syms (make-hasheq)])
                                    (module-begin-k s ctx)))]
                               ;; Also, force `post-expansion` to be right, in case 'module-begin
                               ;; module is triggered within some other mode; a correct value
                               ;; for `post-expansion` is important to getting phase-specific
                               ;; binding right.
                               [post-expansion #:parent root-expand-context
                                               (lambda (s) (add-scope s inside-scope))]))

     ;; In case `#%module-begin` expansion is forced on syntax that
     ;; that wasn't already introduced into the mdoule's inside scope,
     ;; add it to all the given body forms
     (define added-s (add-scope mb-s inside-scope))

     (define-match mb-m added-s '(#%module-begin body ...))
     (define bodys (mb-m 'body))
     (log-expand ctx 'rename-one added-s)
     
     (define rebuild-mb-s (keep-as-needed ctx mb-s))
     
     ;; For variable repeferences before corresponding binding (phase >= 1)
     (define need-eventually-defined (make-hasheqv)) ; phase -> list of id
     
     ;; For `syntax-local-lift-module-end-declaration`, which is accumulated
     ;; across phases:
     (define module-ends (make-shared-module-ends))
     
     ;; Accumulate `#%declare` content
     (define declared-keywords (make-hasheq))
     
     ;; Accumulated declared submodule names for `syntax-local-submodules`
     (define declared-submodule-names (make-hasheq))

     ;; Requires that were lifted during `#%module-begin` expansion:
     (define initial-lifted-requires (get-require-lifts (expand-context-require-lifts ctx)))
     
     ;; Module expansion always parses the module body along the way,
     ;; even if `to-parsed?` in `ctx` is not true. The body is parsed
     ;; so that the module can be declared for reference by
     ;; submodules. So, if expansion is supposed to a syntax object
     ;; instead of `module-parsed`, then we'll need to accumulate both
     ;; parsed and expanded results; see "expanded+parsed.rkt".
     
     ;; The expansion of the module body happens in 4 passes:
     ;;  Pass 1: Partial expansion to determine imports and definitions
     ;;  Pass 2: Complete expansion of remaining expressions
     ;;  Pass 3: Parsing of provide forms
     ;;  Pass 4: Parsing of `module*` submodules
     
     ;; Passes 1 and 2 are nested via `begin-for-syntax`:
     (define expression-expanded-bodys
       (let pass-1-and-2-loop ([bodys bodys] [phase phase] [keep-stops? (stop-at-module*? ctx)]
                                             [initial-lifted-requires initial-lifted-requires])

         ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         ;; Pass 1: partially expand to discover all bindings and install all 
         ;; defined macro transformers
         
         ;; Need to accumulate definition contexts created during
         ;; partial expansion:
         (define def-ctx-scopes (box null))
         
         (define partial-body-ctx (struct*-copy expand-context ctx
                                                [context 'module]
                                                [phase phase]                                                
                                                [namespace (namespace->namespace-at-phase m-ns phase)]
                                                [stops (free-id-set phase (module-expand-stop-ids phase))]
                                                [def-ctx-scopes def-ctx-scopes]
                                                [need-eventually-defined need-eventually-defined] ; used only at phase 1 and up
                                                [declared-submodule-names declared-submodule-names]
                                                [lift-key #:parent root-expand-context (generate-lift-key)]
                                                [lifts (make-lift-context
                                                        (make-wrap-as-definition self frame-id
                                                                                 inside-scope all-scopes-s
                                                                                 defined-syms requires+provides))]
                                                [module-lifts (make-module-lift-context phase #t)]
                                                [require-lifts (make-require-lift-context
                                                                phase
                                                                (make-parse-lifted-require m-ns self requires+provides
                                                                                           #:declared-submodule-names declared-submodule-names
                                                                                           #:add-defined-portal add-defined-portal)
                                                                initial-lifted-requires)]
                                                [to-module-lifts (make-to-module-lift-context
                                                                  phase
                                                                  #:shared-module-ends module-ends
                                                                  #:end-as-expressions? #f)]))
         
         ;; Result is mostly a list of S-expressions, but can also
         ;; contain `compile-form` or `expanded+parsed` structures:
         (define partially-expanded-bodys
           (partially-expand-bodys bodys
                                   #:phase phase
                                   #:ctx partial-body-ctx
                                   #:namespace m-ns
                                   #:self self
                                   #:frame-id frame-id
                                   #:requires-and-provides requires+provides
                                   #:need-eventually-defined need-eventually-defined
                                   #:all-scopes-stx all-scopes-s
                                   #:defined-syms defined-syms
                                   #:declared-keywords declared-keywords
                                   #:declared-submodule-names declared-submodule-names
                                   #:compiled-submodules compiled-submodules
                                   #:modules-being-compiled modules-being-compiled
                                   #:mpis-to-reset mpis-to-reset
                                   #:portal-syntaxes portal-syntaxes
                                   #:add-defined-portal add-defined-portal
                                   #:loop pass-1-and-2-loop))

         ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
         ;; Pass 2: finish expanding expressions
         
         (log-expand partial-body-ctx 'next-group)
         
         (define body-ctx (struct*-copy expand-context (accumulate-def-ctx-scopes partial-body-ctx def-ctx-scopes)
                                        [stops (if keep-stops?
                                                   (expand-context-stops ctx)
                                                   empty-free-id-set)]
                                        [def-ctx-scopes #f]
                                        [post-expansion #:parent root-expand-context #f]
                                        [to-module-lifts (make-to-module-lift-context phase
                                                                                      #:shared-module-ends module-ends
                                                                                      #:end-as-expressions? #t)]))

         (finish-expanding-body-expressions partially-expanded-bodys
                                            #:phase phase
                                            #:ctx body-ctx
                                            #:self self
                                            #:declared-submodule-names declared-submodule-names
                                            #:compiled-submodules compiled-submodules
                                            #:modules-being-compiled modules-being-compiled
                                            #:mpis-to-reset mpis-to-reset)))

     (log-expand ctx 'next-group)

     ;; Check that any tentatively allowed reference at phase >= 1 is ok
     (check-defined-by-now need-eventually-defined self ctx requires+provides)
     
     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Pass 3: resolve provides at all phases
     
     (log-expand ctx 'next-group)
     
     (define fully-expanded-bodys-except-post-submodules
       (resolve-provides expression-expanded-bodys
                         #:requires-and-provides requires+provides
                         #:declared-submodule-names declared-submodule-names
                         #:namespace m-ns
                         #:phase phase
                         #:self self
                         #:ctx ctx))

     ;; Validate any cross-phase persistence request
     (define is-cross-phase-persistent? (hash-ref declared-keywords '#:cross-phase-persistent #f))
     (when is-cross-phase-persistent?
       (unless (requires+provides-can-cross-phase-persistent? requires+provides)
         (raise-syntax-error #f "cannot be cross-phase persistent because some required module isn't"
                             rebuild-s
                             (hash-ref declared-keywords '#:cross-phase-persistent)
                             null
                             (format "~n  required module: ~a"
                                     (requires+provides-why-not-cross-phase-persistent requires+provides))))
       (check-cross-phase-persistent-form fully-expanded-bodys-except-post-submodules self))

     (define realm (let ([realm-stx (hash-ref declared-keywords '#:realm #f)])
                     (if realm-stx
                         (syntax-e realm-stx)
                         (current-compile-realm))))

     (define flatten-requires? (hash-ref declared-keywords '#:flatten-requires #f))
     
     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Pass 4: expand `module*` submodules
     
     (log-expand ctx 'next-group)
     
     ;; Create a new namespace to avoid retaining the instance that
     ;; was needed to expand this module body:
     (define submod-m-ns (make-m-ns m-ns #:for-submodule? #t))
     
     (define submod-ctx (struct*-copy expand-context ctx
                                      [frame-id #:parent root-expand-context #f]
                                      [post-expansion #:parent root-expand-context #f]
                                      [namespace submod-m-ns]))
     
     (define declare-enclosing-module
       ;; Ensure this module on demand for `module*` submodules that might use it
       (delay (declare-module-for-expansion fully-expanded-bodys-except-post-submodules
                                            #:module-name-id (m 'id:module-name)
                                            #:rebuild-s rebuild-s
                                            #:requires-and-provides requires+provides
                                            #:namespace submod-m-ns
                                            #:self self
                                            #:enclosing enclosing-self
                                            #:root-ctx root-ctx
                                            #:ctx submod-ctx
                                            #:modules-being-compiled modules-being-compiled
                                            #:flatten-requires? flatten-requires?
                                            #:realm realm
                                            #:portal-syntaxes portal-syntaxes
                                            #:fill compiled-module-box)))
     
     (define fully-expanded-bodys
       (cond
        [(stop-at-module*? submod-ctx)
         fully-expanded-bodys-except-post-submodules]
        [else
         (expand-post-submodules fully-expanded-bodys-except-post-submodules
                                 #:declare-enclosing declare-enclosing-module
                                 #:phase phase
                                 #:self self
                                 #:requires-and-provides requires+provides
                                 #:enclosing-is-cross-phase-persistent? is-cross-phase-persistent?
                                 #:all-scopes-s all-scopes-s
                                 #:mpis-to-reset mpis-to-reset
                                 #:declared-submodule-names declared-submodule-names
                                 #:compiled-submodules compiled-submodules
                                 #:modules-being-compiled modules-being-compiled
                                 #:ctx submod-ctx)]))
     
     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Finish
     
     ;; Assemble the `#%module-begin` result:
     (cond
      [(expand-context-to-parsed? submod-ctx)
       (parsed-#%module-begin rebuild-mb-s (parsed-only fully-expanded-bodys) realm flatten-requires?)]
      [else
       (define mb-result-s
         (rebuild
          rebuild-mb-s
          `(,(mb-m '#%module-begin) ,@(syntax-only fully-expanded-bodys))))
       (cond
        [(not (expand-context-in-local-expand? submod-ctx))
         (expanded+parsed mb-result-s
                          (parsed-#%module-begin rebuild-mb-s (parsed-only fully-expanded-bodys) realm flatten-requires?))]
        [else mb-result-s])]))

   ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   ;; Actually expand the `#%module-body` form
   
   ;; The preceding function performs the expansion; here's where we
   ;; trigger it
   
   (define mb-ctx
     (struct*-copy expand-context ctx
                   [context 'module-begin]
                   [module-begin-k module-begin-k]
                   [in-local-expand? #f]
                   [lifts #f]
                   [module-lifts #f]
                   [to-module-lifts #f]
                   [require-lifts (make-require-lift-context
                                   phase
                                   (make-parse-lifted-require m-ns self requires+provides
                                                              #:declared-submodule-names (make-hasheq)
                                                              #:add-defined-portal add-defined-portal))]))

   (define mb-scopes-s
     (if keep-enclosing-scope-at-phase
         ;; for `(module* name #f)`, use the `(module* ...)` form:
         (apply-module-scopes s)
         ;; otherwise, use the initial require
         all-scopes-s))

   ;; Need to accumulate definition contexts created during
   ;; expansion to `#%module-begin`:
   (define mb-def-ctx-scopes (box null))
   
   ;; Add `#%module-begin` around the body if it's not already present;
   ;; also logs 'rename-one
   (define mb
     (ensure-module-begin bodys
                          #:module-name-sym module-name-sym
                          #:scopes-s mb-scopes-s
                          #:m-ns m-ns
                          #:ctx mb-ctx
                          #:def-ctx-scopes mb-def-ctx-scopes
                          #:phase phase
                          #:s s
                          #:paramz paramz
                          #:exit-paramz exit-paramz))
  (log-expand ctx 'next)

  (cond
    [(compiled-module-expansion? (syntax-e mb))
     ;; detour: fully compiled linklet bundle/directory returned
     ;; send it on up the chain
     (unless (and (expand-context-to-parsed? init-ctx)
                  (not enclosing-self))
       (raise-syntax-error #f "invalid context for a compiled module from module-begin expansion" bodys))
     (unless (eq? (current-code-inspector) initial-code-inspector)
       (raise-syntax-error #f "compiled-module generation disallowed by code inspector" bodys))
     (parsed-bundle rebuild-s (syntax-e mb))]
    [else

     ;; Expand the body
     (define expanded-mb (performance-region
                          ['expand 'module-begin]
                          (call-with-configure-parameterization
                           paramz
                           exit-paramz
                           (lambda ()
                             (expand mb (struct*-copy expand-context (accumulate-def-ctx-scopes mb-ctx mb-def-ctx-scopes)
                                                      [def-ctx-scopes #f]))))))

     ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     ;; Assemble the `module` result

     (define parsed-mb (if (expanded+parsed? expanded-mb)
                           (expanded+parsed-parsed expanded-mb)
                           expanded-mb))

     (define-values (requires recur-requires flattened-requires provides)
       (extract-requires-and-provides requires+provides self self
                                      #:flatten-requires? (parsed-#%module-begin-flatten-requires? parsed-mb)
                                      #:namespace m-ns))

     (define result-form
       (and (or (expand-context-to-parsed? init-ctx)
                always-produce-compiled?)
            (parsed-module rebuild-s
                           #f
                           (m 'id:module-name)
                           self
                           requires
                           recur-requires
                           flattened-requires
                           provides
                           (requires+provides-all-bindings-simple? requires+provides)
                           (root-expand-context-encode-for-module root-ctx self self)
                           (parsed-#%module-begin-body parsed-mb)
                           (parsed-#%module-begin-realm parsed-mb)
                           portal-syntaxes
                           (unbox compiled-module-box)
                           compiled-submodules)))
     
     (define result-s
       (cond
         [(not (expand-context-to-parsed? init-ctx))
          ;; Shift the "self" reference that we have been using for expansion
          ;; to a generic and constant (for a particular submodule path)
          ;; "self", so that we can reocognize it for compilation or to shift
          ;; back on any future re-expansion:
          (define generic-self (make-generic-self-module-path-index self))
          
          ;; Make `self` like `generic-self`; this hacky update plays the
          ;; role of applying a shift to identifiers that are in syntax
          ;; properties, such as the 'origin property
          (imitate-generic-module-path-index! self)
          (for ([mpi (in-list (unbox mpis-to-reset))])
            (imitate-generic-module-path-index! mpi))

          (let* ([result-s
                  (rebuild
                   rebuild-s
                   `(,(m 'module) ,(m 'id:module-name) ,initial-require-s ,(expanded+parsed-s expanded-mb)))]
                 [result-s 
                  (syntax-module-path-index-shift result-s self generic-self)]
                 [result-s (attach-root-expand-context-properties result-s root-ctx self generic-self)]
                 [result-s (if (requires+provides-all-bindings-simple? requires+provides)
                               (syntax-property result-s 'module-body-context-simple? #t)
                               result-s)])
            (log-expand init-ctx 'rename-one result-s)
            result-s)]))
     
     (cond
       [(expand-context-to-parsed? init-ctx) result-form]
       [always-produce-compiled?
        (expanded+parsed result-s result-form)]
       [else result-s])]))

;; ----------------------------------------

;; Add `#%module-begin` to `bodys`, if needed, and otherwise
;; expand to a core `#%module-begin` form
(define (ensure-module-begin bodys
                             #:module-name-sym module-name-sym
                             #:scopes-s scopes-s
                             #:m-ns m-ns
                             #:ctx ctx
                             #:def-ctx-scopes def-ctx-scopes
                             #:phase phase
                             #:s s
                             #:paramz paramz
                             #:exit-paramz exit-paramz)
  (define (make-mb-ctx)
    (struct*-copy expand-context ctx
                  [context 'module-begin]
                  [only-immediate? #t]
                  [def-ctx-scopes def-ctx-scopes]))
  (define mb
    (cond
     [(= 1 (length bodys))
      ;; Maybe it's already a `#%module-begin` form, or maybe it
      ;; will expand to one
      (cond
       [(eq? '#%module-begin (core-form-sym (car bodys) phase))
        ;; Done
        (car bodys)]
       [else
        ;; A single body form might be a macro that expands to
        ;; the primitive `#%module-begin` form:
        (define named-body-s (add-enclosing-name-property (car bodys) module-name-sym))
        (log-expand ctx 'track-syntax 'property named-body-s (car bodys))
        (define partly-expanded-body
          (performance-region
           ['expand 'module-begin]
           (call-with-configure-parameterization
            paramz
            exit-paramz
            (lambda ()
              (expand named-body-s
                      (make-mb-ctx))))))
        (cond
         [(compiled-module-expansion? (syntax-e partly-expanded-body))
          ;; Fully compiled linklet bundle/directory returned, so no wrapper needed
          partly-expanded-body]
         [(eq? '#%module-begin (core-form-sym partly-expanded-body phase))
          ;; Yes, it expanded to `#%module-begin`
          partly-expanded-body]
         [else
          ;; No, it didn't expand to `#%module-begin`
          (add-module-begin (list partly-expanded-body) s scopes-s phase module-name-sym 
                            (make-mb-ctx)
                            #:log-rename-one? #f)])])]
     [else
      ;; Multiple body forms definitely need a `#%module-begin` wrapper
      (add-module-begin bodys s scopes-s phase module-name-sym
                        (make-mb-ctx))]))
  (define named-mb (add-enclosing-name-property mb module-name-sym))
  (log-expand ctx 'track-syntax 'property named-mb mb)
  named-mb)

;; Add `#%module-begin`, because it's needed
(define (add-module-begin bodys s scopes-s phase module-name-sym mb-ctx
                          #:log-rename-one? [log-rename-one? #t])
  (define mb-id (datum->syntax scopes-s '#%module-begin))
  ;; If `mb-id` is not bound, we'd like to give a clear error message
  (unless (resolve mb-id phase)
    (raise-syntax-error #f "no #%module-begin binding in the module's language" s))
  (define mb (datum->syntax scopes-s `(,mb-id ,@bodys) s s))
  (log-expand mb-ctx 'tag mb)
  (define named-mb (add-enclosing-name-property mb module-name-sym))
  (log-expand mb-ctx 'track-syntax 'property named-mb mb)
  (define partly-expanded-mb (performance-region
                              ['expand 'module-begin]
                              (expand named-mb
                                      mb-ctx)))
  (unless (eq? '#%module-begin (core-form-sym partly-expanded-mb phase))
    (raise-syntax-error #f "expansion of #%module-begin is not a #%plain-module-begin form" s
                        partly-expanded-mb))
  partly-expanded-mb)

(define (add-enclosing-name-property stx module-name-sym)
  (syntax-property stx 'enclosing-module-name module-name-sym))

(define (compiled-module-expansion? v)
  (or (linklet-bundle? v)
      (linklet-directory? v)))

;; ----------------------------------------

;; Make function to adjust syntax that appears in the original module body
(define (make-apply-module-scopes inside-scope outside-scope
                                  init-ctx keep-enclosing-scope-at-phase
                                  self enclosing-self enclosing-mod)
  (lambda (s)
    (performance-region
     ['expand 'module 'scopes]
     (define s-without-enclosing
       (if keep-enclosing-scope-at-phase
           ;; Keep enclosing module scopes for `(module* _ #f ....)`
           s
           ;; Remove the scopes of the top level or a module outside of
           ;; this module, as well as any relevant use-site scopes
           (remove-use-site-scopes
            (remove-scopes s (root-expand-context-module-scopes init-ctx))
            init-ctx)))
     ;; Add outside- and inside-edge scopes
     (define s-with-edges
       (add-scope (add-scope s-without-enclosing
                             outside-scope)
                  inside-scope))
     (define s-with-suitable-enclosing
       (cond
        [keep-enclosing-scope-at-phase
         ;; Shift any references to the enclosing module to be relative to the
         ;; submodule
         (syntax-module-path-index-shift
          s-with-edges
          enclosing-self
          enclosing-mod)]
        [else s-with-edges]))
     ;; In case we're expanding syntax that was previously expanded,
     ;; shift the generic "self" to the "self" for the current expansion:
     (syntax-module-path-index-shift
      s-with-suitable-enclosing
      (make-generic-self-module-path-index self)
      self
      ;; Also preserve the expansion-time code inspector
      (current-code-inspector)))))

;; ----------------------------------------

;; Pass 1 of `module` expansion, which uncovers definitions,
;; requires, and `module` submodules
(define (partially-expand-bodys bodys
                                #:phase phase
                                #:ctx partial-body-ctx
                                #:namespace m-ns
                                #:self self
                                #:frame-id frame-id
                                #:requires-and-provides requires+provides
                                #:need-eventually-defined need-eventually-defined
                                #:all-scopes-stx all-scopes-stx
                                #:defined-syms defined-syms
                                #:declared-keywords declared-keywords
                                #:declared-submodule-names declared-submodule-names
                                #:compiled-submodules compiled-submodules
                                #:modules-being-compiled modules-being-compiled
                                #:mpis-to-reset mpis-to-reset
                                #:portal-syntaxes portal-syntaxes
                                #:add-defined-portal add-defined-portal
                                #:loop pass-1-and-2-loop)
  (namespace-visit-available-modules! m-ns phase)
  (let loop ([tail? #t] [bodys bodys])
    (cond
     [(null? bodys)
      (cond
       [(and tail? (not (zero? phase)))
        null]
       [tail?
        ;; Were at the very end of the module; if there are any lifted-to-end
        ;; declarations, keep going
        (define bodys
          (append
           (get-and-clear-end-lifts! (expand-context-to-module-lifts partial-body-ctx))
           (get-and-clear-provide-lifts! (expand-context-to-module-lifts partial-body-ctx))))
        (cond
          [(null? bodys) null]
          [else
           (define added-bodys (add-post-expansion-scope bodys partial-body-ctx))
           (log-expand partial-body-ctx 'module-end-lifts added-bodys)
           (loop #t added-bodys)])]
       [else null])]
     [else
      (define rest-bodys (cdr bodys))
      (log-expand partial-body-ctx 'next)
      (define exp-body (performance-region
                        ['expand 'form-in-module/1]
                        ;; --- expand to core form ---
                        (expand (car bodys) partial-body-ctx)))
      (define lifted-defns (get-and-clear-lifts! (expand-context-lifts partial-body-ctx)))
      (define lifted-reqs (get-and-clear-require-lifts! (expand-context-require-lifts partial-body-ctx)))
      (define lifted-mods (get-and-clear-module-lifts! (expand-context-module-lifts partial-body-ctx)))
      (define added-lifted-mods (add-post-expansion-scope lifted-mods partial-body-ctx))
      (unless (and (null? lifted-defns) (null? lifted-reqs) (null? lifted-mods))
        (log-expand partial-body-ctx 'module-pass1-lifts
                    (lifted-defns-extract-syntax lifted-defns)
                    lifted-reqs
                    added-lifted-mods))
      (define exp-lifted-mods (loop #f added-lifted-mods))
      (log-expand partial-body-ctx 'module-pass1-case exp-body)
      (append/tail-on-null
       ;; Save any requires lifted during partial expansion
       lifted-reqs
       ;; Ditto for expressions
       lifted-defns
       ;; Ditto for modules, which need to be processed
       exp-lifted-mods
       ;; Dispatch on form revealed by partial expansion
       (case (core-form-sym exp-body phase)
         [(begin)
          (log-expand partial-body-ctx 'prim-begin exp-body)
          (define-match m exp-body '(begin e ...))
          (define (track e) (syntax-track-origin e exp-body))
          (define spliced-bodys (append (map track (m 'e)) rest-bodys))
          (log-expand partial-body-ctx 'splice spliced-bodys)
          (loop tail? spliced-bodys)]
         [(begin-for-syntax)
          (log-expand partial-body-ctx 'prim-begin-for-syntax exp-body)
          (define-match m exp-body '(begin-for-syntax e ...))
          (log-expand partial-body-ctx 'prepare-env)
          (define ct-m-ns (namespace->namespace-at-phase m-ns (add1 phase)))
          (prepare-next-phase-namespace partial-body-ctx)
          (log-expand partial-body-ctx 'phase-up)
          (define nested-bodys (pass-1-and-2-loop (m 'e) (add1 phase) #f null))
          (log-expand partial-body-ctx 'next-group)
          (namespace-run-available-modules! m-ns (add1 phase)) ; to support running `begin-for-syntax`
          (eval-nested-bodys nested-bodys (add1 phase) ct-m-ns self partial-body-ctx)
          (namespace-visit-available-modules! m-ns phase) ; since we're shifting back a phase
          (log-expand partial-body-ctx 'exit-case
                      (let ([s-nested-bodys (for/list ([nested-body (in-list nested-bodys)])
                                              (extract-syntax nested-body))])
                        (cons (m 'begin-for-syntax) s-nested-bodys)))
          (cons
           (semi-parsed-begin-for-syntax exp-body nested-bodys)
           (loop tail? rest-bodys))]
         [(define-values)
          (log-expand partial-body-ctx 'prim-define-values exp-body)
          (define-match m exp-body '(define-values (id ...) rhs))
          (define ids (remove-use-site-scopes (m 'id) partial-body-ctx))
          (check-no-duplicate-ids ids phase exp-body)
          (check-ids-unbound ids phase requires+provides #:in exp-body)
          (define syms (select-defined-syms-and-bind! ids defined-syms 
                                                      self phase all-scopes-stx
                                                      #:frame-id frame-id
                                                      #:requires+provides requires+provides
                                                      #:in exp-body))
          (for ([sym (in-list syms)])
            ;; In case `local-expand` created a binding with `sym` to a transformer
            (namespace-unset-transformer! m-ns phase sym))
          (add-defined-syms! requires+provides syms phase)
          (log-expand partial-body-ctx 'exit-case `(,(m 'define-values) ,ids ,(m 'rhs)))
          (cons
           (semi-parsed-define-values exp-body syms ids (m 'rhs))
           (loop tail? rest-bodys))]
         [(define-syntaxes)
          (log-expand partial-body-ctx 'prim-define-syntaxes exp-body)
          (define-match m exp-body '(define-syntaxes (id ...) rhs))
          (log-expand partial-body-ctx 'prepare-env)
          (prepare-next-phase-namespace partial-body-ctx)
          (log-expand partial-body-ctx 'phase-up)
          (define ids (remove-use-site-scopes (m 'id) partial-body-ctx))
          (check-no-duplicate-ids ids phase exp-body)
          (check-ids-unbound ids phase requires+provides #:in exp-body)
          (define syms (select-defined-syms-and-bind! ids defined-syms
                                                      self phase all-scopes-stx
                                                      #:frame-id frame-id
                                                      #:requires+provides requires+provides
                                                      #:in exp-body
                                                      #:as-transformer? #t))
          (add-defined-syms! requires+provides syms phase #:as-transformer? #t)
          (define (install-values vals)
            ;; Install transformers in the namespace for expansion:
            (for ([sym (in-list syms)]
                  [val (in-list vals)]
                  [id (in-list ids)])
              (maybe-install-free=id-in-context! val id phase partial-body-ctx)
              (maybe-install-portal-syntax! val sym phase portal-syntaxes)
              (namespace-set-transformer! m-ns phase sym val)))
          ;; Expand and evaluate RHS:
          (define-values (exp-rhs parsed-rhs vals)
            (expand+eval-for-syntaxes-binding 'define-syntaxes
                                              (m 'rhs) ids
                                              (struct*-copy expand-context partial-body-ctx
                                                            [lifts #f]
                                                            ;; require lifts ok, others disallowed
                                                            [module-lifts #f]
                                                            [to-module-lifts #f]
                                                            [need-eventually-defined need-eventually-defined])
                                              #:log-next? #f
                                              #:wrap (lambda (go)
                                                       (call-with-module-prompt/value-list
                                                        'define-syntaxes
                                                        go
                                                        ids
                                                        install-values))))
          (log-expand partial-body-ctx 'exit-case `(,(m 'define-syntaxes) ,ids ,exp-rhs))
          (define parsed-body (parsed-define-syntaxes (keep-properties-only exp-body) ids syms parsed-rhs))
          (cons (if (expand-context-to-parsed? partial-body-ctx)
                    parsed-body
                    (expanded+parsed
                     (rebuild 
                      exp-body
                      `(,(m 'define-syntaxes) ,ids ,exp-rhs))
                     parsed-body))
                (loop tail? rest-bodys))]
         [(#%require)
          (log-expand partial-body-ctx 'prim-require exp-body)
          (define ready-body (remove-use-site-scopes exp-body partial-body-ctx))
          (define-match m ready-body '(#%require req ...))
          (parse-and-perform-requires! (m 'req) exp-body #:self self
                                       m-ns phase #:run-phase phase
                                       requires+provides
                                       #:declared-submodule-names declared-submodule-names
                                       #:who 'module
                                       #:all-scopes-stx all-scopes-stx
                                       #:add-defined-portal add-defined-portal)
          (log-expand partial-body-ctx 'exit-case ready-body)
          (cons ready-body
                (loop tail? rest-bodys))]
         [(#%provide)
          (log-expand partial-body-ctx 'prim-stop #f)
          ;; save for last pass
          (cons exp-body
                (loop tail? rest-bodys))]
         [(module)
          ;; Submodule to parse immediately
          (log-expand partial-body-ctx 'prim-submodule #f)
          (define ready-body (remove-use-site-scopes exp-body partial-body-ctx))
          (define submod
            (expand-submodule ready-body self partial-body-ctx
                              #:is-star? #f
                              #:declared-submodule-names declared-submodule-names
                              #:mpis-to-reset mpis-to-reset
                              #:compiled-submodules compiled-submodules
                              #:modules-being-compiled modules-being-compiled))
          (cons submod
                (loop tail? rest-bodys))]
         [(module*)
          ;; Submodule to save for after this module
          (log-expand partial-body-ctx 'prim-stop #f)
          (cons exp-body
                (loop tail? rest-bodys))]
         [(#%declare)
          (log-expand partial-body-ctx 'prim-declare exp-body)
          (define-match m exp-body '(#%declare kw ...))
          (let loop ([kws (m 'kw)])
            (unless (null? kws)
              (define kw (car kws))
              (unless (keyword? (syntax-e kw))
                (raise-syntax-error #f "expected a keyword" exp-body kw))
              (unless (memq (syntax-e kw) '(#:cross-phase-persistent #:empty-namespace #:unsafe
                                            #:unlimited-require #:realm
                                            #:require=define #:flatten-requires))
                (raise-syntax-error #f "not an allowed declaration keyword" exp-body kw))
              (define has-arg? (eq? (syntax-e kw) '#:realm))
              (when (hash-ref declared-keywords (syntax-e kw) #f)
                (raise-syntax-error #f "keyword declared multiple times" exp-body kw))
              (when (eq? (syntax-e kw) '#:unsafe)
                (unless (eq? (current-code-inspector) initial-code-inspector)
                  (raise-syntax-error #f "unsafe compilation disallowed by code inspector" exp-body kw)))
              (when (eq? (syntax-e kw) '#:realm)
                (unless (and (pair? (cdr kws))
                             (identifier? (cadr kws)))
                  (raise-syntax-error #f "expected an identifier after keyword" exp-body kw)))
              (when (eq? (syntax-e kw) '#:require=define)
                (disable-definitions-shadow-imports! requires+provides))
              (hash-set! declared-keywords (syntax-e kw) (if has-arg? (cadr kws) kw))
              (loop (if has-arg? (cddr kws) (cdr kws)))))
          (define parsed-body (parsed-#%declare exp-body))
          (cons (if (expand-context-to-parsed? partial-body-ctx)
                    parsed-body
                    (expanded+parsed exp-body parsed-body))
                (loop tail? rest-bodys))]
         [else
          ;; save expression for next pass
          (log-expand partial-body-ctx 'prim-stop #f)
          (cons exp-body
                (loop tail? rest-bodys))]))])))

;; Convert lifted identifiers plus expression to a `define-values` form:
(define (make-wrap-as-definition self frame-id
                                 inside-scope all-scopes-stx
                                 defined-syms requires+provides)
  (lambda (ids rhs phase)
    (define scoped-ids (for/list ([id (in-list ids)])
                         (add-scope id inside-scope)))
    (define syms
      (select-defined-syms-and-bind! scoped-ids defined-syms
                                     self phase all-scopes-stx
                                     #:frame-id frame-id
                                     #:requires+provides requires+provides))
    (define s (add-scope (datum->syntax
                          #f
                          (list (datum->syntax (syntax-shift-phase-level core-stx phase)
                                               'define-values)
                                scoped-ids
                                rhs))
                         inside-scope))
    (values scoped-ids
            (semi-parsed-define-values s syms scoped-ids rhs))))

(define (add-post-expansion-scope bodys ctx)
  (define pe (root-expand-context-post-expansion ctx))
  (if pe
      (for/list ([body (in-list bodys)])
        (apply-post-expansion pe body))
      bodys))

;; ----------------------------------------

;; Pass 2 of `module` expansion, which expands all expressions
(define (finish-expanding-body-expressions partially-expanded-bodys
                                           #:phase phase
                                           #:ctx body-ctx
                                           #:self self
                                           #:declared-submodule-names declared-submodule-names
                                           #:compiled-submodules compiled-submodules
                                           #:modules-being-compiled modules-being-compiled
                                           #:mpis-to-reset mpis-to-reset)
  (let loop ([tail? #t] [bodys partially-expanded-bodys])
    (cond
     [(null? bodys)
      (cond
        [(and tail? (not (zero? phase)))
         null]
        [tail? 
         ;; We're at the very end of the module, again, so check for lifted-to-end
         ;; declarations
         (define bodys
           (append
            (get-and-clear-end-lifts! (expand-context-to-module-lifts body-ctx))
            (get-and-clear-provide-lifts! (expand-context-to-module-lifts body-ctx))))
         (cond
           [(null? bodys)
            null]
           [else
            (log-expand body-ctx 'module-end-lifts bodys)
            (loop #t (add-post-expansion-scope bodys body-ctx))])]
        [else null])]
     [else
      (log-expand body-ctx 'next)
      (define body (car bodys))
      (define rest-bodys (cdr bodys))
      (define exp-body
        (cond
         [(or (parsed? body)
              (expanded+parsed? body)
              (semi-parsed-begin-for-syntax? body))
          ;; An already-parsed (enough for now) form
          body]
         [(semi-parsed-define-values? body)
          (define ids (semi-parsed-define-values-ids body))
          (define rhs-ctx (as-named-context (as-expression-context body-ctx) ids))
          (define syms (semi-parsed-define-values-syms body))
          (define s (semi-parsed-define-values-s body))
          (define-match m s #:unless (expand-context-to-parsed? rhs-ctx)
            '(define-values _ _))
          (define rebuild-s (keep-as-needed rhs-ctx s #:keep-for-parsed? #t))
          (log-expand* body-ctx ['visit #f] ['enter-prim #f] ['prim-define-values #f])
          (define exp-rhs (performance-region
                           ['expand 'form-in-module/2]
                           (expand (semi-parsed-define-values-rhs body) rhs-ctx)))
          (log-expand* body-ctx ['exit-prim/return #f])
          (define comp-form
            (parsed-define-values rebuild-s ids syms
                                  (if (expand-context-to-parsed? rhs-ctx)
                                      ;; Have (and need only) parsed form
                                      exp-rhs
                                      ;; Expand rhs again to parse it
                                      (expand exp-rhs (as-to-parsed-context rhs-ctx)))))
          (if (expand-context-to-parsed? rhs-ctx)
              comp-form
              (expanded+parsed
               (rebuild
                rebuild-s
                `(,(m 'define-values) ,ids ,exp-rhs))
               comp-form))]
         [else
          (case (core-form-sym body phase)
            [(#%require #%provide module*)
             ;; handle earlier or later
             body]
            [else
             (performance-region
              ['expand 'form-in-module/2]
              (define exp-body (expand body (as-expression-context body-ctx)))
              (if (expand-context-to-parsed? body-ctx)
                  ;; Have (and need only) parsed form
                  exp-body
                  ;; Expand again to parse it
                  (expanded+parsed
                   exp-body
                   (expand exp-body (as-to-parsed-context body-ctx)))))])]))
      (define lifted-defns (get-and-clear-lifts! (expand-context-lifts body-ctx)))
      (define lifted-requires
        ;; Get any requires and provides, keeping them as-is
        (get-and-clear-require-lifts! (expand-context-require-lifts body-ctx)))
      (define lifted-modules (get-and-clear-module-lifts! (expand-context-module-lifts body-ctx)))
      (define no-lifts? (and (null? lifted-defns) (null? lifted-modules) (null? lifted-requires)))
      (unless no-lifts?
        (log-expand body-ctx 'module-pass2-lifts
                    lifted-requires
                    (add-post-expansion-scope lifted-modules body-ctx)
                    (lifted-defns-extract-syntax lifted-defns)))
      (define exp-lifted-modules
        ;; If there were any module lifts, the `module` forms need to
        ;; be expanded
        (expand-non-module*-submodules lifted-modules
                                       phase
                                       self
                                       body-ctx
                                       #:mpis-to-reset mpis-to-reset
                                       #:declared-submodule-names declared-submodule-names
                                       #:compiled-submodules compiled-submodules
                                       #:modules-being-compiled modules-being-compiled))
      (unless no-lifts? (log-expand body-ctx 'next-group))
      (define exp-lifted-defns
        ;; If there were any lifts, the right-hand sides need to be expanded
        (loop #f lifted-defns))
      (unless no-lifts? (log-expand body-ctx 'next-group))
      (append
       lifted-requires
       exp-lifted-modules
       exp-lifted-defns
       (cons exp-body
             (loop tail? rest-bodys)))])))

(define (check-defined-by-now need-eventually-defined self ctx requires+provides)
  ;; If `need-eventually-defined` is not empty, report an error
  (for ([(phase l) (in-hash need-eventually-defined)])
    (for ([id (in-list (reverse l))])
      (define b (resolve+shift id phase))
      (define bound-here? (and b
                               (module-binding? b)
                               (eq? (module-binding-sym b) (syntax-e id))
                               (eq? (module-binding-module b) self)))
      (define bound-kind (and bound-here?
                              (defined-sym-kind requires+provides (module-binding-sym b) phase)))
      (unless (eq? bound-kind 'variable)
        (raise-syntax-error #f
                            (string-append
                             (cond
                               [(not b) "reference to an unbound identifier"]
                               [(eq? bound-kind 'transformer) "identifier treated as a variable, but later defined as syntax"]
                               [else "identifier treated as a variable, but later bound differently"])
                             (format "\n  at phase: ~a" (case phase
                                                          [(1) "1; the transformer environment"]
                                                          [else phase])))
                            id #f null
                            (syntax-debug-info-string id ctx))))))

;; ----------------------------------------

;; Pass 3 of `module` expansion, which parses `provide` forms and
;; matches them up with definitions and requires
(define (resolve-provides expression-expanded-bodys
                          #:requires-and-provides requires+provides
                          #:declared-submodule-names declared-submodule-names
                          #:namespace m-ns
                          #:phase phase
                          #:self self
                          #:ctx ctx)
  (performance-region
   ['expand 'provide]
   (let loop ([bodys expression-expanded-bodys] [phase phase])
     (cond
      [(null? bodys) null]
      [(or (parsed? (car bodys))
           (expanded+parsed? (car bodys)))
       (log-expand ctx 'next)
       (cons (car bodys)
             (loop (cdr bodys) phase))]
      [(semi-parsed-begin-for-syntax? (car bodys))
       (log-expand ctx 'enter-begin-for-syntax)
       (define nested-bodys (loop (semi-parsed-begin-for-syntax-body (car bodys)) (add1 phase)))
       (log-expand ctx 'exit-begin-for-syntax)
       ;; Stil semi-parsed; finished in pass 4
       (cons (struct-copy semi-parsed-begin-for-syntax (car bodys)
                          [body nested-bodys])
             (loop (cdr bodys) phase))]
      [else
       (define body (car bodys))
       (case (core-form-sym body phase)
         [(#%provide)
          (log-expand* ctx ['enter-prim body] ['prim-provide body])
          (define-match m body '(#%provide spec ...))
          (define-values (track-stxes specs)
            (parse-and-expand-provides! (m 'spec) body
                                        requires+provides self
                                        phase (struct*-copy expand-context ctx
                                                            [context 'top-level]
                                                            [phase phase]
                                                            [namespace (namespace->namespace-at-phase m-ns phase)]
                                                            [requires+provides requires+provides]
                                                            [declared-submodule-names declared-submodule-names])))
          (cond
           [(expand-context-to-parsed? ctx)
            (loop (cdr bodys) phase)]
           [else
            (define new-s
              (syntax-track-origin*
               track-stxes
               (rebuild
                body
                `(,(m '#%provide) ,@specs))))
            (log-expand ctx 'exit-prim new-s)
            (cons new-s
                  (loop (cdr bodys) phase))])]
         [else
          (log-expand ctx 'next)
          (cons body
                (loop (cdr bodys) phase))])]))))

;; ----------------------------------------

;; In support of pass 4, declare a module (in a temporary namespace)
;; before any `module*` submodule is expanded
(define (declare-module-for-expansion fully-expanded-bodys-except-post-submodules
                                      #:module-name-id module-name-id
                                      #:rebuild-s rebuild-s
                                      #:requires-and-provides requires+provides
                                      #:namespace m-ns
                                      #:self self
                                      #:enclosing enclosing-self
                                      #:root-ctx root-ctx
                                      #:ctx ctx
                                      #:modules-being-compiled modules-being-compiled
                                      #:realm realm
                                      #:flatten-requires? flatten-requires?
                                      #:portal-syntaxes portal-syntaxes
                                      #:fill compiled-module-box)

  (define-values (requires recur-requires flattened-requires provides)
    (extract-requires-and-provides requires+provides self self
                                   #:flatten-requires? flatten-requires?
                                   #:namespace m-ns))

  (define parsed-mod
    (parsed-module rebuild-s
                   #f
                   module-name-id
                   self
                   requires
                   recur-requires
                   flattened-requires
                   provides
                   (requires+provides-all-bindings-simple? requires+provides)
                   (root-expand-context-encode-for-module root-ctx self self)
                   (parsed-only fully-expanded-bodys-except-post-submodules)
                   realm
                   portal-syntaxes
                   #f
                   (hasheq)))

  (define module-name (module-path-index-resolve (or enclosing-self self)))
  (define compiled-module
    (compile-module parsed-mod
                    (make-compile-context #:namespace m-ns
                                          #:module-self enclosing-self
                                          #:full-module-name (and enclosing-self
                                                                  (resolved-module-path-name module-name)))
                    #:serializable? (expand-context-for-serializable? ctx)
                    #:to-correlated-linklet? (expand-context-to-correlated-linklet? ctx)
                    #:modules-being-compiled modules-being-compiled
                    #:need-compiled-submodule-rename? #f))
  (set-box! compiled-module-box compiled-module)
  
  (define root-module-name (resolved-module-path-root-name module-name))
  (parameterize ([current-namespace m-ns]
                 [current-module-declare-name (make-resolved-module-path root-module-name)])
    (eval-module compiled-module
                 #:with-submodules? #f)))

(define (attach-root-expand-context-properties s root-ctx orig-self new-self)
  ;; Original API:
  (let* ([s (syntax-property s 'module-body-context (root-expand-context-all-scopes-stx root-ctx))]
         [s (syntax-property s
                             'module-body-inside-context
                             (apply-post-expansion (root-expand-context-post-expansion root-ctx)
                                                   empty-syntax))])
    s))

;; ----------------------------------------

;; Pass 4 of `module` expansion, which expands `module*` forms;
;; this pass muct happen after everything else for the module, since a
;; `module*` submodule can require from its enclosing module; in
;; addition to expanding `module*`, generate expanded `begin-for-syntax`
;; as needed and ensure that parsed `begin-for-syntax` has only parsed
;; forms
(define (expand-post-submodules fully-expanded-bodys-except-post-submodules
                                #:declare-enclosing declare-enclosing-module
                                #:phase phase
                                #:self self
                                #:requires-and-provides requires+provides
                                #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                                #:all-scopes-s all-scopes-s
                                #:mpis-to-reset mpis-to-reset
                                #:declared-submodule-names declared-submodule-names
                                #:compiled-submodules compiled-submodules
                                #:modules-being-compiled modules-being-compiled
                                #:ctx submod-ctx)
  (let loop ([bodys fully-expanded-bodys-except-post-submodules] [phase phase])
    (cond
     [(null? bodys) null]
     [else
      (define body (car bodys))
      (define rest-bodys (cdr bodys))
      (cond
       [(semi-parsed-begin-for-syntax? body)
        (define body-s (semi-parsed-begin-for-syntax-s body))
        (log-expand submod-ctx 'enter-begin-for-syntax)
        (define-match m body-s '(begin-for-syntax _ ...))
        (define rebuild-body-s (keep-as-needed submod-ctx body-s))
        (define nested-bodys (loop (semi-parsed-begin-for-syntax-body body) (add1 phase)))
        (define parsed-bfs (parsed-begin-for-syntax rebuild-body-s (parsed-only nested-bodys)))
        (log-expand submod-ctx 'exit-begin-for-syntax)
        (cons
         (if (expand-context-to-parsed? submod-ctx)
             parsed-bfs
             (expanded+parsed
              (rebuild rebuild-body-s `(,(m 'begin-for-syntax) ,@(syntax-only nested-bodys)))
              parsed-bfs))
         (loop rest-bodys phase))]
       [(or (parsed? body)
            (expanded+parsed? body))
        ;; We can skip any other parsed form
        (log-expand submod-ctx 'next)
        (cons body
              (loop rest-bodys phase))]
       [else
        (case (core-form-sym body phase)
          [(module*)
           ;; Ensure that the enclosing module is declared:
           (force declare-enclosing-module)
           (define ready-body (remove-use-site-scopes body submod-ctx))
           (define-match f-m  body #:try '(module* name #f . _))
           (define submod
             (cond
              [(f-m)
               ;; Need to shift the submodule relative to the enclosing module:
               (define neg-phase (phase- 0 phase))
               (define shifted-s (syntax-shift-phase-level ready-body neg-phase))
               (define submod
                 (expand-submodule shifted-s self submod-ctx
                                   #:is-star? #t
                                   #:keep-enclosing-scope-at-phase neg-phase
                                   #:enclosing-requires+provides requires+provides
                                   #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                                   #:mpis-to-reset mpis-to-reset
                                   #:declared-submodule-names declared-submodule-names
                                   #:compiled-submodules compiled-submodules
                                   #:modules-being-compiled modules-being-compiled))
               (cond
                [(parsed? submod) submod]
                [(expanded+parsed? submod)
                 (struct-copy expanded+parsed submod
                              [s (syntax-shift-phase-level (expanded+parsed-s submod) phase)])]
                [else (syntax-shift-phase-level submod phase)])]
              [else
               (expand-submodule ready-body self submod-ctx
                                 #:is-star? #t
                                 #:mpis-to-reset mpis-to-reset
                                 #:declared-submodule-names declared-submodule-names
                                 #:compiled-submodules compiled-submodules
                                 #:modules-being-compiled modules-being-compiled)]))
           (cons submod
                 (loop rest-bodys phase))]
          [else
           ;; We can skip any other unparsed form
           (log-expand submod-ctx 'next)
           (cons body
                 (loop rest-bodys phase))])])])))

(define (stop-at-module*? ctx)
  (free-id-set-member? (expand-context-stops ctx)
                       (expand-context-phase ctx)
                       (syntax-shift-phase-level (datum->syntax core-stx 'module*)
                                                 (expand-context-phase ctx))))

;; ----------------------------------------

(define (check-ids-unbound ids phase requires+provides #:in s)
  (for ([id (in-list ids)])
    (check-not-defined requires+provides id phase #f #:in s #:who 'module)))

;; ----------------------------------------

(define (eval-nested-bodys bodys phase m-ns self ctx)
  ;; The definitions and expression `bodys` are fully expanded and
  ;; parsed; evaluate them
  (for ([body (in-list bodys)])
    (define p (if (expanded+parsed? body)
                  (expanded+parsed-parsed body)
                  body))
    (cond
     [(parsed-define-values? p)
      (define syms (parsed-define-values-syms p))
      (define ids (parsed-define-values-ids p))
      (eval-for-bindings 'define-values ids (parsed-define-values-rhs p) phase m-ns ctx
                         #:wrap (lambda (go)
                                  ;; prompt is outside setting variables, to be consistent
                                  ;; with a visit where definitions have prompts
                                  (call-with-module-prompt/value-list
                                   'define
                                   go
                                   ids
                                   (lambda (vals)
                                     (for ([sym (in-list syms)]
                                           [val (in-list vals)])
                                       (namespace-set-variable! m-ns phase sym val))))))
      ;; In case the module prompt was used to escape, to be consistent
      ;; with a visit later, complain if variables are not set
      (for ([sym (in-list syms)])
        (namespace-get-variable m-ns phase sym
                                (lambda ()
                                  (raise
                                   (exn:fail:contract:variable
                                    (string-append "define-values: skipped variable definition during expansion\n"
                                                   "  variable: " (symbol->string sym))
                                    (current-continuation-marks)
                                    sym)))))]
     [(or (parsed-define-syntaxes? p)
          (semi-parsed-begin-for-syntax? p))
      ;; already evaluated during expansion
      (void)]
     [(or (parsed-#%declare? p)
          (syntax? p))
      ;; handled earlier or later
      (void)]
     [else
      ;; an expression
      (parameterize ([current-namespace m-ns])
        (parameterize-like
         #:with ([current-expand-context ctx])
         (let ([c (compile-single p (make-compile-context
                                     #:namespace m-ns
                                     #:phase phase))])
           (call-with-module-prompt
            (lambda ()
              (eval-single-top c m-ns))))))])))

;; ----------------------------------------

(define (expand-submodule s self ctx
                          #:is-star? is-star?
                          #:keep-enclosing-scope-at-phase [keep-enclosing-scope-at-phase #f]
                          #:enclosing-requires+provides [enclosing-r+p #f]
                          #:enclosing-is-cross-phase-persistent? [enclosing-is-cross-phase-persistent? #f]
                          #:mpis-to-reset mpis-to-reset
                          #:declared-submodule-names declared-submodule-names
                          #:compiled-submodules compiled-submodules
                          #:modules-being-compiled modules-being-compiled)
  (log-expand* ctx ['enter-prim s] [(if is-star? 'prim-submodule* 'prim-submodule) #f])

  ;; Register name and check for duplicates
  (define-match m s '(module name . _))
  (define name (syntax-e (m 'name)))
  (when (hash-ref declared-submodule-names name #f)
    (raise-syntax-error #f "submodule already declared with the same name" s name))
  (hash-set! declared-submodule-names name (syntax-e (m 'module)))

  (log-expand* ctx ['enter-prim s])

  (define submod
    (expand-module s
                   (struct*-copy expand-context ctx
                                 [context 'module]
                                 [stops empty-free-id-set]
                                 [post-expansion #:parent root-expand-context #f])
                   self
                   #:always-produce-compiled? #t
                   #:keep-enclosing-scope-at-phase keep-enclosing-scope-at-phase
                   #:enclosing-requires+provides enclosing-r+p
                   #:enclosing-is-cross-phase-persistent? enclosing-is-cross-phase-persistent?
                   #:mpis-for-enclosing-reset mpis-to-reset
                   #:modules-being-compiled modules-being-compiled))

  (log-expand* ctx ['exit-prim (extract-syntax submod)])
  
  ;; Compile and declare the submodule for use by later forms
  ;; in the enclosing module:
  (define ns (expand-context-namespace ctx))
  (define module-name (module-path-index-resolve self))
  (define root-module-name (resolved-module-path-root-name module-name))
  (define compiled-submodule
    (compile-module (if (expanded+parsed? submod)
                        (expanded+parsed-parsed submod)
                        submod)
                    (make-compile-context #:namespace ns
                                          #:module-self self
                                          #:full-module-name (resolved-module-path-name module-name))
                    #:force-linklet-directory? #t
                    #:serializable? (expand-context-for-serializable? ctx)
                    #:to-correlated-linklet? (expand-context-to-correlated-linklet? ctx)
                    #:modules-being-compiled modules-being-compiled
                    #:need-compiled-submodule-rename? #f))
  (hash-set! compiled-submodules name (cons is-star? compiled-submodule))
  (parameterize ([current-namespace ns]
                 [current-module-declare-name (make-resolved-module-path root-module-name)])
    (eval-module compiled-submodule
                 #:with-submodules? #f))

  ;; Return the expanded submodule
  (cond
   [(not is-star?)
    submod]
   [(expanded+parsed? submod)
    (struct-copy expanded+parsed submod
                 [parsed (struct-copy parsed-module (expanded+parsed-parsed submod)
                                      [star? #t])])]
   [else
    (struct-copy parsed-module submod
                 [star? #t])]))

;; Expand `module` forms, leave `module*` forms alone:
(define (expand-non-module*-submodules bodys phase self ctx
                                       #:mpis-to-reset mpis-to-reset
                                       #:declared-submodule-names declared-submodule-names
                                       #:compiled-submodules compiled-submodules
                                       #:modules-being-compiled modules-being-compiled)
  (for/list ([body (in-list bodys)])
    (log-expand ctx 'next)
    (case (core-form-sym body phase)
      [(module)
       (expand-submodule body self ctx
                         #:is-star? #f
                         #:mpis-to-reset mpis-to-reset
                         #:declared-submodule-names declared-submodule-names
                         #:compiled-submodules compiled-submodules
                         #:modules-being-compiled modules-being-compiled)]
      [else body])))

;; ----------------------------------------

(define (make-parse-lifted-require m-ns self requires+provides
                                   #:declared-submodule-names declared-submodule-names
                                   #:add-defined-portal add-defined-portal)
  (lambda (s phase)
    (define-match m s '(#%require req))
    (parse-and-perform-requires! (list (m 'req)) s #:self self
                                 m-ns phase #:run-phase phase
                                 requires+provides
                                 #:declared-submodule-names declared-submodule-names
                                 #:add-defined-portal add-defined-portal
                                 #:who 'require)
    (set-requires+provides-all-bindings-simple?! requires+provides #f)
    s))

;; ----------------------------------------

(define (defn-extract-syntax defn)
  (datum->syntax #f `(define-values ,(semi-parsed-define-values-ids defn)
                       ,(semi-parsed-define-values-rhs defn))
                 (semi-parsed-define-values-s defn)))

(define (lifted-defns-extract-syntax lifted-defns)
  (for/list ([lifted-defn (in-list lifted-defns)])
    (defn-extract-syntax lifted-defn)))

;; ----------------------------------------

(define (maybe-install-portal-syntax! val sym phase portal-syntaxes)
  (when (portal-syntax? val)
    (add-portal-stx! portal-syntaxes val sym phase)))

(define (add-portal-stx! portal-syntaxes val sym phase)
  (define ht (hash-ref portal-syntaxes phase #hasheq()))
  (hash-set! portal-syntaxes
             phase
             (hash-set ht sym (portal-syntax-content val))))

(define (make-add-defined-portal self requires+provides
                                 portal-syntaxes defined-syms
                                 all-scopes-stx frame-id m-ns)
  (lambda (id phase portal-stx orig-s)
    (check-ids-unbound (list id) phase requires+provides #:in orig-s)
    (define syms (select-defined-syms-and-bind! (list id) defined-syms
                                                self phase all-scopes-stx
                                                #:requires+provides requires+provides
                                                #:in orig-s
                                                #:frame-id frame-id
                                                #:as-transformer? #t))
    (add-defined-syms! requires+provides syms phase #:as-transformer? #t)
    (define sym (car syms))
    (define t (portal-syntax portal-stx #f))
    (namespace-set-transformer! m-ns phase sym t)
    (add-portal-stx! portal-syntaxes t sym phase)
    sym))
