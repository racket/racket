#lang racket/base
(require "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/match.rkt"
         "../common/phase.rkt"
         "../namespace/core.rkt"
         "../namespace/module.rkt"
         "../common/module-path.rkt"
         "../common/performance.rkt"
         "../expand/parsed.rkt"
         "module-use.rkt"
         "serialize.rkt"
         "side-effect.rkt"
         "built-in-symbol.rkt"
         "../host/linklet.rkt"
         "context.rkt"
         "header.rkt"
         "reserved-symbol.rkt"
         "instance.rkt"
         "form.rkt"
         "compiled-in-memory.rkt"
         "linklet.rkt"
         "correlated-linklet.rkt"
         "../eval/reflect.rkt"
         "../eval/reflect-name.rkt")

(provide compile-module)

;; Compiles module to a set of linklets that is returned as a
;; `compiled-in-memory`
(define (compile-module p cctx
                        #:force-linklet-directory? [force-linklet-directory? #f]
                        #:serializable? [serializable? #f]
                        #:to-correlated-linklet? [to-correlated-linklet? #f]
                        #:modules-being-compiled [modules-being-compiled (make-hasheq)]
                        #:need-compiled-submodule-rename? [need-compiled-submodule-rename? #t])

  (define full-module-name (let ([parent-full-name (compile-context-full-module-name cctx)]
                                 [name (syntax-e (parsed-module-name-id p))])
                             (if parent-full-name
                                 (append (if (list? parent-full-name)
                                             parent-full-name
                                             (list parent-full-name))
                                         (list name))
                                 name)))
  
  ;; Extract submodules; each list is (cons linklet-directory-key compiled-in-memory)
  (define compiled-submodules (parsed-module-compiled-submodules p))
  (define (get-submodules star?)
    (for/list ([(name star?+compiled) (in-hash compiled-submodules)]
               #:when (eq? star? (car star?+compiled)))
      (cons name (if (and need-compiled-submodule-rename?
                          (not (parsed-module-compiled-module p)))
                     (update-submodule-names (cdr star?+compiled) name full-module-name)
                     (cdr star?+compiled)))))
  (define pre-submodules (sort (get-submodules #f) symbol<? #:key car))
  (define post-submodules (sort (get-submodules #t) symbol<? #:key car))

  (cond
   [(parsed-module-compiled-module p)
    => (lambda (c)
         ;; We've already compiled the module body during expansion.
         ;; Update the name in the compiled form and add in submodules.
         (define-values (name prefix) (if (symbol? full-module-name)
                                          (values full-module-name null)
                                          (let ([r (reverse full-module-name)])
                                            (values (car r) (reverse (cdr r))))))
         (define m (change-module-name c name prefix))
         (module-compiled-submodules (module-compiled-submodules m #t (map cdr pre-submodules))
                                     #f
                                     (map cdr post-submodules)))]
   [else
    (compile-module-from-parsed p cctx
                                #:full-module-name full-module-name
                                #:force-linklet-directory? force-linklet-directory?
                                #:serializable? serializable?
                                #:to-correlated-linklet? to-correlated-linklet?
                                #:modules-being-compiled modules-being-compiled
                                #:pre-submodules pre-submodules
                                #:post-submodules post-submodules
                                #:need-compiled-submodule-rename? need-compiled-submodule-rename?)]))

;; ------------------------------------------------------------

(define (compile-module-from-parsed p cctx
                                    #:full-module-name full-module-name
                                    #:force-linklet-directory? force-linklet-directory?
                                    #:serializable? serializable?
                                    #:to-correlated-linklet? to-correlated-linklet?
                                    #:modules-being-compiled modules-being-compiled
                                    #:pre-submodules pre-submodules   ; sorted by name
                                    #:post-submodules post-submodules ; sorted by name
                                    #:need-compiled-submodule-rename? need-compiled-submodule-rename?)
  (performance-region
   ['compile 'module]
   
   (define enclosing-self (compile-context-module-self cctx))
   (define self (parsed-module-self p))
   (define requires (parsed-module-requires p))
   (define provides (parsed-module-provides p))
   (define encoded-root-expand-ctx-box (box (parsed-module-encoded-root-ctx p))) ; for `module->namespace`
   (define body-context-simple? (parsed-module-root-ctx-simple? p))
   (define language-info (filter-language-info (syntax-property (parsed-s p) 'module-language)))
   (define bodys (parsed-module-body p))
   
   (define empty-result-for-module->namespace? #f)

   (define mpis (make-module-path-index-table))
   
   (define body-cctx (struct-copy compile-context cctx
                                  [phase 0]
                                  [self self]
                                  [module-self self]
                                  [full-module-name full-module-name]
                                  [lazy-syntax-literals? #t]))
   
   (define cross-phase-persistent? #f)
   
   ;; Callback to track phases that have side effects
   (define side-effects (make-hasheqv))
   (define (check-side-effects! e ; compiled expression
                                expected-results ; number of expected results, or #f if any number is ok
                                phase
                                required-reference?)
     (unless (hash-ref side-effects phase #f)
       (when (any-side-effects? e expected-results #:ready-variable? required-reference?)
         (hash-set! side-effects phase #t))))
   
   (when (and need-compiled-submodule-rename?
              modules-being-compiled)
     ;; Re-register submodules, since they're so far registered under
     ;; the expand-time module path.
     (unless (null? post-submodules)
       (error "internal error: have post submodules, but not already compiled"))
     (register-compiled-submodules modules-being-compiled pre-submodules self))
   
   ;; Compile the sequence of body forms:
   (define-values (body-linklets
                   min-phase
                   max-phase
                   phase-to-link-module-uses
                   phase-to-link-module-uses-expr
                   phase-to-link-extra-inspectorsss
                   syntax-literals
                   root-ctx-pos)
     (compile-forms bodys body-cctx mpis
                    #:body-imports `([,get-syntax-literal!-id]
                                     [,set-transformer!-id])
                    #:body-import-instances (list empty-syntax-literals-instance
                                                  empty-module-body-instance)
                    #:body-suffix-forms '((void)) ; otherwise, compiler always preserves last form
                    #:force-phases '(0) ; minor hack for more consistent compilation
                    #:encoded-root-expand-ctx-box encoded-root-expand-ctx-box
                    #:root-ctx-only-if-syntax? body-context-simple?
                    #:compiled-expression-callback check-side-effects!
                    #:other-form-callback (lambda (body cctx)
                                            (cond
                                              [(parsed-#%declare? body)
                                               (define-match m (parsed-s body) '(_ kw ...))
                                               (for ([kw (in-list (m 'kw))])
                                                 (when (eq? (syntax-e kw) '#:cross-phase-persistent)
                                                   (set! cross-phase-persistent? #t))
                                                 (when (eq? (syntax-e kw) '#:empty-namespace)
                                                   (set! empty-result-for-module->namespace? #t)
                                                   (set-box! encoded-root-expand-ctx-box #f)))
                                               #f]
                                              [else #f]))
                    #:get-module-linklet-info (lambda (mod-name phase)
                                                (define ht (and modules-being-compiled
                                                                (hash-ref modules-being-compiled mod-name #f)))
                                                (and ht (hash-ref ht phase #f)))
                    #:serializable? serializable?
                    #:module-prompt? #t
                    #:to-correlated-linklet? to-correlated-linklet?))
   
   (when modules-being-compiled
     ;; Record this module's linklets for cross-module inlining among (sub)modules
     ;; that are compiled together
     (hash-set! modules-being-compiled
                (module-path-index-resolve self) 
                (for/hasheq ([(phase linklet) (in-hash body-linklets)])
                  (values phase
                          (module-linklet-info linklet
                                               (hash-ref phase-to-link-module-uses phase #f)
                                               self
                                               #f ; inspector is the same as other modules
                                               #f ; no extra inspector, so far
                                               (and phase-to-link-extra-inspectorsss
                                                    (hash-ref phase-to-link-extra-inspectorsss phase #f)))))))
   
   ;; Assemble the declaration linking unit, which includes linking
   ;; information for each phase, is instanted once for a module
   ;; declaration, and is shared among instances
   (define declaration-linklet
     (and serializable?
          ((lambda (s)
             (if to-correlated-linklet?
                 (make-correlated-linklet s 'decl)
                 (performance-region
                  ['compile 'module 'linklet]
                  (compile-linklet s 'decl))))
           (generate-module-declaration-linklet mpis self requires provides
                                                phase-to-link-module-uses-expr))))
   
   ;; Assemble a linklet that shifts syntax objects on demand.
   ;; Include an encoding of the root expand context, if any, so that
   ;; `module->namespace` can have the same scopes as literal syntax
   ;; objects in the module.
   (define syntax-literals-linklet
     (and (not (syntax-literals-empty? syntax-literals))
          ((lambda (s)
             (if to-correlated-linklet?
                 (make-correlated-linklet s 'syntax-literals)
                 (performance-region
                  ['compile 'module 'linklet]
                  (define-values (linklet new-keys)
                    (compile-linklet s 'syntax-literals
                                     (vector deserialize-instance
                                             empty-top-syntax-literal-instance
                                             empty-syntax-literals-data-instance
                                             empty-instance-instance)
                                     (lambda (inst) (values inst #f))
                                     (if serializable? '(serializable) '())))
                  linklet)))
           `(linklet
             ;; imports
             (,deserialize-imports
              [,mpi-vector-id]
              [,deserialized-syntax-vector-id
               ,@(if serializable?
                     `(,deserialize-syntax-id)
                     '())]
              ,instance-imports)
             ;; exports
             (,get-syntax-literal!-id
              get-encoded-root-expand-ctx)
             ;; body
             ,@(generate-lazy-syntax-literals! syntax-literals mpis self
                                               #:skip-deserialize? (not serializable?))
             (define-values (get-encoded-root-expand-ctx)
               ,(cond
                 [root-ctx-pos
                  `(lambda ()
                    ,(generate-lazy-syntax-literal-lookup root-ctx-pos))]
                 [empty-result-for-module->namespace?
                  ;; We also attach this information directly to the bundle,
                  ;; in case this linklet is not included (due to an empty
                  ;; set of syntax literals)
                  `'empty]
                 [else
                  `'#f]))))))
   
   ;; Assemble a linklet that deserializes unshifted syntax objects on
   ;; demand. An instance of this linklet is shared for all
   ;; instantiations of the module, like the data linklet. It's
   ;; separate from the data linklet so that the data linklet can be
   ;; instantiated for information that just depends on module path
   ;; indexes, such as required modules.
   (define syntax-literals-data-linklet
     (and serializable?
          (not (syntax-literals-empty? syntax-literals))
          ((lambda (s) (if to-correlated-linklet?
                           (make-correlated-linklet s 'syntax-literals-data)
                           (performance-region
                            ['compile 'module 'linklet]
                            (compile-linklet s 'syntax-literals-data #f #f '(serializable)))))
           `(linklet
             ;; imports
             (,deserialize-imports
              [,mpi-vector-id])
             ;; exports
             (,deserialized-syntax-vector-id
              ,deserialize-syntax-id)
             ;; body
             (define-values (,deserialized-syntax-vector-id)
               (make-vector ,(syntax-literals-count syntax-literals) #f))
             ,@(performance-region
                ['compile 'module 'serialize]
                (generate-lazy-syntax-literals-data! syntax-literals mpis))))))

   ;; The data linklet houses deserialized data for use by the
   ;; declaration and module-body linklets. Its instance is shared
   ;; across module instances.
   (define data-linklet
     (and serializable?
          ((lambda (s) (if to-correlated-linklet?
                           (make-correlated-linklet s 'data)
                           (performance-region
                            ['compile 'module 'linklet]
                            (compile-linklet s 'data))))
           (generate-module-data-linklet mpis))))

   ;; Combine linklets with other metadata as the bundle:
   (define bundle
     (let* ([bundle (hash-set body-linklets 'name full-module-name)]
            [bundle (hash-set bundle 'decl (or declaration-linklet 
                                               ;; Need a 'decl mapping to indicate
                                               ;; that bundle is a module:
                                               'in-memory))]
            [bundle (if data-linklet
                        (hash-set bundle 'data data-linklet)
                        bundle)]
            [bundle (if syntax-literals-linklet
                        (hash-set bundle 'stx syntax-literals-linklet)
                        bundle)]
            [bundle (if syntax-literals-data-linklet
                        (hash-set bundle 'stx-data syntax-literals-data-linklet)
                        bundle)]
            [bundle (if (null? pre-submodules)
                        bundle
                        (hash-set bundle 'pre (map car pre-submodules)))]
            [bundle (if (null? post-submodules)
                        bundle
                        (hash-set bundle 'post (map car post-submodules)))]
            [bundle (if cross-phase-persistent?
                        (hash-set bundle 'cross-phase-persistent? #t)
                        bundle)]
            [bundle (if language-info
                        (hash-set bundle 'language-info language-info)
                        bundle)]
            [bundle (if (zero? min-phase)
                        bundle
                        (hash-set bundle 'min-phase min-phase))]
            [bundle (if (zero? max-phase)
                        bundle
                        (hash-set bundle 'max-phase max-phase))]
            [bundle (if (hash-count side-effects)
                        (hash-set bundle 'side-effects (sort (hash-keys side-effects) <))
                        bundle)]
            [bundle (if empty-result-for-module->namespace?
                        (hash-set bundle 'module->namespace 'empty)
                        bundle)])
       (hash->linklet-bundle bundle)))

   ;; Combine with submodules in a linklet directory
   (define ld
     (cond
      [(and (null? pre-submodules)
            (null? post-submodules)
            (not force-linklet-directory?))
       ;; Just use the bundle representation directly:
       bundle]
      [else
       (define ht
         (for/fold ([ht (hasheq #f bundle)]) ([sm (in-list (append pre-submodules post-submodules))])
           (hash-set ht
                     (car sm)
                     (compiled-in-memory-linklet-directory
                      (cdr sm)))))
       (hash->linklet-directory ht)]))

  ;; Save mpis and syntax for direct evaluation, instead of unmarshaling:
  (compiled-in-memory ld
                      self
                      requires
                      provides
                      phase-to-link-module-uses
                      (current-code-inspector)
                      phase-to-link-extra-inspectorsss
                      (mpis-as-vector mpis)
                      (syntax-literals-as-vector syntax-literals)
                      (map cdr pre-submodules)
                      (map cdr post-submodules)
                      #f     ; no namespace scopes
                      #f)))  ; not purely functional, since it declares a module

;; ----------------------------------------

;; When a submodule is compiled while expanding a module, then it has a base
;; module name that is an uninterned symbol. 
(define (update-submodule-names cim name full-module-name)
  (change-module-name cim name (if (symbol? full-module-name)
                                   (list full-module-name)
                                   (reverse (cdr (reverse full-module-name))))))

(define (register-compiled-submodules modules-being-compiled pre-submodules self)
  (for ([s (in-list pre-submodules)])
    (define name (car s))
    (define cim (cdr s))
    (define phase-to-link-module-uses (compiled-in-memory-phase-to-link-module-uses cim))
    (define ld (compiled-in-memory-linklet-directory cim))
    (define sm-self (module-path-index-join `(submod "." ,name) self))
    (define phase-to-extra-inspectorsss (compiled-in-memory-phase-to-link-extra-inspectorsss cim))
    (hash-set! modules-being-compiled
               (module-path-index-resolve sm-self)
               (for/hasheq ([(phase linklet) (in-hash (linklet-bundle->hash
                                                       (if (linklet-directory? ld)
                                                           (hash-ref (linklet-directory->hash ld) #f)
                                                           ld)))]
                            #:when (number? phase))
                 (values phase
                         (module-linklet-info linklet
                                              (hash-ref phase-to-link-module-uses phase #f)
                                              (compiled-in-memory-original-self cim)
                                              #f ; inspector is the same as the module being compiled
                                              (compiled-in-memory-compile-time-inspector cim)
                                              (and phase-to-extra-inspectorsss
                                                   (hash-ref phase-to-extra-inspectorsss phase #f))))))))

;; ----------------------------------------

(define (filter-language-info li)
  (and (vector? li)
       (= 3 (vector-length li))
       (module-path? (vector-ref li 0))
       (symbol? (vector-ref li 1))
       li))
